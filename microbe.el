;;; microbe.el --- A local SQLite-backed Micro.blog client -*- lexical-binding: t; -*-

;; Author: Milos Miljkovic
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: comm, hypermedia
;; URL: https://github.com/miljko/microbe.el

;;; Commentary:
;; An offline-first, high-performance client for Micro.blog.
;; Uses Emacs 29's built-in SQLite to store posts locally for instant
;; loading and full-text search. Bypasses Emacs's internal network 
;; quirks by using `curl` for API communication, UTF-8 
;; handling, and media uploads.

;;; Code:

(require 'sqlite)
(require 'json)
(require 'auth-source)

;; =====================================================================
;; Variables & Configuration
;; =====================================================================

(defvar microblog-db-file (expand-file-name "microblog.sqlite" user-emacs-directory)
  "Path to the local Micro.blog SQLite database.")

(defvar-local microblog-edit-post-id nil 
  "Tracks the ID of the post currently being edited. Nil if composing a new post.")

(defvar-local microblog-edit-post-url nil 
  "Tracks the live URL of the post being edited. Nil if composing a new post.")

;; =====================================================================
;; Database Initialization
;; =====================================================================

(defun microblog-init-db ()
  "Initialize the SQLite database and create the posts table."
  (interactive)
  (let ((db (sqlite-open microblog-db-file)))
    (sqlite-execute db "
      CREATE TABLE IF NOT EXISTS posts (
        id TEXT PRIMARY KEY,
        date_published TEXT,
        content_html TEXT,
        url TEXT,
        title TEXT,
        categories TEXT
      );")
    (message "Micro.blog database initialized.")))

(defun microblog-reset-db ()
  "Delete all posts from the local database. Useful for a fresh sync."
  (interactive)
  (let ((db (sqlite-open microblog-db-file)))
    (when (y-or-n-p "Are you sure you want to delete ALL local posts? ")
      (sqlite-execute db "DELETE FROM posts")
      (message "Database cleared."))))

;; =====================================================================
;; Authentication
;; =====================================================================

(defun microblog-get-token ()
  "Retrieve the Micro.blog API token from auth-sources."
  (let ((match (car (auth-source-search :host "micro.blog" :user "apikey"))))
    (if match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret) (funcall secret) secret))
      (error "Micro.blog token not found in auth-sources. See README."))))

;; =====================================================================
;; Sync Engine (Read)
;; =====================================================================

(defun microblog-sync-posts ()
  "Download all posts into the local SQLite DB. Uses curl for clean UTF-8."
  (interactive)
  (let* ((token (microblog-get-token))
         (db (sqlite-open microblog-db-file))
         (limit 50)
         (offset 0)
         (more-posts t)
         (total-count 0))
    
    (message "Starting full sync via curl...")
    (sqlite-execute db "BEGIN TRANSACTION")
    
    (while more-posts
      (let* ((url (format "https://micro.blog/micropub?q=source&limit=%d&offset=%d" limit offset))
             (curl-buf (generate-new-buffer "*curl-json*")))
        
        (message "Fetching posts %d to %d..." offset (+ offset limit))
        (call-process "curl" nil curl-buf nil "-s" "-H" (concat "Authorization: Bearer " token) url)
        
        (with-current-buffer curl-buf
          (goto-char (point-min))
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'symbol)
                 (data (json-read))
                 (items (alist-get 'items data)))
            
            (if (or (null items) (zerop (length items)))
                (setq more-posts nil)
              
              (dolist (item items)
                (let* ((props (alist-get 'properties item))
                       (content (car (alist-get 'content props)))
                       (date (car (alist-get 'published props)))
                       (url (car (alist-get 'url props)))
                       (id (or (car (alist-get 'uid props)) url))
                       (title (car (alist-get 'name props)))
                       (cats-list (alist-get 'category props))
                       (categories (if cats-list (mapconcat #'identity cats-list ", ") "")))
                  
                  (when (listp content) (setq content (alist-get 'html content)))
                  
                  (sqlite-execute db "INSERT OR REPLACE INTO posts (id, date_published, content_html, url, title, categories) VALUES (?, ?, ?, ?, ?, ?)"
                                  (list id date content url title categories))
                  (setq total-count (1+ total-count))))
              
              (setq offset (+ offset limit))))
          (kill-buffer curl-buf))))
    
    (sqlite-execute db "COMMIT")
    (message "Sync complete! Updated %d posts." total-count)))

;; =====================================================================
;; Publishing Engine (Create & Update)
;; =====================================================================

(defun microblog-submit-update ()
  "Parse the buffer and send to Micro.blog. Handles both New and Edited posts."
  (interactive)
  (let ((title "") (categories "") (body "")
        (edit-buf (current-buffer))
        (post-id microblog-edit-post-id)
        (post-url microblog-edit-post-url)
        (is-new (null microblog-edit-post-url))) 
    
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^Title: \\(.*\\)$" nil t) 
        (setq title (string-trim (match-string 1))))
      (goto-char (point-min))
      (when (re-search-forward "^Categories: \\(.*\\)$" nil t) 
        (setq categories (string-trim (match-string 1))))
      (goto-char (point-min))
      (if (re-search-forward "^--$" nil t)
          (progn 
            (forward-line 1) 
            (setq body (buffer-substring-no-properties (point) (point-max))))
        (error "Could not find the '--' separator. Please restore it before saving."))
      
      (let* ((cat-list (if (string-empty-p categories) '() (split-string categories "," t "[ \t]+")))
             (temp-file (make-temp-file "microblog-payload"))
             (payload nil))
        
        ;; Construct JSON payload depending on Create vs Update
        (if is-new
            (let ((props `((content . ,(vector body)))))
              (unless (string-empty-p title) (push `(name . ,(vector title)) props))
              (when cat-list (push `(category . ,(vconcat cat-list)) props))
              (setq payload `((type . ["h-entry"]) (properties . ,props))))
          (let ((replace-alist `((content . ,(vector body)))))
            (unless (string-empty-p title) (push `(name . ,(vector title)) replace-alist))
            (when cat-list (push `(category . ,(vconcat cat-list)) replace-alist))
            (setq payload `((action . "update") (url . ,post-url) (replace . ,replace-alist)))))
        
        (with-temp-file temp-file (insert (json-encode payload)))
        (message (if is-new "Publishing new post..." "Sending update..."))
        
        ;; Send via curl for robust networking
        (let* ((curl-output-buffer (generate-new-buffer "*curl-output*"))
               (exit-code (call-process "curl" nil curl-output-buffer nil
                                        "-s" "-w" "%{http_code}" "-X" "POST"
                                        "-H" (concat "Authorization: Bearer " (microblog-get-token))
                                        "-H" "Content-Type: application/json"
                                        "-d" (concat "@" temp-file)
                                        "https://micro.blog/micropub")))
          (delete-file temp-file)
          
          ;; Parse result
          (with-current-buffer curl-output-buffer
            (goto-char (point-max))
            (let* ((http-status (buffer-substring (- (point-max) 3) (point-max)))
                   (response-body (buffer-substring (point-min) (- (point-max) 3))))
              
              (if (member http-status '("200" "201" "202" "204"))
                  (progn
                    (kill-buffer edit-buf)
                    (if is-new
                        (progn
                          (message "Published! Fetching latest posts...")
                          (microblog-sync-posts))
                      ;; Optimistic local UI update for edits
                      (let ((db (sqlite-open microblog-db-file)))
                        (sqlite-execute db "UPDATE posts SET title = ?, content_html = ?, categories = ? WHERE id = ?"
                                        (list title body categories post-id)))
                      (message "Success! Post updated."))
                    
                    ;; Refresh UI
                    (let ((list-buf (get-buffer "*Micro.blog Headers*")))
                      (when list-buf
                        (with-current-buffer list-buf
                          (setq tabulated-list-entries (microblog-get-entries))
                          (tabulated-list-print t)))))
                (error "Operation failed! HTTP %s. Server said: %s" http-status response-body))))
          (kill-buffer curl-output-buffer))))))

;; =====================================================================
;; Media Uploads
;; =====================================================================

(defun microblog-attach-image ()
  "Upload an image to Micro.blog and insert its Markdown link."
  (interactive)
  (let* ((file (read-file-name "Select image to upload: "))
         (alt-text (read-string "Alt text: "))
         (token (microblog-get-token))
         (edit-buf (current-buffer))
         (curl-buf (generate-new-buffer "*microblog-upload*"))
         (image-url nil))
    (message "Uploading %s to Micro.blog..." (file-name-nondirectory file))
    
    (call-process "curl" nil curl-buf nil "-s" "-i"
                  "-H" (concat "Authorization: Bearer " token)
                  "-F" (concat "file=@" (expand-file-name file))
                  "https://micro.blog/micropub/media")
    
    (with-current-buffer curl-buf
      (goto-char (point-min))
      (if (re-search-forward "^HTTP/[0-9.]+ 20[12]" nil t)
          (progn
            (goto-char (point-min))
            (if (re-search-forward "^[Ll]ocation:[ \t]*\\(.*\\)\r?$" nil t)
                (setq image-url (string-trim (match-string 1)))
              (error "Upload succeeded but the server didn't return a Location URL.")))
        (error "Upload failed. Server response: %s" (buffer-substring (point-min) (point-max)))))
    (kill-buffer curl-buf)
    
    (when image-url
      (with-current-buffer edit-buf
        (insert (format "![%s](%s)" alt-text image-url))
        (message "Image uploaded and linked!")))))

;; =====================================================================
;; Tag/Category Management
;; =====================================================================

(defun microblog-get-known-categories ()
  "Return a list of all unique categories used in the local database."
  (let* ((db (sqlite-open microblog-db-file))
         (rows (sqlite-select db "SELECT categories FROM posts WHERE categories IS NOT NULL AND categories != ''"))
         (all-cats '()))
    (dolist (row rows)
      (let ((cats (split-string (car row) "," t "[ \t]+")))
        (setq all-cats (append cats all-cats))))
    (delete-dups all-cats)))

(defun microblog-insert-categories ()
  "Prompt for categories and precisely update the header line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^Categories:[ \t]*\\(.*\\)$" nil t)
        (let* ((line-start (match-beginning 0))
               (line-end (match-end 0))
               (current-cats (string-trim (match-string 1)))
               (known-cats (microblog-get-known-categories))
               (initial-input (if (string-empty-p current-cats) nil (concat current-cats ",")))
               (chosen (completing-read-multiple "Categories: " known-cats nil nil initial-input))
               (clean-chosen (remove "" (mapcar #'string-trim chosen)))
               (new-cats-string (mapconcat #'identity clean-chosen ", ")))
          (delete-region line-start line-end)
          (goto-char line-start)
          (insert (concat "Categories: " new-cats-string)))
      (error "Could not find the 'Categories:' header line!"))))

;; =====================================================================
;; Edit / Compose Mode
;; =====================================================================

(defvar microblog-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'microblog-submit-update)
    (define-key map (kbd "C-c C-k") 'kill-buffer-and-window)
    (define-key map (kbd "C-c C-t") 'microblog-insert-categories)
    (define-key map (kbd "C-c C-a") 'microblog-attach-image)
    map))

(define-minor-mode microblog-edit-mode
  "Minor mode for editing Micro.blog posts."
  :lighter " MicroBlog"
  :keymap microblog-edit-mode-map)

(defun microblog-compose-post ()
  "Open a new buffer to compose a Micro.blog post."
  (interactive)
  (let ((buffer (get-buffer-create "*Compose: New Post*")))
    (with-current-buffer buffer
      (erase-buffer)
      (if (fboundp 'markdown-mode) (markdown-mode) (text-mode))
      (visual-line-mode 1)
      (microblog-edit-mode 1)
      (setq-local microblog-edit-post-id nil)
      (setq-local microblog-edit-post-url nil)
      (insert "Title: \nCategories: \n--\n\n")
      (goto-char (point-max))
      (switch-to-buffer-other-window buffer)
      (message "Compose mode. Type your post and press C-c C-c to publish."))))

(defun microblog-edit-post ()
  "Edit the post at point in a new buffer."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (db (sqlite-open microblog-db-file))
         (row (car (sqlite-select db "SELECT title, categories, content_html, url FROM posts WHERE id = ?" (list id))))
         (title (nth 0 row))
         (cats (nth 1 row))
         (content-html (nth 2 row))
         (url (nth 3 row))
         (buffer (get-buffer-create (format "*Edit: %s*" (or title "Micro-post")))))
    (with-current-buffer buffer
      (erase-buffer)
      (if (fboundp 'markdown-mode) (markdown-mode) (text-mode))
      (visual-line-mode 1)
      (microblog-edit-mode 1)
      (setq-local microblog-edit-post-id id)
      (setq-local microblog-edit-post-url url)
      (insert (format "Title: %s\n" (or title "")))
      (insert (format "Categories: %s\n" (or cats "")))
      (insert "--\n")
      (insert content-html)
      ;; Convert HTML to Markdown if Pandoc is installed
      (if (executable-find "pandoc")
          (call-process-region (point-min) (point-max) "pandoc" t t nil "-f" "html" "-t" "markdown"))
      (goto-char (point-max))
      (switch-to-buffer-other-window buffer)
      (message "Edit mode. Press C-c C-c to save."))))

(defun microblog--gemini-payload (text)
  "Construct the strict JSON payload instructing Gemini to only copy-edit."
  (let* ((system-instruction "You are a strict, technical copy-editor. Your ONLY job is to fix spelling mistakes, typographical errors, and invalid Markdown or Hugo shortcode syntax. You MUST NOT alter the author's voice, style, phrasing, vocabulary, or structural choices. Output ONLY the corrected text. Do not add conversational filler, introductions, or explanations.")
         (payload `((contents . [((parts . [((text . ,text))]))])
                    (systemInstruction . ((parts . [((text . ,system-instruction))])))
                    (generationConfig . ((temperature . 0.1))))))
    (json-encode payload)))

(defun microblog-gemini-copy-edit ()
  "Send the current buffer (or region) to Gemini and ediff the result."
  (interactive)
  (if (string-empty-p microblog-gemini-api-key)
      (error "Please set `microblog-gemini-api-key` first!"))
  
  (let* ((use-region (use-region-p))
         (orig-text (if use-region
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (buffer-substring-no-properties (point-min) (point-max))))
         (orig-buf (current-buffer))
         (temp-file (make-temp-file "microbe-gemini"))
         (curl-buf (generate-new-buffer "*microbe-gemini-network*"))
         (output-buf-name "*Microbe: Gemini Edit*")
         (api-url (format "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent?key=%s" microblog-gemini-api-key)))
    
    (with-temp-file temp-file
      (insert (microblog--gemini-payload orig-text)))
    
    (message "Asking Gemini 2.5 Flash to review your draft...")
    
    (let ((proc (start-process "microbe-gemini-proc" curl-buf "curl" "-s" "-X" "POST"
                               "-H" "Content-Type: application/json"
                               "-d" (concat "@" temp-file)
                               api-url)))
      
      (set-process-sentinel proc
        (lambda (process event)
          (when (memq (process-status process) '(exit signal))
            (with-current-buffer (process-buffer process)
              (goto-char (point-min))
              (let* ((json-object-type 'alist)
                     (json-array-type 'list)
                     (response (condition-case nil (json-read) (error nil)))
                     (candidates (alist-get 'candidates response))
                     (content (when candidates (alist-get 'content (car candidates))))
                     (parts (when content (alist-get 'parts content)))
                     (edited-text (when parts (alist-get 'text (car parts))))
                     (error-msg (alist-get 'error response)))
                
                (if edited-text
                    (progn
                      (with-current-buffer (get-buffer-create output-buf-name)
                        (erase-buffer)
                        (insert edited-text)
                        (when (fboundp 'markdown-mode) (markdown-mode)))
                      
                      (if use-region
                          (let ((region-buf (get-buffer-create "*Microbe: Original Region*")))
                            (with-current-buffer region-buf
                              (erase-buffer)
                              (insert orig-text)
                              (when (fboundp 'markdown-mode) (markdown-mode)))
                            (ediff-buffers region-buf output-buf-name))
                        (ediff-buffers orig-buf output-buf-name))
                      
                      (message "Review complete! Use 'n' and 'p' in the Ediff panel to navigate."))
                  
                  (message "Gemini API failed: %s"
                           (if error-msg (alist-get 'message error-msg) "Unknown error")))))
            
            (ignore-errors (delete-file temp-file))
            (kill-buffer (process-buffer process))))))))

(defun microblog-ediff-accept ()
  "Accept the Gemini edit (copy Buffer B to Buffer A) and jump to the next."
  (interactive)
  (ediff-copy-B-to-A nil)
  (ediff-next-difference))

(defun microblog-ediff-decline ()
  "Decline the Gemini edit (keep your original text) and jump to the next."
  (interactive)
  (ediff-next-difference))

;; Create the custom transparent keymap
(defvar microblog-ediff-keys-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'microblog-ediff-accept)
    (define-key map (kbd "d") 'microblog-ediff-decline)
    map)
  "Keymap for Microbe Ediff controls.")

;; Wrap the keymap in a Minor Mode
(define-minor-mode microblog-ediff-keys-mode
  "Minor mode to overlay Accept/Decline keys in Ediff."
  :lighter " Review"
  :keymap microblog-ediff-keys-map)

(defun microblog-setup-ediff-keys ()
  "Activate custom keys if this is a Gemini review session."
  (when (and (boundp 'ediff-buffer-B)
             (buffer-live-p ediff-buffer-B)
             (string= (buffer-name ediff-buffer-B) "*Microbe: Gemini Edit*"))
    ;; Turn on our custom keys only in the Ediff Control Panel
    (microblog-ediff-keys-mode 1)))

;; Hook into the session startup, which runs every time!
(add-hook 'ediff-startup-hook 'microblog-setup-ediff-keys)


(define-key microblog-edit-mode-map (kbd "C-c C-p") 'microblog-gemini-copy-edit)

;; =====================================================================
;; List View Mode
;; =====================================================================

(defun microblog-strip-html (html)
  "Remove HTML tags and newlines for the summary view."
  (if (null html) ""
    (with-temp-buffer
      (insert html)
      (goto-char (point-min))
      (while (re-search-forward "<[^>]+>" nil t) (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "[\n\r]+" nil t) (replace-match " "))
      (string-trim (buffer-string)))))

(defun microblog-get-entries ()
  "Query DB and format rows for tabulated list."
  (let* ((db (sqlite-open microblog-db-file))
         (rows (sqlite-select db "SELECT id, date_published, content_html, url, title, categories FROM posts ORDER BY date_published DESC")))
    (mapcar (lambda (row)
              (let* ((id (nth 0 row))
                     (date (nth 1 row))
                     (raw-content (nth 2 row))
                     (url (nth 3 row))
                     (title (nth 4 row))
                     (categories (nth 5 row))
                     (date-display (if (> (length date) 10) (substring date 0 10) date))
                     (summary (microblog-strip-html raw-content)))
                (when (or (null title) (string-empty-p title)) (setq title "-"))
                (when (> (length summary) 60) (setq summary (concat (substring summary 0 57) "...")))
                (list id (vector date-display title (or categories "") summary))))
            rows)))

(defun microblog-copy-url ()
  "Copy the live URL of the current post to the clipboard."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (db (sqlite-open microblog-db-file))
         (row (car (sqlite-select db "SELECT url FROM posts WHERE id = ?" (list id))))
         (url (nth 0 row)))
    (if (and url (not (string-empty-p url)))
        (progn (kill-new url) (message "Copied: %s" url))
      (error "No URL found for this post."))))

(defun microblog-open-in-browser ()
  "Open the live URL of the current post in the system's default web browser."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (db (sqlite-open microblog-db-file))
         (row (car (sqlite-select db "SELECT url FROM posts WHERE id = ?" (list id))))
         (url (nth 0 row)))
    (if (and url (not (string-empty-p url)))
        (progn 
          (browse-url url) 
          (message "Opening in browser: %s" url))
      (error "No URL found for this post."))))

(defun microblog-search (term)
  "Search the full text of titles, categories, and bodies for TERM."
  (interactive "sSearch Micro.blog: ")
  (let* ((db (sqlite-open microblog-db-file))
         (sql-term (concat "%" term "%"))
         (rows (sqlite-select db 
                "SELECT id, date_published, content_html, url, title, categories 
                 FROM posts 
                 WHERE title LIKE ? OR content_html LIKE ? OR categories LIKE ? 
                 ORDER BY date_published DESC"
                (list sql-term sql-term sql-term)))
         (entries (mapcar (lambda (row)
                            (let* ((id (nth 0 row))
                                   (date (nth 1 row))
                                   (raw-content (nth 2 row))
                                   (url (nth 3 row))
                                   (title (nth 4 row))
                                   (categories (nth 5 row))
                                   (date-display (if (> (length date) 10) (substring date 0 10) date))
                                   (summary (microblog-strip-html raw-content)))
                              (when (or (null title) (string-empty-p title)) (setq title "-"))
                              (when (> (length summary) 60) (setq summary (concat (substring summary 0 57) "...")))
                              (list id (vector date-display title (or categories "") summary))))
                          rows)))
    (with-current-buffer (get-buffer-create "*Micro.blog Headers*")
      (setq tabulated-list-entries entries)
      (tabulated-list-print t)
      (message "Found %d results for '%s'. Press 'g' to clear." (length entries) term))))

(define-derived-mode microblog-headers-mode tabulated-list-mode "MicroHeaders"
  "Major mode for browsing Micro.blog posts."
  (setq tabulated-list-format [("Date" 12 t) ("Title" 25 t) ("Cats" 15 t) ("Text Snippet" 60 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Date" . t))
  (tabulated-list-init-header))

(define-key microblog-headers-mode-map (kbd "e") 'microblog-edit-post)
(define-key microblog-headers-mode-map (kbd "c") 'microblog-compose-post)
(define-key microblog-headers-mode-map (kbd "w") 'microblog-copy-url)
(define-key microblog-headers-mode-map (kbd "s") 'microblog-search)
(define-key microblog-headers-mode-map (kbd "g") 'microblog-list)
(define-key microblog-headers-mode-map (kbd "o") 'microblog-open-in-browser)

;; =====================================================================
;; Main Entry Point
;; =====================================================================

;;;###autoload
(defun microblog-list ()
  "Open the Micro.blog headers view."
  (interactive)
  (microblog-init-db) ;; Ensure DB exists
  (let ((buf (get-buffer-create "*Micro.blog Headers*")))
    (with-current-buffer buf
      (microblog-headers-mode)
      (setq tabulated-list-entries (microblog-get-entries))
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(provide 'microbe)
;;; microbe.el ends here
