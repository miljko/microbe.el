;;; inkling.el --- An Inkwell (Micro.blog) RSS Client -*- lexical-binding: t; -*-

(require 'sqlite)
(require 'json)
(require 'shr)
(require 'microbe) ;; Loads the file microbe.el, which contains microblog-* functions

;; =====================================================================
;; Configuration & Variables
;; =====================================================================

(defvar inkling-db-file (expand-file-name "inkling.sqlite" user-emacs-directory)
  "Path to the local Inkling database.")

(defvar-local inkling-current-id nil "Tracks the API ID of the post being read.")
(defvar-local inkling-current-url nil "Tracks the URL of the post being read.")
(defvar-local inkling-current-title nil "Tracks the title of the post being read.")

(defvar-local inkling-show-unread-only nil 
  "Tracks whether the list view is filtered to unread items.")

;; =====================================================================
;; Database
;; =====================================================================

(defun inkling-init-db ()
  "Initialize the SQLite database for Inkwell feeds."
  (interactive)
  (let ((db (sqlite-open inkling-db-file)))
    (sqlite-execute db "
      CREATE TABLE IF NOT EXISTS feeds (
        id TEXT PRIMARY KEY,
        date_published TEXT,
        content TEXT,
        url TEXT,
        title TEXT,
        author TEXT,
        is_read INTEGER DEFAULT 0,
        is_starred INTEGER DEFAULT 0
      );")
    ;; Safely upgrade existing databases to include the new column
    (condition-case nil
        (sqlite-execute db "ALTER TABLE feeds ADD COLUMN is_starred INTEGER DEFAULT 0")
      (error nil))))

;; =====================================================================
;; Sync Engine (Feedbin-style API)
;; =====================================================================

(defun inkling-sync-feeds ()
  "Download entries, unread state, and starred state from Inkwell."
  (interactive)
  (inkling-init-db)
  (let* ((token (microblog-get-token))
         (db (sqlite-open inkling-db-file))
         (entries-buf (generate-new-buffer "*inkling-entries*"))
         (unread-buf (generate-new-buffer "*inkling-unread*"))
         (starred-buf (generate-new-buffer "*inkling-starred*"))
         (total-count 0))
    
    (message "Syncing Inkwell feeds...")
    
    ;; 1. Fetch Data
    (call-process "curl" nil entries-buf nil "-s" "-H" (concat "Authorization: Bearer " token) 
                  "https://micro.blog/feeds/v2/entries.json?mode=extended")
    (call-process "curl" nil unread-buf nil "-s" "-H" (concat "Authorization: Bearer " token) 
                  "https://micro.blog/feeds/v2/unread_entries.json")
    (call-process "curl" nil starred-buf nil "-s" "-H" (concat "Authorization: Bearer " token) 
                  "https://micro.blog/feeds/v2/starred_entries.json")
    
    (sqlite-execute db "BEGIN TRANSACTION")
    
    ;; 2. Parse & Insert Entries
    (with-current-buffer entries-buf
      (goto-char (point-min))
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'symbol)
             (items (condition-case nil (json-read) (error nil))))
        (when items
          (dolist (item items)
            (let* ((id (alist-get 'id item))
                   (url (alist-get 'url item))
                   (title (alist-get 'title item))
                   (content (alist-get 'content item))
                   (date (alist-get 'published item))
                   (author (alist-get 'author item)))
              ;; Insert everything as un-starred and read by default
              (sqlite-execute db "INSERT OR REPLACE INTO feeds (id, date_published, content, url, title, author, is_read, is_starred) VALUES (?, ?, ?, ?, ?, ?, 1, 0)"
                              (list (format "%s" id) date content url title author))
              (setq total-count (1+ total-count)))))))
    
    ;; 3. Parse & Apply Unread State
    (with-current-buffer unread-buf
      (goto-char (point-min))
      (let* ((json-array-type 'list)
             (unread-ids (condition-case nil (json-read) (error nil))))
        (when unread-ids
          (dolist (uid unread-ids)
            (sqlite-execute db "UPDATE feeds SET is_read = 0 WHERE id = ?" (list (format "%s" uid)))))))

    ;; 4. Parse & Apply Starred State
    (with-current-buffer starred-buf
      (goto-char (point-min))
      (let* ((json-array-type 'list)
             (starred-ids (condition-case nil (json-read) (error nil))))
        ;; Reset all stars locally first, so if unstarred elsewhere, it syncs here
        (sqlite-execute db "UPDATE feeds SET is_starred = 0")
        (when starred-ids
          (dolist (uid starred-ids)
            (sqlite-execute db "UPDATE feeds SET is_starred = 1 WHERE id = ?" (list (format "%s" uid)))))))
    
    (sqlite-execute db "COMMIT")
    (kill-buffer entries-buf)
    (kill-buffer unread-buf)
    (kill-buffer starred-buf)
    
    ;; Refresh UI
    (when (get-buffer "*Inkling Headers*")
      (with-current-buffer "*Inkling Headers*"
        (setq tabulated-list-entries (inkling-get-entries))
        (tabulated-list-print t)))
    (message "Inkwell sync complete! Fetched %d items." total-count)))

;; =====================================================================
;; Core Actions
;; =====================================================================

(defun inkling-get-current-post-data ()
  "Return (ID URL TITLE) for the current post."
  (cond
   ((derived-mode-p 'inkling-headers-mode)
    (let* ((id (tabulated-list-get-id))
           (db (sqlite-open inkling-db-file))
           (row (car (sqlite-select db "SELECT url, title FROM feeds WHERE id = ?" (list id)))))
      (list id (nth 0 row) (nth 1 row))))
   (inkling-read-mode 
    (list inkling-current-id inkling-current-url inkling-current-title))
   (t (error "Not in an Inkling buffer!"))))

(defun inkling-copy-url ()
  "Copy the live URL of the current feed item."
  (interactive)
  (let* ((data (inkling-get-current-post-data))
         (url (nth 1 data)))
    (kill-new url)
    (message "Copied: %s" url)))

(defun inkling-toggle-bookmark ()
  "Toggle the starred/bookmarked status of the current post."
  (interactive)
  (let* ((data (inkling-get-current-post-data))
         (id (nth 0 data))
         (db (sqlite-open inkling-db-file))
         (row (car (sqlite-select db "SELECT is_starred FROM feeds WHERE id = ?" (list id))))
         (currently-starred (= (nth 0 row) 1))
         (new-status (if currently-starred 0 1))
         (token (microblog-get-token))
         (temp-file (make-temp-file "inkling-star")))
    
    ;; 1. Update the local database
    (sqlite-execute db "UPDATE feeds SET is_starred = ? WHERE id = ?" (list new-status id))
    
    ;; 2. Update the UI instantly
    (when (get-buffer "*Inkling Headers*")
      (with-current-buffer "*Inkling Headers*"
        (let ((entry (assoc id tabulated-list-entries)))
          (when entry
            ;; Index 1 is the Star column in our format array
            (aset (cadr entry) 1 (if (= new-status 1) "★" " "))))
        (tabulated-list-print t)))
    
    ;; 3. Send the command to Inkwell
    (with-temp-file temp-file
      (insert (json-encode `((starred_entries . [,(string-to-number id)])))))
    
    (let* ((method (if currently-starred "DELETE" "POST"))
           (curl-buf (generate-new-buffer "*inkling-bookmark*")))
      (call-process "curl" nil curl-buf nil "-s" "-w" "%{http_code}" "-X" method
                    "-H" (concat "Authorization: Bearer " token)
                    "-H" "Content-Type: application/json"
                    "-d" (concat "@" temp-file)
                    "https://micro.blog/feeds/v2/starred_entries.json")
      (delete-file temp-file)
      
      (with-current-buffer curl-buf
        (goto-char (point-max))
        (let ((http-status (buffer-substring (- (point-max) 3) (point-max))))
          (if (member http-status '("200" "201" "202" "204"))
              (message (if currently-starred "Removed bookmark." "Added bookmark!"))
            (error "Bookmark sync failed. HTTP %s" http-status))))
      (kill-buffer curl-buf))))

(defun inkling-toggle-read-status ()
  "Toggle the read/unread status of the current post."
  (interactive)
  (let* ((data (inkling-get-current-post-data))
         (id (nth 0 data))
         (db (sqlite-open inkling-db-file))
         (row (car (sqlite-select db "SELECT is_read FROM feeds WHERE id = ?" (list id))))
         (currently-read (= (nth 0 row) 1))
         (new-status (if currently-read 0 1))
         (token (microblog-get-token))
         (temp-file (make-temp-file "inkling-toggle")))
    
    ;; Update the local database
    (sqlite-execute db "UPDATE feeds SET is_read = ? WHERE id = ?" (list new-status id))
    
    ;; Update the UI instantly
    (when (get-buffer "*Inkling Headers*")
      (with-current-buffer "*Inkling Headers*"
        (let ((entry (assoc id tabulated-list-entries)))
          (when entry
            ;; Index 0 is the Status column. "•" if unread (0), space if read (1)
            (aset (cadr entry) 0 (if (= new-status 0) "•" " "))))
        (tabulated-list-print t)))
    
    ;; Tell the Inkwell server
    (with-temp-file temp-file
      (insert (json-encode `((unread_entries . [,(string-to-number id)])))))
    
    (let* ((endpoint (if currently-read
                         "https://micro.blog/feeds/v2/unread_entries.json"         ;; Mark Unread
                       "https://micro.blog/feeds/v2/unread_entries/delete.json"))  ;; Mark Read
           (curl-buf (generate-new-buffer "*inkling-network*")))
      
      (call-process "curl" nil curl-buf nil "-s" "-w" "%{http_code}" "-X" "POST"
                    "-H" (concat "Authorization: Bearer " token)
                    "-H" "Content-Type: application/json"
                    "-d" (concat "@" temp-file)
                    endpoint)
      (delete-file temp-file)
      
      (with-current-buffer curl-buf
        (goto-char (point-max))
        (let ((http-status (buffer-substring (- (point-max) 3) (point-max))))
          (if (member http-status '("200" "201" "202"))
              (message (if currently-read "Marked as unread." "Marked as read."))
            (error "Sync failed. HTTP %s" http-status))))
      (kill-buffer curl-buf))))

(defun inkling-quote-and-compose ()
  "Start a post quoting the active region without clobbering the list view."
  (interactive)
  (let* ((data (inkling-get-current-post-data))
         (url (nth 1 data))
         (title (or (nth 2 data) "Link"))
         (quote-text (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       ""))
         (compose-buf-name "*Compose: New Post*"))
    
    ;; 1. Deactivate highlight so it doesn't bleed into the new buffer
    (deactivate-mark)
    
    ;; 2. Run compose function but BLOCK it from changing windows
    (save-window-excursion
      (microblog-compose-post))
    
    ;; 3. Safely target the correct buffer and insert quote
    (with-current-buffer (get-buffer compose-buf-name)
      (goto-char (point-max))
      (if (string-empty-p quote-text)
          (insert (format "[%s](%s)\n\n" title url))
        (setq quote-text (replace-regexp-in-string "\n" "\n> " quote-text))
        (insert (format "> %s\n\n[%s](%s)\n\n" quote-text title url))))
    
    ;; 4. Explicitly create a 3rd window at the bottom and switch
    (split-window-below)
    (other-window 1)
    (switch-to-buffer compose-buf-name)
    (message "Ready to compose!")))

(defun inkling-mark-read (id)
  "Mark a post as read on the server and locally."
  (let* ((token (microblog-get-token))
         (db (sqlite-open inkling-db-file))
         (temp-file (make-temp-file "inkling-read")))
    
    (sqlite-execute db "UPDATE feeds SET is_read = 1 WHERE id = ?" (list id))
    
    (with-temp-file temp-file
      (insert (json-encode `((unread_entries . [,(string-to-number id)])))))
    
    (call-process "curl" nil nil nil "-s" "-X" "POST"
                  "-H" (concat "Authorization: Bearer " token)
                  "-H" "Content-Type: application/json"
                  "-d" (concat "@" temp-file)
                  "https://micro.blog/feeds/v2/unread_entries/delete.json")
    (delete-file temp-file)))

(defun inkling-open-in-browser ()
  "Open the current post's URL in the system's default web browser."
  (interactive)
  (let* ((data (inkling-get-current-post-data))
         (url (nth 1 data)))
    (if (and url (not (string-empty-p url)))
        (progn
          (browse-url url)
          (message "Opening in browser: %s" url))
      (error "No URL found for this post!"))))

;; =====================================================================
;; Navigation
;; =====================================================================

(defun inkling-next-post ()
  "Close the current post and open the next one in the list."
  (interactive)
  (let ((list-win (get-buffer-window "*Inkling Headers*"))
        (old-buf (current-buffer)))
    (if list-win
        (progn
          (select-window list-win)
          (forward-line 1)
          (if (tabulated-list-get-id)
              (progn
                (inkling-read-post)
                (kill-buffer old-buf))
            (forward-line -1)
            (select-window (get-buffer-window old-buf))
            (message "You are at the bottom of the list.")))
      (error "List window not visible!"))))

(defun inkling-prev-post ()
  "Close the current post and open the previous one in the list."
  (interactive)
  (let ((list-win (get-buffer-window "*Inkling Headers*"))
        (old-buf (current-buffer)))
    (if list-win
        (progn
          (select-window list-win)
          (forward-line -1)
          (if (tabulated-list-get-id)
              (progn
                (inkling-read-post)
                (kill-buffer old-buf))
            (forward-line 1)
            (select-window (get-buffer-window old-buf))
            (message "You are at the top of the list.")))
      (error "List window not visible!"))))

;; =====================================================================
;; Read Mode
;; =====================================================================

(defvar inkling-read-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'inkling-next-post)
    (define-key map (kbd "k") 'inkling-prev-post)
    (define-key map (kbd "w") 'inkling-copy-url)
    (define-key map (kbd "c") 'inkling-quote-and-compose)
    (define-key map (kbd "b") 'inkling-toggle-bookmark)
    (define-key map (kbd "r") 'inkling-toggle-read-status)
    (define-key map (kbd "o") 'inkling-open-in-browser)
    (define-key map (kbd "q") 'quit-window)
    map))

(define-minor-mode inkling-read-mode
  "Minor mode for reading Inkwell posts."
  :lighter " Inkling-Read"
  :keymap inkling-read-mode-map)

(defun inkling-read-post ()
  "Open the RSS post under cursor and mark it as read."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (db (sqlite-open inkling-db-file))
         (row (car (sqlite-select db "SELECT title, content, url FROM feeds WHERE id = ?" (list id))))
         (title (nth 0 row))
         (content (nth 1 row))
         (url (nth 2 row))
         (buf (get-buffer-create (format "*Inkling: %s*" (or title "Post")))))
    
    (inkling-mark-read id)
    
    ;; In-place UI update for the read marker
    (let ((entry (assoc id tabulated-list-entries)))
      (when entry
        (aset (cadr entry) 0 " ")))
    (tabulated-list-print t)

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "<h1>%s</h1><br>" (or title "Untitled")))
        (insert content)
        (shr-render-region (point-min) (point-max))
        (goto-char (point-min))
        (setq-local inkling-current-id id)
        (setq-local inkling-current-url url)
        (setq-local inkling-current-title title)
        (inkling-read-mode 1)
        (visual-line-mode 1)
        (read-only-mode 1)))
    (switch-to-buffer-other-window buf)))

;; =====================================================================
;; Headers Mode (List View)
;; =====================================================================

(defun inkling-toggle-unread ()
  "Toggle the list view between Unread Only and All Items."
  (interactive)
  (setq inkling-show-unread-only (not inkling-show-unread-only))
  (setq tabulated-list-entries (inkling-get-entries))
  (tabulated-list-print t)
  (message (if inkling-show-unread-only "Filtered: Unread Only" "Showing All Items")))

(define-derived-mode inkling-headers-mode tabulated-list-mode "Inkling"
  "Major mode for browsing Inkwell feeds."
  ;; Tell the Date column to use our custom sorting function
  (setq tabulated-list-format [("St" 2 t) ("*" 1 t) ("Date" 12 inkling-sort-by-date) ("Author" 15 t) ("Title" 60 nil)])
  (setq tabulated-list-padding 2)
  ;; Restore the default sort order (t means descending)
  (setq tabulated-list-sort-key '("Date" . t))
  
  (tabulated-list-init-header)
  (hl-line-mode 1)
  (setq-local hl-line-sticky-flag t))

(define-key inkling-headers-mode-map (kbd "RET") 'inkling-read-post)
(define-key inkling-headers-mode-map (kbd "w") 'inkling-copy-url)
(define-key inkling-headers-mode-map (kbd "c") 'inkling-quote-and-compose)
(define-key inkling-headers-mode-map (kbd "b") 'inkling-toggle-bookmark)
(define-key inkling-headers-mode-map (kbd "r") 'inkling-toggle-read-status)
(define-key inkling-headers-mode-map (kbd "o") 'inkling-open-in-browser)
(define-key inkling-headers-mode-map (kbd "g") 'inkling-sync-feeds)
(define-key inkling-headers-mode-map (kbd "u") 'inkling-toggle-unread)

(defun inkling-sort-by-date (a b)
  "Custom sort function to compare full timestamps hidden in text properties."
  (let* ((date-a (get-text-property 0 'full-date (aref (cadr a) 2)))
         (date-b (get-text-property 0 'full-date (aref (cadr b) 2))))
    ;; Use standard string comparison. Emacs will reverse it if you click the column twice.
    (string< (or date-a "") (or date-b ""))))

(defun inkling-get-entries ()
  "Query DB and format rows, respecting the unread filter."
  (let* ((db (sqlite-open inkling-db-file))
         (query (if inkling-show-unread-only
                    "SELECT id, date_published, author, title, is_read, is_starred FROM feeds WHERE is_read = 0 ORDER BY date_published DESC"
                  "SELECT id, date_published, author, title, is_read, is_starred FROM feeds ORDER BY date_published DESC"))
         (rows (sqlite-select db query)))
    (mapcar (lambda (row)
              (let* ((id (nth 0 row))
                     (full-date (or (nth 1 row) ""))
                     (short-date (if (> (length full-date) 10) (substring full-date 0 10) full-date))
                     ;; Secretly attach the full timestamp to the short date string!
                     (date-display (propertize short-date 'full-date full-date))
                     (author (or (nth 2 row) "Unknown"))
                     (title (or (nth 3 row) "Untitled"))
                     (is-read (nth 4 row))
                     (is-starred (nth 5 row))
                     (status (if (= is-read 0) "•" " "))
                     (star-status (if (= is-starred 1) "★" " ")))
                (list id (vector status star-status date-display author title))))
            rows)))

;;;###autoload
(defun inkling-list ()
  "Open the Inkling RSS reader."
  (interactive)
  (inkling-init-db)
  (let ((buf (get-buffer-create "*Inkling Headers*")))
    (with-current-buffer buf
      (inkling-headers-mode)
      (setq inkling-show-unread-only t) 
      (setq tabulated-list-entries (inkling-get-entries))
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(provide 'inkling)
;;; inkling.el ends here
