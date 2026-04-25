;;; inkling.el --- An Inkwell (Micro.blog) RSS Client -*- lexical-binding: t; -*-
;;; Version 2.0 (April 25, 2026)

(require 'sqlite)
(require 'json)
(require 'dom)
(require 'shr)
(require 'microbe) ;; Loads the file microbe.el, which contains microblog-* functions

;; =====================================================================
;; Configuration & Variables
;; =====================================================================

(defvar inkling-db-file (expand-file-name "inkling.sqlite" user-emacs-directory)
  "Path to the local Inkling database.")
(defvar inkling-max-sync-pages 2
  "Maximum number of pages to fetch during a sync. (Approx 100-200 items per page).")
(defvar inkling-opml-source-url "https://feedland.org/opml?screenname=miljko"
  "URL of your master OPML file (e.g., a raw GitHub URL or Feedland endpoint).
If left empty, Inkling will prompt you for the URL when you run a sync.")

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
  "Download entries (paginated), unread state, and starred state from Inkwell."
  (interactive)
  (inkling-init-db)
  (let* ((token (microblog-get-token)) ;; (Or microbe-get-token if renamed)
         (db (sqlite-open inkling-db-file))
         (unread-buf (generate-new-buffer "*inkling-unread*"))
         (starred-buf (generate-new-buffer "*inkling-starred*"))
         (total-count 0)
         (page 1)
         (fetching-entries t))
    
    (message "Syncing Inkwell feeds (this may take a moment)...")
    
    (sqlite-execute db "BEGIN TRANSACTION")
    
    ;; 1. Fetch Entries with Pagination
    (while (and fetching-entries (<= page inkling-max-sync-pages))
      (let ((entries-buf (generate-new-buffer "*inkling-entries*")))
        (message "Fetching page %d..." page)
        (call-process "curl" nil entries-buf nil "-s" "-H" (concat "Authorization: Bearer " token) 
                      (format "https://micro.blog/feeds/v2/entries.json?mode=extended&page=%d" page))
        
        (with-current-buffer entries-buf
          (goto-char (point-min))
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'symbol)
                 (items (condition-case nil (json-read) (error nil))))
            
            ;; If the API returns an empty list, we've reached the end of the history
            (if (or (null items) (zerop (length items)))
                (setq fetching-entries nil)
              
              (dolist (item items)
                (let* ((id (alist-get 'id item))
                       (url (alist-get 'url item))
                       (title (alist-get 'title item))
                       (content (alist-get 'content item))
                       (date (alist-get 'published item))
                       (author (alist-get 'author item)))
                  (sqlite-execute db "INSERT OR REPLACE INTO feeds (id, date_published, content, url, title, author, is_read, is_starred) VALUES (?, ?, ?, ?, ?, ?, 1, 0)"
                                  (list (format "%s" id) date content url title author))
                  (setq total-count (1+ total-count))))
              (setq page (1+ page)))))
        (kill-buffer entries-buf)))
    
    ;; 2. Fetch & Apply Unread State
    (message "Fetching unread state...")
    (call-process "curl" nil unread-buf nil "-s" "-H" (concat "Authorization: Bearer " token) 
                  "https://micro.blog/feeds/v2/unread_entries.json")
    (with-current-buffer unread-buf
      (goto-char (point-min))
      (let* ((json-array-type 'list)
             (unread-ids (condition-case nil (json-read) (error nil))))
        (when unread-ids
          (dolist (uid unread-ids)
            (sqlite-execute db "UPDATE feeds SET is_read = 0 WHERE id = ?" (list (format "%s" uid)))))))

    ;; 3. Fetch & Apply Starred State
    (message "Fetching bookmarked state...")
    (call-process "curl" nil starred-buf nil "-s" "-H" (concat "Authorization: Bearer " token) 
                  "https://micro.blog/feeds/v2/starred_entries.json")
    (with-current-buffer starred-buf
      (goto-char (point-min))
      (let* ((json-array-type 'list)
             (starred-ids (condition-case nil (json-read) (error nil))))
        (sqlite-execute db "UPDATE feeds SET is_starred = 0")
        (when starred-ids
          (dolist (uid starred-ids)
            (sqlite-execute db "UPDATE feeds SET is_starred = 1 WHERE id = ?" (list (format "%s" uid)))))))
    
    (sqlite-execute db "COMMIT")
    (kill-buffer unread-buf)
    (kill-buffer starred-buf)
    
    ;; Refresh UI
    (when (get-buffer "*Inkling Headers*")
      (with-current-buffer "*Inkling Headers*"
        (setq tabulated-list-entries (inkling-get-entries))
        (tabulated-list-print t)))
    (message "Inkwell sync complete! Fetched %d items across %d pages." total-count (1- page))))

;; =====================================================================
;; Core Actions
;; =====================================================================

(defun inkling-get-current-post-data ()
  "Return (ID URL TITLE) for the current post or bookmark."
  (cond
   ;; Standard Timeline Feeds
   ((derived-mode-p 'inkling-headers-mode)
    (let* ((id (tabulated-list-get-id))
           (db (sqlite-open inkling-db-file))
           (row (car (sqlite-select db "SELECT url, title FROM feeds WHERE id = ?" (list id)))))
      (list id (nth 0 row) (nth 1 row))))
   ;; New Bookmarks List
   ((derived-mode-p 'inkling-bookmarks-mode)
    (let* ((id (tabulated-list-get-id))
           (db (sqlite-open inkling-db-file))
           (row (car (sqlite-select db "SELECT url, title FROM bookmarks WHERE id = ?" (list id)))))
      (list id (nth 0 row) (nth 1 row))))
   ;; Inside a specific post/bookmark reading buffer
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
  "Toggle the starred/bookmarked status of the current post asynchronously."
  (interactive)
  (let* ((data (inkling-get-current-post-data))
         (id (nth 0 data))
         (db (sqlite-open inkling-db-file))
         (row (car (sqlite-select db "SELECT is_starred FROM feeds WHERE id = ?" (list id))))
         (currently-starred (= (nth 0 row) 1))
         (new-status (if currently-starred 0 1))
         (token (microblog-get-token))
         (temp-file (make-temp-file "inkling-star")))
    
    ;; 1. Update local database instantly
    (sqlite-execute db "UPDATE feeds SET is_starred = ? WHERE id = ?" (list new-status id))
    
    ;; 2. Update UI instantly
    (when (get-buffer "*Inkling Headers*")
      (with-current-buffer "*Inkling Headers*"
        (let ((entry (assoc id tabulated-list-entries)))
          (when entry
            (aset (cadr entry) 1 (if (= new-status 1) "★" " "))))
        (tabulated-list-print t)))
    
    ;; 3. Prepare payload
    (with-temp-file temp-file
      (insert (json-encode `((starred_entries . [,(string-to-number id)])))))
    
    ;; 4. Fire background curl
    (let* ((method (if currently-starred "DELETE" "POST"))
           (curl-buf (generate-new-buffer "*inkling-bookmark*"))
           (proc (start-process "inkling-bg-star" curl-buf "curl" "-s" "-w" "%{http_code}" "-X" method
                                "-H" (concat "Authorization: Bearer " token)
                                "-H" "Content-Type: application/json"
                                "-d" (concat "@" temp-file)
                                "https://micro.blog/feeds/v2/starred_entries.json")))
      
      ;; 5. Clean up and report success in the background
      (set-process-sentinel proc
                            (lambda (process event)
                              (when (memq (process-status process) '(exit signal))
                                (with-current-buffer (process-buffer process)
                                  (goto-char (point-max))
                                  (let ((http-status (buffer-substring (- (point-max) 3) (point-max))))
                                    (if (member http-status '("200" "201" "202" "204"))
                                        (message (if currently-starred "Removed bookmark." "Added bookmark!"))
                                      (message "Bookmark sync failed. HTTP %s" http-status))))
                                (ignore-errors (delete-file temp-file))
                                (kill-buffer (process-buffer process))))))))

(defun inkling-toggle-read-status ()
  "Toggle the read/unread status of the current post asynchronously."
  (interactive)
  (let* ((data (inkling-get-current-post-data))
         (id (nth 0 data))
         (db (sqlite-open inkling-db-file))
         (row (car (sqlite-select db "SELECT is_read FROM feeds WHERE id = ?" (list id))))
         (currently-read (= (nth 0 row) 1))
         (new-status (if currently-read 0 1))
         (token (microblog-get-token))
         (temp-file (make-temp-file "inkling-toggle")))
    
    ;; 1. Update local database instantly
    (sqlite-execute db "UPDATE feeds SET is_read = ? WHERE id = ?" (list new-status id))
    
    ;; 2. Update UI instantly
    (when (get-buffer "*Inkling Headers*")
      (with-current-buffer "*Inkling Headers*"
        (let ((entry (assoc id tabulated-list-entries)))
          (when entry
            (aset (cadr entry) 0 (if (= new-status 0) "•" " "))))
        (tabulated-list-print t)))
    
    ;; 3. Prepare payload
    (with-temp-file temp-file
      (insert (json-encode `((unread_entries . [,(string-to-number id)])))))
    
    ;; 4. Fire background curl
    (let* ((endpoint (if currently-read
                         "https://micro.blog/feeds/v2/unread_entries.json"
                       "https://micro.blog/feeds/v2/unread_entries/delete.json"))
           (curl-buf (generate-new-buffer "*inkling-network*"))
           (proc (start-process "inkling-bg-toggle" curl-buf "curl" "-s" "-w" "%{http_code}" "-X" "POST"
                                "-H" (concat "Authorization: Bearer " token)
                                "-H" "Content-Type: application/json"
                                "-d" (concat "@" temp-file)
                                endpoint)))
      
      ;; 5. Clean up and report success in the background
      (set-process-sentinel proc
                            (lambda (process event)
                              (when (memq (process-status process) '(exit signal))
                                (with-current-buffer (process-buffer process)
                                  (goto-char (point-max))
                                  (let ((http-status (buffer-substring (- (point-max) 3) (point-max))))
                                    (if (member http-status '("200" "201" "202"))
                                        (message (if currently-read "Marked as unread." "Marked as read."))
                                      (message "Sync failed. HTTP %s" http-status))))
                                (ignore-errors (delete-file temp-file))
                                (kill-buffer (process-buffer process))))))))

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
  "Mark a post as read locally and sync to the server asynchronously."
  (let* ((token (microblog-get-token))
         (db (sqlite-open inkling-db-file))
         (temp-file (make-temp-file "inkling-read")))
    
    ;; 1. Update DB instantly
    (sqlite-execute db "UPDATE feeds SET is_read = 1 WHERE id = ?" (list id))
    
    ;; 2. Prepare payload
    (with-temp-file temp-file
      (insert (json-encode `((unread_entries . [,(string-to-number id)])))))
    
    ;; 3. Fire curl in the background so Emacs doesn't freeze
    (let ((proc (start-process "inkling-bg-read" nil "curl" "-s" "-X" "POST"
                               "-H" (concat "Authorization: Bearer " token)
                               "-H" "Content-Type: application/json"
                               "-d" (concat "@" temp-file)
                               "https://micro.blog/feeds/v2/unread_entries/delete.json")))
      
      ;; 4. Set a background watcher to clean up the temp file when curl finishes
      (set-process-sentinel proc
                            (lambda (process event)
                              (ignore-errors (delete-file temp-file)))))))

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
         ;; Clean the title to prevent newlines in buffer names
         (title (inkling-clean-string (or (nth 0 row) "Untitled")))
         (content (nth 1 row))
         (url (nth 2 row))
         ;; THE FIX: Append the unique ID to the buffer name
         (buf (get-buffer-create (format "*Inkling: %s [%s]*" title id))))
    
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
(define-key inkling-headers-mode-map (kbd "M-b") 'inkling-bookmark-current)
(define-key inkling-read-mode-map (kbd "M-b") 'inkling-bookmark-current)

(defun inkling-sort-by-date (a b)
  "Custom sort function to compare full timestamps hidden in text properties."
  (let* ((date-a (get-text-property 0 'full-date (aref (cadr a) 2)))
         (date-b (get-text-property 0 'full-date (aref (cadr b) 2))))
    ;; Use standard string comparison. Emacs will reverse it if you click the column twice.
    (string< (or date-a "") (or date-b ""))))

(defun inkling-clean-string (str)
  "Strip newlines and compress whitespace so `tabulated-list-mode` doesn't break."
  (if (and str (stringp str))
      (string-trim (replace-regexp-in-string "[ \n\r\t]+" " " str))
    ""))

(defun inkling-get-entries ()
  "Query DB and format rows, respecting the unread filter."
  (let* ((db (sqlite-open inkling-db-file))
         ;; Add 'url' to the end of the SELECT statements so we can use it as a fallback
         (query (if inkling-show-unread-only
                    "SELECT id, date_published, author, title, is_read, is_starred, url FROM feeds WHERE is_read = 0 GROUP BY COALESCE(NULLIF(url, ''), id) ORDER BY date_published DESC"
                  "SELECT id, date_published, author, title, is_read, is_starred, url FROM feeds GROUP BY COALESCE(NULLIF(url, ''), id) ORDER BY date_published DESC"))
         (rows (sqlite-select db query)))
    (mapcar (lambda (row)
              (let* ((id (nth 0 row))
                     (full-date (or (nth 1 row) ""))
                     (short-date (if (> (length full-date) 10) (substring full-date 0 10) full-date))
                     ;; Secretly attach the full timestamp to the short date string
                     (date-display (propertize short-date 'full-date full-date))
                     (author-raw (nth 2 row))
                     (title (or (nth 3 row) "Untitled"))
                     (is-read (nth 4 row))
                     (is-starred (nth 5 row))
                     (url (nth 6 row)) ; Extract the newly queried url
                     
                     ;; Smart fallback logic for the Author column
                     (author (cond
                              ;; 1. Use the author if it exists and isn't an empty string
                              ((and author-raw (not (string= author-raw ""))) author-raw)
                              ;; 2. Fallback: Extract the domain from the URL (stripping https and www)
                              ((and url (string-match "^https?://\\(?:www\\.\\)?\\([^/]+\\)" url))
                               (match-string 1 url))
                              ;; 3. Final fallback
                              (t "Unknown")))
		     ;; Wrap the outputs in inkling-clean-string
                     (clean-author (inkling-clean-string author))
                     (clean-title (inkling-clean-string (or (nth 3 row) "Untitled")))
                     
                     (status (if (= is-read 0) "•" " "))
                     (star-status (if (= is-starred 1) "★" " ")))
                ;; Use the clean variables in the vector
                (list id (vector status star-status date-display clean-author clean-title))))
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

;; =====================================================================
;; Bookmarks Engine & UI
;; =====================================================================

(defun inkling-init-bookmarks-db ()
  "Initialize the bookmarks table in the Inkling database."
  (interactive)
  (let ((db (sqlite-open inkling-db-file)))
    (sqlite-execute db "
      CREATE TABLE IF NOT EXISTS bookmarks (
        id TEXT PRIMARY KEY,
        url TEXT,
        title TEXT,
        content TEXT,
        tags TEXT,
        date_bookmarked TEXT
      );")))

(defun inkling-sync-bookmarks ()
  "Download and sync bookmarks and tags from Micro.blog, auto-fetching missing titles."
  (interactive)
  (inkling-init-bookmarks-db)
  (let* ((token (microblog-get-token))
         (db (sqlite-open inkling-db-file))
         (buf (generate-new-buffer "*inkling-bm-sync*"))
         (bookmarks-to-fetch nil)) ;; NEW: Queue for background fetching
    (message "Syncing bookmarks...")
    (let ((sync-success nil))
      (sqlite-execute db "BEGIN TRANSACTION")
      (unwind-protect
          (progn
            (call-process "curl" nil buf nil "-s" "-H" (concat "Authorization: Bearer " token) 
                          "https://micro.blog/posts/bookmarks")
            (with-current-buffer buf
              (goto-char (point-min))
              (let* ((json-object-type 'alist)
                     (json-array-type 'list)
                     (json-key-type 'symbol)
                     (data (condition-case nil (json-read) (error nil)))
                     (items (alist-get 'items data)))
                (when items
                  (dolist (item items)
                    (let* ((id (format "%s" (alist-get 'id item)))
                           (api-title (alist-get 'title item))
                           (url (alist-get 'url item))
                           (content (or (alist-get 'content_html item) (alist-get 'content item)))
                           (tags-raw (alist-get 'tags item))
                           (tags (cond
                                  ((stringp tags-raw) tags-raw)
                                  ((or (listp tags-raw) (vectorp tags-raw)) 
                                   (mapconcat (lambda (x) (format "%s" x)) tags-raw ", "))
                                  (t "")))
                           (date (alist-get 'date_published item))
                           
                           (local-row (car (sqlite-select db "SELECT title FROM bookmarks WHERE id = ?" (list id))))
                           (local-title (nth 0 local-row))
                           
                           (final-title (cond
                                         ((and local-title (not (string-blank-p local-title))) local-title)
                                         ((and api-title (not (string-blank-p api-title))) api-title)
                                         (t ""))))
                      
                      (sqlite-execute db "INSERT OR REPLACE INTO bookmarks (id, url, title, content, tags, date_bookmarked) VALUES (?, ?, ?, ?, ?, ?)"
                                      (list id url final-title content tags date))
                      
                      ;; NEW: If we have a URL but no title, queue it up!
                      (when (and (string-blank-p final-title) url (not (string-blank-p url)))
                        (push (cons id url) bookmarks-to-fetch)))))))
            
            (sqlite-execute db "COMMIT")
            (setq sync-success t))
        (unless sync-success
          (sqlite-execute db "ROLLBACK")
          (message "Sync aborted: Database lock safely released."))))
    (kill-buffer buf)
    
    (when (get-buffer "*Inkling Bookmarks*")
      (with-current-buffer "*Inkling Bookmarks*"
        (setq tabulated-list-entries (inkling-get-bookmarks-entries))
        (tabulated-list-print t)))
    
    ;; NEW: The DB is unlocked. Safe to fire off the async fetchers!
    (if bookmarks-to-fetch
        (progn
          (message "Sync complete! Fetching %d missing titles in the background..." (length bookmarks-to-fetch))
          (dolist (bm bookmarks-to-fetch)
            (inkling--async-fetch-title (car bm) (cdr bm))))
      (message "Bookmarks sync complete!"))))

(defun inkling--async-fetch-title (id url)
  "Internal function to asynchronously fetch an HTML title for a given ID and URL."
  (url-retrieve url
                (lambda (status id db-file)
                  (unless (plist-get status :error)
                    (goto-char (point-min))
                    ;; Skip raw HTTP headers
                    (when (search-forward "\n\n" nil t)
                      (let* ((dom (libxml-parse-html-region (point) (point-max)))
                             (title-node (car (dom-by-tag dom 'title)))
                             (fetched-title (when title-node (dom-text title-node))))
                        (when fetched-title
                          (let ((clean-title (inkling-clean-string fetched-title))
                                (db (sqlite-open db-file)))
                            ;; 1. Update the local DB
                            (sqlite-execute db "UPDATE bookmarks SET title = ? WHERE id = ?" (list clean-title id))
                            ;; 2. Update the UI natively if the buffer is open
                            (when (get-buffer "*Inkling Bookmarks*")
                              (with-current-buffer "*Inkling Bookmarks*"
                                (let ((entry (assoc id tabulated-list-entries)))
                                  (when entry
                                    ;; Update the Title column (index 2)
                                    (aset (cadr entry) 2 clean-title)))
                                (tabulated-list-print t)))
                            (message "Title fetched: %s" clean-title))))))
                  ;; Always clean up the background buffer
                  (kill-buffer (current-buffer)))
                ;; Pass variables into the callback
                (list id inkling-db-file)))

(defun inkling-bookmarks-fetch-title ()
  "Fetch the HTML title for the bookmark under cursor and update the database locally."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (db (sqlite-open inkling-db-file))
         (row (car (sqlite-select db "SELECT url FROM bookmarks WHERE id = ?" (list id))))
         (url (nth 0 row)))
    (if (or (null url) (string-blank-p url))
        (message "No URL found for this bookmark.")
      (message "Fetching title for %s..." url)
      (inkling--async-fetch-title id url))))

;; =====================================================================
;; Bookmarks List View
;; =====================================================================

(defvar-local inkling-bookmark-tag-filter nil
  "Current tag used to filter the bookmarks list view.")

(defun inkling-get-bookmarks-entries ()
  "Query the database for bookmarks, respecting the active tag filter."
  (let* ((db (sqlite-open inkling-db-file))
         (query (if inkling-bookmark-tag-filter
                    "SELECT id, date_bookmarked, tags, title, url FROM bookmarks WHERE tags LIKE ? ORDER BY date_bookmarked DESC"
                  "SELECT id, date_bookmarked, tags, title, url FROM bookmarks ORDER BY date_bookmarked DESC"))
         (params (if inkling-bookmark-tag-filter
                     (list (format "%%%s%%" inkling-bookmark-tag-filter))
                   nil))
         (rows (sqlite-select db query params)))
    (mapcar (lambda (row)
              (let* ((id (nth 0 row))
                     (full-date (or (nth 1 row) ""))
                     (short-date (if (> (length full-date) 10) (substring full-date 0 10) full-date))
                     (date-display (propertize short-date 'full-date full-date))
                     (tags (or (nth 2 row) ""))
                     (title-raw (nth 3 row))
                     (url-raw (nth 4 row))
                     (url (or url-raw ""))
                     
                     ;; 1. Extract just the short domain for the URL column
                     (short-url (if (string-match "^https?://\\(?:www\\.\\)?\\([^/]+\\)" url)
                                    (match-string 1 url)
                                  url))
                     
                     ;; 2. Strict Title logic (No longer falling back to the URL)
                     (title (if (and title-raw (not (string-blank-p title-raw)))
                                title-raw
                              "Untitled"))
                     
                     (clean-title (inkling-clean-string title))
                     (clean-domain (inkling-clean-string short-url))
                     (clean-tags (inkling-clean-string tags)))
                
                ;; Output the 4 distinct columns
                (list id (vector date-display clean-tags clean-title clean-domain))))
            rows)))

(defun inkling-bookmarks-filter ()
  "Prompt for a tag and filter the bookmarks list."
  (interactive)
  (let ((tag (read-string "Filter by tag (leave blank to clear): ")))
    (setq inkling-bookmark-tag-filter (if (string-blank-p tag) nil tag))
    (setq tabulated-list-entries (inkling-get-bookmarks-entries))
    (tabulated-list-print t)
    (message (if inkling-bookmark-tag-filter
                 (format "Filtered to: %s" tag)
               "Cleared filters."))))

(defun inkling-read-bookmark ()
  "Open the selected bookmark in a reading buffer."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (db (sqlite-open inkling-db-file))
         (row (car (sqlite-select db "SELECT title, content, url FROM bookmarks WHERE id = ?" (list id))))
         (title (inkling-clean-string (or (nth 0 row) "Untitled")))
         (content (nth 1 row))
         (url (nth 2 row))
         (buf (get-buffer-create (format "*Inkling Bookmark: %s [%s]*" title id))))
    
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "<h1><a href=\"%s\">%s</a></h1><br>" (or url "#") title))
        (when content (insert content))
        (shr-render-region (point-min) (point-max))
        (goto-char (point-min))
        ;; Pass context down so 'o' and 'w' keys work natively inside the reader
        (setq-local inkling-current-id id)
        (setq-local inkling-current-url url)
        (setq-local inkling-current-title title)
        (inkling-read-mode 1)
        (visual-line-mode 1)
        (read-only-mode 1)))
    (switch-to-buffer-other-window buf)))

(define-derived-mode inkling-bookmarks-mode tabulated-list-mode "Inkling Bookmarks"
  "Major mode for browsing Micro.blog Bookmarks."
  (setq tabulated-list-format [("Date" 12 inkling-sort-by-date) 
                               ("Tags" 15 t) 
                               ("Title" 50 t)
                               ("Domain" 20 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Date" . t))
  (tabulated-list-init-header)
  (hl-line-mode 1)
  (setq-local hl-line-sticky-flag t))

(define-key inkling-bookmarks-mode-map (kbd "RET") 'inkling-read-bookmark)
(define-key inkling-bookmarks-mode-map (kbd "o") 'inkling-open-in-browser)
(define-key inkling-bookmarks-mode-map (kbd "w") 'inkling-copy-url)
(define-key inkling-bookmarks-mode-map (kbd "g") 'inkling-sync-bookmarks)
(define-key inkling-bookmarks-mode-map (kbd "f") 'inkling-bookmarks-filter)
(define-key inkling-bookmarks-mode-map (kbd "t") 'inkling-bookmarks-edit-tags)
(define-key inkling-bookmarks-mode-map (kbd "d") 'inkling-bookmarks-delete)

;;;###autoload
(defun inkling-bookmarks ()
  "Open the Inkling Bookmarks reader."
  (interactive)
  (inkling-init-bookmarks-db)
  (let ((buf (get-buffer-create "*Inkling Bookmarks*")))
    (with-current-buffer buf
      (inkling-bookmarks-mode)
      (setq inkling-bookmark-tag-filter nil)
      (setq tabulated-list-entries (inkling-get-bookmarks-entries))
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(require 'url-util) ;; Ensures URL encoding functions are available

;; =====================================================================
;; Tagging, Adding & Deleting Bookmarks
;; =====================================================================

(defun inkling-bookmarks-edit-tags ()
  "Edit the tags for the bookmark currently under the cursor."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (db (sqlite-open inkling-db-file))
         ;; We no longer need to query the URL, just the tags
         (row (car (sqlite-select db "SELECT tags FROM bookmarks WHERE id = ?" (list id))))
         (old-tags (or (nth 0 row) ""))
         (new-tags (read-string "Tags (comma-separated): " old-tags))
         (token (microblog-get-token))
         (temp-file (make-temp-file "inkling-tag")))
    
    ;; 1. Update the local SQLite database instantly
    (sqlite-execute db "UPDATE bookmarks SET tags = ? WHERE id = ?" (list new-tags id))
    
    ;; 2. Update the Emacs UI instantly
    (let ((entry (assoc id tabulated-list-entries)))
      (when entry
        (aset (cadr entry) 1 (inkling-clean-string new-tags))))
    (tabulated-list-print t)
    
    ;; 3. Prepare the payload (ONLY tags)
    (with-temp-file temp-file
      (insert (format "tags=%s" (url-hexify-string new-tags))))
    
    ;; 4. Fire the curl request to the specific ID endpoint
    (let ((proc (start-process "inkling-bg-tag" nil "curl" "-s" "-X" "POST"
                               "-H" (concat "Authorization: Bearer " token)
                               "-d" (concat "@" temp-file)
                               (format "https://micro.blog/posts/bookmarks/%s" id))))
      (set-process-sentinel proc
                            (lambda (process event)
                              (ignore-errors (delete-file temp-file))
                              (message "Tags synced to Micro.blog!"))))))

(defun inkling-bookmark-current ()
  "Bookmark the current URL (or prompt for one) and add tags."
  (interactive)
  (let* ((default-url (cond
                       ((derived-mode-p 'inkling-read-mode) inkling-current-url)
                       ((derived-mode-p 'inkling-headers-mode) (nth 1 (inkling-get-current-post-data)))
                       (t "")))
         (url (read-string "Bookmark URL: " default-url))
         (tags (read-string "Tags (comma-separated): "))
         (token (microblog-get-token))
         (temp-file (make-temp-file "inkling-new-bm")))
    
    ;; 1. Format for Micropub: h=entry & bookmark-of=URL
    (with-temp-file temp-file
      (insert (format "h=entry&bookmark-of=%s" (url-hexify-string url)))
      ;; 2. Micropub expects tags as repeated category[] parameters
      (unless (string-blank-p tags)
        (let ((tag-list (split-string tags "," t "[ \t]+")))
          (dolist (tag tag-list)
            (insert (format "&category[]=%s" (url-hexify-string tag)))))))
    
    (message "Adding bookmark...")
    
    ;; 3. Fire the curl request to the Micropub endpoint
    (let ((proc (start-process "inkling-bg-add-bm" nil "curl" "-s" "-X" "POST"
                               "-H" (concat "Authorization: Bearer " token)
                               "-d" (concat "@" temp-file)
                               "https://micro.blog/micropub")))
      (set-process-sentinel proc
                            (lambda (process event)
                              (ignore-errors (delete-file temp-file))
                              (message "Bookmark saved! Press 'g' in the Bookmarks list to pull it down."))))))

(defun inkling-bookmarks-delete ()
  "Delete the bookmark under the cursor locally and on Micro.blog."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (db (sqlite-open inkling-db-file))
         ;; Fetch the title just to make the confirmation prompt friendly
         (row (car (sqlite-select db "SELECT title FROM bookmarks WHERE id = ?" (list id))))
         (title (or (nth 0 row) "this bookmark"))
         (token (microblog-get-token)))
    
    ;; Ask for confirmation before dropping the hammer
    (when (y-or-n-p (format "Really delete '%s'? " title))
      
      ;; 1. Nuke it from the local database
      (sqlite-execute db "DELETE FROM bookmarks WHERE id = ?" (list id))
      
      ;; 2. Instantly refresh the UI to remove the row
      (setq tabulated-list-entries (inkling-get-bookmarks-entries))
      (tabulated-list-print t)
      
      ;; 3. Fire the DELETE request to Micro.blog in the background
      (let ((proc (start-process "inkling-bg-delete" nil "curl" "-s" "-X" "DELETE"
                                 "-H" (concat "Authorization: Bearer " token)
                                 (format "https://micro.blog/posts/bookmarks/%s" id))))
        (set-process-sentinel proc
                              (lambda (process event)
                                (message "Bookmark permanently deleted.")))))))

(provide 'inkling)
;;; inkling.el ends here
