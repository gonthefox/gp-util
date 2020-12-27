;; google patent utility
;; gp-util.el
;; -*- coding: utf-8 -*-

(require 'dom)
(require 'request)
(defcustom db-path "/mnt/c/wsl/db/patent/"
  "full path to the directory where rawfiles are stored.")
(defcustom rawfile-name "raw.html"
  "filename for the raw files.")

(defun gp-search-db-for-patent-util (patent-number)
  (let ((full-path (concat db-path patent-number "/" rawfile-name)))
    (if (file-exists-p full-path) (format "%s" full-path))))

;; kind-code 付きでパスが見つからなかった場合は、kind-code抜きで検索
(defun gp-search-db-for-patent (patent-number)
  "Search DB for the patent."
  (or (gp-search-db-for-patent-util patent-number)
      (gp-search-db-for-patent-util (strip-kind-code patent-number))))

(defun strip-kind-code (patent-number)
  (string-match "\\([A-Z]\\{2\\}[0-9]\\{7,8\\}\\)" patent-number)
  (setq short-pn (match-string 1 patent-number)))

;(gp-search-db-for-patent "US8165102B1")

(defun gp-get-dom (patent-number)
  "Convert a HTML into a DOM tree and return it"
  (let ((file (gp-search-db-for-patent patent-number)))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min) (point-max))
      (libxml-parse-html-region (point-min) (point-max)))))

;(gp-get-title "US7796941")

(defun gp-get-title (patent-number)
  "Obtain the title of the patent."
  (let ((span-list (dom-by-tag (gp-get-dom patent-number) 'span))
	(node))
    (while (not (string= (dom-attr node 'itemprop) "title"))
      (setq node (car span-list))
      (setq span-list (cdr span-list)))
    (nth 0 (dom-strings node))))

;; dom-attributes 特定のノードのattributesのリストを取り出す
;; dom-attr (node attr) 特定のノードの指定した attrの値を取り出す?

(defun gp-retrieve-patent (PATENT-NUMBER)
  "Retrieve a patent specified by PATENT-NUMBER from Google Patent 
   and transform into a dom tree"
  (let ((url (concat "https://patents.google.com/patent/" PATENT-NUMBER)))
    (with-current-buffer (url-retrieve-synchronously url)
      (mm-with-part (mm-dissect-buffer 'no-strict-mime)
	(libxml-parse-html-region (point-min) (point-max))))))

(defun gp-retrieve-and-store-patent (patent-number)
  "Retrieve a patent from Google patents and store it in DB-PATH as RAWFILE-NAME"
  (let ((url (concat "https://patents.google.com/patent/" patent-number)))
    (request url
      :parser  'buffer-string
      :success (function* (lambda (&key data &allow-other-keys)
			    (when data
			      (with-temp-buffer
				(erase-buffer)
				(insert data)
				(write-region (point-min) (point-max)
					      (concat db-path rawfile-name))
				))))
      :error (function* (lambda (&key error-thrown &allow-other-keys)
			  (message "Got error %S" error-thrown)))
      )
    ))

(provide 'gp-util)
