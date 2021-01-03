;; google patent utility
;; gp-util.el
;; -*- coding: utf-8 -*-

(require 'dom)
(require 'request)

(defcustom db-path "/var/db/patent/"
  "full path to the directory where rawfiles are stored.")

(defcustom rawfile-name "raw.html"
  "filename for the raw files.")

(defcustom gp-url "https://patents.google.com/patent/"
  "URL of Google Patents")

(defcustom wget-program "/usr/bin/wget"
  "path for wget")

(defun gp-full-path-to-rawfile-store (patent-number)
  "Return the full path to a rawfile store."
  (concat db-path patent-number "/"))

(defun gp-full-path-to-rawfile (patent-number)
  "Return the full path to a raw file."
  (concat (gp-full-path-to-rawfile-store patent-number) rawfile-name))

(defun gp-find-patent (patent-number)
  "Return t if the raw file exists"
  (if (file-exists-p (gp-full-path-to-rawfile patent-number)) t))

(defun gp-get-patent (patent-number)
  "Get a patent document as a string of the raw html"
  ;; 指定した特許公報がDBに存在すれば取得
  (if (gp-find-patent patent-number) (gp-get-patent-from-db patent-number)
    ;; Google Patentから特許公報を取得
    (gp-retrieve-patent patent-number)
    ;; 指定した特許公報がDBに存在すれば取得    
    (if (gp-find-patent patent-number) (gp-get-patent-from-db patent-number)
      (message "Error: %s cannot load." patent-number ))))

(defun gp-get-patent-from-db (patent-number)
  (with-temp-buffer
    (insert-file-contents (gp-full-path-to-rawfile patent-number))
    (while (re-search-forward "^[\s-]+$" nil t)
      (replace-match ""))
    (flush-lines "^\n")
    (buffer-string)))    

(defun gp-retrieve-patent (patent-number)
  (gp-retrieve-and-store-patent patent-number)
  )

(defun gp-search-db-for-patent-util (patent-number)
  "Return full path to the specified patent as a raw html file"
  (let ((filename  (gp-full-path-to-rawfile patent-number)))
    (if (file-exists-p filename) (format "%s" filename) nil)))

;; kind-code 付きでパスが見つからなかった場合は、kind-code抜きで検索
(defun gp-search-db-for-patent (patent-number)
  "Search DB for the patent."
  (or (gp-search-db-for-patent-util patent-number)
      (gp-search-db-for-patent-util (strip-kind-code patent-number))
      nil
      ))

(defun strip-kind-code (patent-number)
  (string-match "\\([A-Z]\\{2\\}[0-9]\\{7,8\\}\\)" patent-number)
  (setq short-pn (match-string 1 patent-number)))

(defun gp-retrieve-and-store-patent (PATENT-NUMBER)
  "Retrieve a patent specified by PATENT-NUMBER from Google Patent 
   and transform into a dom tree"
  (gp-retrieve-and-store-patent-wget patent-number))

(defun gp-retrieve-and-store-patent-wget (patent-number)
  "Retrieve a patent from Google patents and store it in DB-PATH as RAWFILE-NAME"
  (let ((url  (concat gp-url  patent-number))
	(store (gp-full-path-to-rawfile-store patent-number))
	(file  (gp-full-path-to-rawfile patent-number)))
    (unless (file-exists-p file)
      (unless (file-exists-p store) (make-directory store t))
      (async-shell-command
       (mapconcat #'shell-quote-argument
		  (list wget-program url "-O" file) " ")))))

(defun gp-get-patent-as-dom-1 (patent-number)
  (with-temp-buffer
    (insert (gp-get-patent patent-number))
    (while (re-search-forward "^[\s-]+$" nil t)
      (replace-match ""))
    (flush-lines "^\n")
    (libxml-parse-html-region (point-min) (point-max))
    ))

(defun gp-get-patent-as-dom (patent-number)
  (your-sanitize-function (gp-get-patent-as-dom-1 patent-number)))

(defun gp-get-title (patent-number)
  (let ( (dom (gp-get-patent-as-dom patent-number)))
    (setq span-list (dom-by-tag (dom-by-tag dom 'body) 'span))
    (if (string= (cdr (assq 'itemprop (nth 1 (car span-list)))) "title")
	(replace-regexp-in-string "^ +$" "" (nth 2 (car span-list))))))

(defun gp-get-application-number (patent-number)
  (let (( dom (gp-get-patent-as-dom patent-number))
	( output '()))
    (setq span-list (dom-by-tag (dom-by-tag dom 'body) 'span))
    (while span-list
      (if (string= (cdr (assq 'itemprop (nth 1 (car span-list)))) "applicationNumber")
	  (setq output (cons  (nth 2 (car span-list)) output ))
	)
      (setq span-list (cdr span-list))
      )
    output
    ))

;(defun gp-get-abstract (patent-number)
;  (dom-text (dom-by-class (gp-get-section patent-number "abstract") "abstract")))

(defun gp-get-section (patent-number section-id)
(let ((section-list (dom-by-tag (gp-get-patent-as-dom patent-number) 'section)))
  (cl-reduce (lambda (s a) (if (string= (dom-attr s 'itemprop) section-id) s a)) section-list :initial-value nil)))

(defun gp-get-abstract (patent-number)
  (gp-get-section patent-number "abstract"))

(defun gp-get-description (patent-number)
  (gp-get-section patent-number "description")
  )

(defun gp-get-claims (patent-number)
  (gp-get-section patent-number "claims")
  )


(defun your-sanitize-function (dom &optional result)
  (push (nreverse
         (cl-reduce (lambda (acc object)
                      (cond
                       ((and (stringp object)
                             (not (string-match-p "[[:graph:]]" object)))
                        acc)
                       ((or (atom object)
                            (consp (car object)))
                        (cons object acc))
                       (t
                        (your-sanitize-function object acc))))
                    dom :initial-value nil))
        result))

;;(gp-get-patent "US6097367")

(provide 'gp-util)
