;; gp-util-print.el
;;
(defcustom gp-style-file "/var/db/patent/config/style.org"
  "full path to the directory where style.org will be stored.")

(defun gp-import-style-file ()
  (copy-file gp-style-file "./" t))

(defun gp-full-path-to-pdf (patent-number)
  (concat (gp-full-path-to-rawfile-store patent-number)
	  (file-name-nondirectory (dom-attr (gp-get-link-item patent-number 'pdfLink) 'href))))

(defun gp-import-pdf-from-db (patent-number)
  (let ((full-path-to-pdf (gp-full-path-to-pdf patent-number)))
    (copy-file full-path-to-pdf "./" t)))

(defun gp-import-figs-from-db (patent-number)
  (let ((image-store   (gp-full-path-to-images-store patent-number))
	(rawfile-store (gp-full-path-to-rawfile-store patent-number)))
    (unless (file-exists-p local-image-store) 
      (copy-directory image-store local-image-store))))

(defun gp-import-image-aliases-from-db (patent-number)
  (let ((full-path-to-image-aliases (concat (gp-full-path-to-rawfile-store patent-number) image-aliases-name)))
         (unless (file-exists-p full-path-to-image-aliases) (gp-create-image-aliases patent-number))
	 (copy-file full-path-to-image-aliases "./" t)))

(defun gp-import-image-embeded-files-from-db (patent-number)
  (let ((figrefs (gp-get-figrefs patent-number)))
    (while figrefs
      (let ((target (concat (gp-full-path-to-rawfile-store patent-number) (format "%s.org" (downcase (car figrefs))))))
	(if (file-exists-p target) (copy-file target "./" t) (gp-create-image-embeded-files patent-number)))
      (setq figrefs (cdr figrefs)))))

  (defun gp-pretty-print-patent-number (patent-number)
    (if (stringp patent-number)
	(let (( country-code (if (string-match "^[a-zA-Z]\\{2\\}" patent-number) (match-string 0 patent-number) nil))
	      ( kind-code    (if (string-match "[a-zA-Z][0-9]$" patent-number) (match-string 0 patent-number) nil))
	      ( just-number  (if (string-match "[0-9]+" patent-number) (match-string 0 patent-number) nil)))

	  (cond ((string= country-code "US") 
	                         (if (string= (substring kind-code 0 1) "A") 
				 (format "%s %s %s" country-code (gp-pretty-print-us-application just-number) kind-code)
                                 (format "%s %s %s" country-code (gp-pretty-print-usp just-number) kind-code )))
	        (t patent-number)
	  ))

      patent-number))

  (defun gp-pretty-print-usp (just-number)
    (let ((acc nil))
      (while (> (length just-number) 0)
	(if (>= (length just-number) 3)
	    (progn 
	      (if (null acc) (setq acc (format "%s" (substring just-number (- (length just-number) 3) (length just-number))))
		(setq acc (format "%s,%s" (substring just-number (- (length just-number) 3) (length just-number)) acc)))
	      (setq just-number (substring just-number 0 (- (length just-number) 3))))
	  (progn (setq acc (format "%s,%s" just-number acc)) (setq just-number nil))))
      acc))

  (defun gp-pretty-print-us-application (just-number)
         (if (>= (length just-number) 4)
	     (format "%s/%s" (substring just-number 0 4) (substring just-number 4 (length just-number)))
	     just-number))

;; gp-print-specification
(defun gp-print-specification (patent-number)
  "Print specification as HTML"
  (gp-get-patent patent-number)
  (if (file-exists-p (gp-full-path-to-images-store patent-number))
      (gp-import-figs-from-db patent-number))
  (gp-import-image-aliases-from-db patent-number)
  (gp-import-image-embeded-files-from-db patent-number)
  (if (file-exists-p (gp-full-path-to-pdf patent-number))
      (gp-import-pdf-from-db patent-number))
  (gp-import-style-file)
  (with-temp-buffer
    (insert (format "#+html: <h1 style=\"text-align: center;\">%s</h1>\n"
		    (replace-regexp-in-string "\\s-+$" "" (dom-text (gp-get-title patent-number)))))
    (insert (format "#+html: <h2 style=\"text-align: center;\">%s</h2>\n"
		    (gp-pretty-print-patent-number (dom-text (gp-get-representative-publication patent-number)))))
    (insert (format "#+author: %s\n" (nth 2 (gp-get-inventor patent-number))))
    (insert (format "#+date: %s\n" (nth 2 (gp-get-filing-date patent-number))))
    (insert (format "#+options: toc:nil H:5\n"))
    (insert (format "#+TOC: headlines 5\n"))
    (insert (format "#+include: \"%s\" \n" image-aliases-name))
    (insert (format "#+include: \"%s\" \n" style-name))    
    (insert (gp-abstract-renderer (gp-get-abstract patent-number)))
    (insert (gp-description-renderer (gp-get-description patent-number)))
    (insert (gp-claims-renderer (gp-get-claims patent-number)))
;;    (insert (gp-claim-tree-renderer-asterisk (gp-make-claim-tree patent-number)))
    (buffer-string)))

(provide 'gp-util-print)
