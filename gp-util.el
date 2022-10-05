;;; gp-util.el --- Google Patent utilities

;;  Auther:  Katsuhito Ishida <katsuhito.ishida@gmail.com>
;;  Version: 0.1
;;; Commentary:
;;  This package provides a set of useful APIs for patent analysis.
;;  
;; 
;; -*- coding: utf-8 -*-
;; (setq patent-number "JP2003085659A")
;;

(require 'dom)
(require 'request)

(setq url-user-agent "User-Agent: w3m/0.5.3\r\n")

;(load "gp-util-claim")
;(load "gp-util-print")
;(load "gp-util-misc")

(defcustom db-path "/var/db/patent/"
  "full path to the directory where rawfiles will be stored.")

(defcustom rawfile-name "raw.html"
  "filename for the raw files.")

(defcustom listfile-name "list.el"
  "filename for the list files.")

(defcustom image-aliases-name "image-aliases.org"
  "filename for the image-aliases.")

(defcustom style-name "style.org"
  "filename for the stylesheet.")

(defcustom gp-url "https://patents.google.com/patent/"
  "URL of Google Patents")

(defcustom local-image-store "./figs"
  "path to local store for image file")

(defcustom db-image-store "./images"
  "path to db store for image file")

(defcustom wget-program "/usr/bin/wget"
  "path for wget")

(defcustom copy-program "/bin/cp"
  "path for cp")


(defun gp-retriever (patent-number)
  "Return paetnt content as s-expression."
  (concat (gp-full-path-to-rawfile-store patent-number) listfile-name))
  
(defun gp-full-path-to-rawfile-store (patent-number)
  "Return the full path to a rawfile store."
  (concat db-path patent-number "/"))

(defun gp-full-path-to-images-store (patent-number)
  "Return the full path to a images store."
  (concat db-path patent-number "/images/"))

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
(defun gp-get-patent-as-dom (patent-number)
  t)

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
  (progn
   (gp-retrieve-and-store-patent-wget patent-number)
   (gp-retrieve-and-store-patent-images-wget patent-number)
   (gp-retrieve-and-store-patent-pdf-wget patent-number)
   (gp-create-image-aliases patent-number)
   (gp-create-image-embeded-files patent-number)
  ))

(defun gp-retrieve-and-store-patent-wget (patent-number)
  "Retrieve a patent from Google patents and store it in DB-PATH as RAWFILE-NAME"
  (let ((url
	 (if (string= (substring patent-number 0 2) "EP")
	     (concat gp-url  patent-number "/en?oq=" patent-number)
	   (concat gp-url  patent-number))
	 )
	(store (gp-full-path-to-rawfile-store patent-number))
	(file  (gp-full-path-to-rawfile patent-number)))
    (unless (file-exists-p file)
      (unless (file-exists-p store) (make-directory store t))
      (call-process-shell-command
       (mapconcat #'shell-quote-argument
		  (list wget-program url "-O" file) " ")))))


(defun gp-request-and-store-patent (patent-number)
  (let* ((url (concat gp-url patent-number))
	(local-db (concat (gp-full-path-to-rawfile-store patent-number)))
	(local-file (concat local-db rawfile-name)))
    (message "%S" url)
    (message "%S %s" local-db (file-exists-p local-db))
    (unless (file-exists-p local-db) (make-directory local-db t))
    (message "%S" local-file)
    (request url
	     :parser 'buffer-string
	     :complete (function*
			(lambda (&key data &allow-other-keys)
			  (switch-to-buffer "*request-result*")
			  (insert data)
			  (goto-char (point-min))
			  (write-file local-file)
			  )))))

(defun gp-retrieve-and-store-patent (patent-number)
  (let* ((url (concat gp-url patent-number))
	(local-db (concat (gp-full-path-to-rawfile-store patent-number)))
	(local-file (concat local-db rawfile-name)))
    (message "%S" url)
    (message "%S %s" local-db (file-exists-p local-db))
    (unless (file-exists-p local-db) (make-directory local-db t))
    (message "%S" local-file)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point-min) (point))
      (write-file local-file))))

(defun gp-convert-html-to-dom (in-buffer)
  (your-sanitize-function
   (with-current-buffer in-buffer
     (while (re-search-forward "^[\s-]+$" nil t)
       (replace-match ""))
     (flush-lines "^\n")
     (libxml-parse-html-region (point-min) (point-max))
     )))

;; section group
(defun gp-get-sections (dom)
  "Extract sections and return them as a list."
  (dom-by-tag dom 'section))

(defun gp-get-each-section (dom-list section-id)
  "Get the section specified by section-id and return it as dom"
  (let (result)
    (dolist (section dom-list result)
      (if (string= (dom-attr section 'itemprop) section-id)
	  (setq result section)
	))))
  
(defun gp-get-abstract (dom)
  "Rectify abstract section"
  (gp-rectify-section dom "abstract"))

(defun gp-get-claims (dom)
  "Rectify claims section"
  (gp-rectify-section dom "claims"))

(defun gp-get-description-heading (dom)
  "Get headings and return them as a dom-list"
  (setq headings (dom-by-tag dom 'heading)))

(defun gp-get-description-paragraph (dom)
  "Get paragraphs and return them as a dom-list"
  (let (result)
    (dolist (div (dom-by-tag dom 'div) result)
      (if (string= (dom-attr div 'class) "description-paragraph")
	  (setq result (cons div result))))))

(defun gp-convert-dom-to-org (dom-list)
  (message "gp-convert-dom-org")
  (let (result)
    (dolist (dom dom-list result)
      (message (format "%s" (dom-attr dom 'num)))
      (setq result (cons (gp-convert-dom-to-org-1 dom) result)))))

(defun gp-convert-dom-to-org-1 (dom)
  (let ((paragraph-number (dom-attr dom 'num)))
    (with-temp-buffer
      (insert (format "[%s] %s" paragraph-number
		      (gp-italic-and-bold (dom-children dom))
		      ))
      (buffer-string))))

(defun gp-italic-and-bold (lst)
  (message "gp-italic-and-bold")
  (with-temp-buffer
    (dolist (item lst)
      (insert (format "%s" item)))
    (goto-char (point-min))
    (while
	(re-search-forward "(\\(\\w+\\)\s\\(\\w+\\)\s\\(.+?\\))" nil t)
      (message "%s" (match-string 0))
      (replace-match
       (cond ((string= (match-string 1) "i")   (format "/%s/"  (match-string 3)))
	     ((string= (match-string 1) "b")   (format "*%s*"  (match-string 3)))
	     ((string= (match-string 1) "sub") (format "_{%s}" (match-string 3)))
	     (t (format "%s" (match-string 0)))))
      )
    (buffer-string))
  )

(defun gp-get-description (dom)
  "Rectify description section"
  (gp-rectify-section dom "description"))

(defun gp-rectify-section (dom section-id)
  "Extract essential text and remove (br nil) tags"
  (let ((essential-tag 
	 (dolist (div (dom-by-tag dom 'div) result)
	   (if (string= (dom-attr div 'class) section-id)
	       (setq result div)))))
    (delete '(br nil) essential-tag)))

(defun gp-get-abstract-section (dom-list)
  "Return abstract section as a dom"
  (gp-get-each-section dom-list "abstract"))

(defun gp-get-description-section (dom-list)
  (gp-get-each-section dom-list "description"))

(defun gp-get-claims-section (dom-list)
  (gp-get-each-section dom-list "claims"))

(defun gp-get-metadata-section (dom-list)
  (gp-get-each-section dom-list "metadata"))

(defun gp-get-application-section (dom-list)
  (gp-get-each-section dom-list "application"))

;; link group
;(defun gp-get-link-item (patent-number link-id)
  "Get a link-item specified by link-id from a tag"
;(let ((a-list (dom-by-tag (gp-get-patent-as-dom patent-number) 'a)))
;  (cl-reduce (lambda (s a) (if (string= (dom-attr s 'itemprop) link-id) s a)) a-list :initial-value nil)))

(defun gp-retrieve-and-store-patent-pdf-wget (patent-number)
  (let* ((pdfLink (dom-attr (gp-get-link-item patent-number 'pdfLink) 'href))
	 (store   (gp-full-path-to-rawfile-store patent-number))
	 (file    (concat store (file-name-nondirectory pdfLink))))
    (unless (file-exists-p file)
      (unless (file-exists-p store) (make-directory store t))
      (call-process-shell-command
      (mapconcat #'shell-quote-argument
		  (list wget-program pdfLink "-O" file) " ")))))
    
;; metadata group
(defun gp-get-metadata-item (patent-number metadata-id)
  "Get a metadata-item specified by metadata-id from metadata"
(let ((span-list (dom-by-tag (gp-get-metadata patent-number) 'span)))
  (cl-reduce (lambda (s a) (if (string= (dom-attr s 'itemprop) metadata-id) s a)) span-list :initial-value nil)))

(defun gp-get-application-number (patent-number)
  (gp-get-metadata-item patent-number "applicationNumber"))

(defun gp-get-priority-date (patent-number)
  (gp-get-metadata-item patent-number "priorityDate"))

(defun gp-get-filing-date (patent-number)
  (gp-get-metadata-item patent-number "filingDate"))

(defun gp-get-title (patent-number)
  (gp-get-metadata-item patent-number "title"))

(defun gp-get-ifi-Status (patent-number)
  (gp-get-metadata-item patent-number "ifiStatus"))

(defun gp-get-representative-publication (patent-number)
  (gp-get-metadata-item patent-number "representativePublication"))

(defun gp-get-pripmary-language (patent-number)
  (gp-get-metadata-item patent-number "primaryLanguage"))

;; other metadata outside of meta section
(defun gp-get-inventor (patent-number)
  "Get the inventor name"
  (let ((dd-list (dom-by-tag (gp-get-patent-as-dom patent-number) 'dd)))
    (cl-reduce (lambda (s a) (if (string= (dom-attr s 'itemprop) "inventor") s a)) dd-list :initial-value nil)))


(defun gp-paragraph-renderer (dom)
  "Receive a paragraph as a dom and render it as text."
  (let (( num-string (dom-attr dom 'num)))
    (if (stringp num-string)
	(format "#+attr_html: :id %s\n#+begin_quote\n%s\n#+end_quote"
		(progn (string-match "[0-9]+" num-string) (match-string 0 num-string))
		(mapconcat 'identity (gp-paragraph-replace-tag dom) "")
		)
      (format "#+begin_quote\n%s\n#+end_quote" 
                (mapconcat 'identity (gp-paragraph-replace-tag dom) ""))
    )))

(defun gp-paragraph-replace-tag (dom)
"Replace a tag into org deccoration"
(let ((items (dom-children dom))
      (acc nil))
      (while items
            (if (listp (car items))
	        (cond ((eq (dom-tag (car items)) 'b)      
                           (setq acc (cons (format "*%s*"        (dom-text (car items))) acc)))

		      ((eq (dom-tag (car items)) 'figref) 
                           (setq acc (cons (format "[[%s:][%s]]" 
                           (replace-regexp-in-string (concat (regexp-quote "FIG.") "\\s-*\\([0-9]+\\)") "FIGREF-\\1" (dom-text (car items)))
			   (dom-text (car items))) acc)))
					   
;		      ((eq (dom-tag (car items)) 'br) 
;                           (setq acc (cons (format "\n") acc)))

		      (t  (setq acc (cons (format "%s" (car items)) acc))))
	    (setq acc (cons (format "%s" (car items)) acc)))
            (setq items (cdr items)))
	    (nreverse acc)))

(defun gp-description-renderer-1 (dom result)
  (append
   (cl-reduce 

    (lambda (acc object) 
      (cond 
       ((and (listp object) (symbolp (car object)))
	(cond 
	 ( (eq (car object) 'h2) (cons (format "* %s\n" (nth 2 object)) acc) )
	 ( (eq (car object) 'heading) (cons (format "** %s\n" (nth 2 object)) acc) )
	 ( (and (eq (car object) 'div)
		(or (string= (dom-attr object 'class) "description-line")
		    (string= (dom-attr object 'class) "description-paragraph")))
	       (cons (format "%s\n" (gp-paragraph-renderer object)) acc) )
	 ( (eq (car object) 'p) (cons (format "%s\n" (gp-paragraph-renderer object)) acc) )
	 ( t (gp-description-renderer-1 (cddr object) acc ))
	 ( t acc)
	 ))
       (t acc)))
	      
    dom :initial-value nil
    ) ;; cl-reduce
   result) ;;append
  );; defun


(defun gp-description-renderer (dom)
  (mapconcat 'identity (nreverse (gp-description-renderer-1 dom nil)) ""))


(defun gp-abstract-renderer-1 (dom result)
  (append
   (cl-reduce 

    (lambda (acc object) 
      (cond 
       ((and (listp object) (symbolp (car object)))
	(cond 
	 ( (eq (car object) 'h2) (cons (format "* %s\n" (nth 2 object)) acc) )
	 ( (eq (car object) 'heading) (cons (format "** %s\n" (nth 2 object)) acc) )

	 ( (and (eq (car object) 'div) (eq (car (car (car (cdr object)))) 'num ))
	   (cons (format "%s\n" (gp-paragraph-renderer object)) acc) )

	 ( (and (eq (car object) 'div) (string= (dom-attr object 'class) "abstract")) 
	       (cons (format "%s\n" (gp-paragraph-renderer object)) acc) )

	 ( t (gp-abstract-renderer-1 (cddr object) acc ))
;	 ( t acc)
	 ))
       (t acc)))
	      
    dom :initial-value nil
    ) ;; cl-reduce
   result) ;;append
  );; defun

(defun gp-abstract-renderer (dom)
  (mapconcat 'identity (nreverse (gp-abstract-renderer-1 dom nil)) ""))



;; imagesを取得しDBに保存
(defun gp-retrieve-and-store-patent-images-wget (patent-number)
  "Retrieve a images from Google patents and store it in DB-PATH/images"
  (let ((image-urls (gp-get-image-urls patent-number))
        (store (gp-full-path-to-images-store patent-number)))
	(unless (file-exists-p store) (make-directory store t))
	(while image-urls
              (setq file (file-name-nondirectory (car image-urls)))
;	      (async-shell-command これだとうまくいかない
              (call-process-shell-command
	              (mapconcat #'shell-quote-argument
		      (list wget-program (car image-urls) "-O" 
		      (concat store file)
		      ) " "))
		      (setq image-urls (cdr image-urls)))
		      ))

(defun gp-get-image-urls (patent-number)
     (let ((meta-list (dom-by-tag (gp-get-patent-as-dom patent-number) 'meta))
	   (acc nil))
       (while meta-list
	 (when (string= (dom-attr (car meta-list) 'itemprop) "full") (setq acc (cons (dom-attr (car meta-list) 'content)  acc)))
	 (setq meta-list (cdr meta-list)))
       acc))

;; satitize function
;; Thanks to https://qiita.com/t-suwa/items/20a4ebf37b0a57ff88b2
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


(defun gp-create-image-aliases (patent-number)
  (with-temp-buffer
    (let ((image-files (reverse (mapcar #'file-name-nondirectory (gp-get-image-urls patent-number)))))
      (while image-files
	(insert (format "#+link: %s file:./figs/%s\n"
			(progn (setq test (car image-files))
			       (if (string-match "-.*?\\([0-9]+\\)\\." test)
				   (format "FIGREF-%d" (string-to-number (match-string 1 test))) nil)
;			       (message test)
			       )
			(car image-files)))
	(setq image-files (cdr image-files)))
      (write-region (point-min) (point-max) (concat (gp-full-path-to-rawfile-store patent-number) image-aliases-name) ))))

  (defun gp-create-image-embeded-files (patent-number)
    (let ((image-files (reverse (mapcar #'file-name-nondirectory (gp-get-image-urls patent-number)))))
      (while image-files
	(with-temp-buffer
	  (message "%s" (car image-files))
	  (insert (format "#+attr_html: :style transform:rotate(0deg) :width 450px\n"))
	  (insert (format "[[%s:]]\n"
			  (progn
			    (setq test (car image-files))
			    (string-match "-.*?\\([0-9]+\\)\\." test)
			    (format "FIGREF-%d" (string-to-number (match-string 1 test))))))
	  (write-region (point-min) (point-max)
			(concat (gp-full-path-to-rawfile-store patent-number)
				(format "figref-%d.org" (string-to-number (match-string 1 test)))))	  
	  (setq image-files (cdr image-files))
	))))


(defun gp-get-figrefs (patent-number)
     (let ((figref-list (dom-by-tag (gp-get-patent-as-dom patent-number) 'figref))
           (acc  (list "FIGREF-0")))
       (while figref-list
         (setq acc (cons 
                    (replace-regexp-in-string (concat (regexp-quote "FIG.") "\\s-*\\([0-9]+\\)") "FIGREF-\\1" (dom-text (car figref-list))) acc))
	 (setq figref-list (cdr figref-list)))
	 (delete-dups acc)))


;; below functions are for utility 
;; list of doms only. if single dom is provided, the very first tag will be omitted. 
(defun scan-dom-multi (result dom-list)
  (reverse (cl-reduce #'scan-dom-single dom-list :initial-value nil)))

;; single dom only. if list of doms is provided, nil will be produced.
(defun scan-dom-single (acc dom)
  (cond ((atom dom) acc)
	((symbolp (car dom)) (setq acc (cons (dom-tag dom) acc)) 
	 (let (( has-children (scan-dom-multi nil (dom-children dom)) )) 
	   (if has-children (cons has-children acc) acc)))
	(t acc)) 
  )

(defun scan-dom (dom-list)
  (scan-dom-multi nil dom-list))

(provide 'gp-util)
