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

(defun gp-get-representatitive-publication (patent-number)
  (gp-get-metadata-item patent-number "representatitivePublication"))

(defun gp-get-pripmary-language (patent-number)
  (gp-get-metadata-item patent-number "primaryLanguage"))

;; claim group
;; ちょっとダサい．cl-reduceを用いて書き直すべきか？
(defun gp-get-claim-as-text (patent-number claim-id)
  "Get an individual claim specified by patent-number and claim-id."
  (let ((claim-list (gp-get-claim patent-number claim-id)))
    (with-temp-buffer
      (while claim-list
	(let ((claim-list-2 (car claim-list)))
	  (while claim-list-2
	    (let ((cell (car claim-list-2)))
	      (cond ((listp cell)
		     (cond ((string= (dom-tag cell) "claim-ref") (insert (dom-text cell))))))
	      (cond ((and (atom cell) (stringp cell)) (insert cell))))
	    (setq claim-list-2 (cdr claim-list-2))))
	(setq claim-list (cdr claim-list)))
      (buffer-string))))

(defun gp-get-claim (patent-number claim-id)
  "Get a claim specified by claim-id from dom"
  ;;  (nth 0 (dom-by-class (gp-get-claim-1 patent-number claim-id) "claim-text")))
  (dom-by-class (gp-get-claim-1 patent-number claim-id) "claim-text"))  

(defun gp-get-claim-1 (patent-number claim-id)
  "Get a claim specified by claim-id from dom"
(let ((div-list (dom-by-tag (gp-get-claims patent-number) 'div)))
  (cl-reduce (lambda (d a) (if (string= (dom-attr d 'id) (format "CLM-%05d" claim-id)) d a)) div-list :initial-value nil)))

;; section group
(defun gp-get-section (patent-number section-id)
  "Get a section specified by section-id from dom"
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

(defun gp-get-metadata (patent-number)
  (gp-get-section patent-number "metadata")
  )

(defun gp-get-family (patent-number)
  (gp-get-section patent-number "family")
  )

(defun gp-get-application (patent-number)
  (gp-get-section patent-number "application")
  )

(defun gp-paragraph-renderer (dom)
  "Receive a paragraph as a dom and render it as text."
  (let (( num-string (dom-attr dom 'num)))
    ;;    (format "#+begin_quote\n[%s]\n%s\n#+end_quote"
    ;;    (progn (string-match "[0-9]+" num-string) (match-string 0 num-string)) (dom-text dom))
    (format "#+begin_quote\n[%s]\n%s\n#+end_quote"
	    (progn (string-match "[0-9]+" num-string) (match-string 0 num-string)) (dom-children dom))    
    ))

(defun gp-description-renderer-1 (dom result)
  (append
   (nreverse (cl-reduce 

	      (lambda (acc object) 
		(cond 
		 ((and (listp object) (symbolp (car object)))
		  (cond 
		   ( (eq (car object) 'h2) (cons (format "** %s\n" (nth 2 object)) acc) )
		   ( (eq (car object) 'heading) (cons (format "*** %s\n" (nth 2 object)) acc) )
		   ( (eq (car object) 'p) (cons (format "%s\n" (gp-paragraph-renderer object)) acc) )
		   ( (eq (car object) 'description-of-drawings) (gp-description-renderer-1 (cddr object) acc) )
		   ( (listp (cddr object)) (gp-description-renderer-1 (cddr object) acc))
		   ( t acc) ))
		 (t acc) ))
	      
	      dom :initial-value nil
	      ) ;; cl-reduce
	     ) ;;nreverse
   result) ;;append
  );; defun

(defun gp-description-renderer (dom)
  (mapconcat 'identity (gp-description-renderer-1 dom nil) ""))

;; satitize function
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
