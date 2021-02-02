;;; gp-util.el --- Google Patent utilities

;;  Auther:  Katsuhito Ishida <katsuhito.ishida@gmail.com>
;;  Version: 0.1
;;; Commentary:
;;  This package provides a set of useful APIs for patent analysis.
;; 
;; -*- coding: utf-8 -*-
;; (setq patent-number "JP2003085659A")
;;

(require 'dom)
(require 'request)

(defcustom db-path "/var/db/patent/"
  "full path to the directory where rawfiles will be stored.")

(defcustom rawfile-name "raw.html"
  "filename for the raw files.")

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
  (gp-retrieve-and-store-patent-wget patent-number)
  (gp-retrieve-and-store-patent-images-wget patent-number)
  (gp-create-image-aliases patent-number))

(defun gp-retrieve-and-store-patent-wget (patent-number)
  "Retrieve a patent from Google patents and store it in DB-PATH as RAWFILE-NAME"
  (let ((url  (concat gp-url  patent-number))
	(store (gp-full-path-to-rawfile-store patent-number))
	(file  (gp-full-path-to-rawfile patent-number)))
    (unless (file-exists-p file)
      (unless (file-exists-p store) (make-directory store t))
;      (async-shell-command
      (call-process-shell-command
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

(defun gp-get-representative-publication (patent-number)
  (gp-get-metadata-item patent-number "representativePublication"))

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

;; other metadata outside of meta section
(defun gp-get-inventor (patent-number)
  "Get the inventor name"
  (let ((dd-list (dom-by-tag (gp-get-patent-as-dom patent-number) 'dd)))
    (cl-reduce (lambda (s a) (if (string= (dom-attr s 'itemprop) "inventor") s a)) dd-list :initial-value nil)))

(defun gp-paragraph-renderer (dom)
  "Receive a paragraph as a dom and render it as text."
  (let (( num-string (dom-attr dom 'num)))
    (if (stringp num-string)
	(format "#+begin_quote\n[%s]\n%s\n#+end_quote"
		(progn (string-match "[0-9]+" num-string) (match-string 0 num-string))
		(gp-paragraph-replace-tag dom)
		)
      (format "#+begin_quote\n%s\n#+end_quote" (gp-paragraph-replace-tag dom)))
    ))

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
	 ( (and (eq (car object) 'div) (string= (dom-attr object 'class) "description-line")) 
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


(defun gp-claim-text-renderer (dom)
    "Receive claims as a dom and render it as text."
      (format "#+begin_quote\n%s\n#+end_quote\n"
	      (nreverse (gp-claim-text-renderer-1 dom nil))
	      )
      )

;;(claims-renderer (gp-get-claims patent-number))
;; domはcl-reduceに渡される．そのとき各要素nodeがdomとして処理できるように，domを含むリストでなければならない  
  (defun gp-claim-text-renderer-1 (dom result)
    (append (cl-reduce (lambda (acc object)
        (cond ((atom object) acc)
	      ((listp object) 
	      (cond 
                    ((consp (car object)) acc)
		    ((and (eq (dom-tag object) 'div) (string= (dom-attr object 'class) "claim"))      
                                                     (gp-claim-text-renderer-1 (dom-children object) acc ))
		    ((and (eq (dom-tag object) 'div) (string= (dom-attr object 'class) "claim-text")) 
		                                     (gp-claim-text-renderer-1 (dom-children object) (cons (format "%s\n" (dom-texts object)) acc)))
		    ((eq (dom-tag object) 'div)             (gp-claim-text-renderer-1 (dom-children object) acc))
		    (t acc)))))

		    dom :initial-value nil
		       )
	    result))


  (defun gp-claims-renderer-1 (dom result)

    (append
     (cl-reduce 

      (lambda (acc object) 
        (cond ((atom object) acc)
	      ((listp object) 
	      (cond 
                    ((consp (car object)) acc)
                    ((eq (dom-tag object) 'h2)              (cons (format "* %s\n"  (dom-children object)) acc)) 
		    ((eq (dom-tag object) 'claim-statement) (gp-claims-renderer-1 (dom-children object) (cons (format "%s\n" (dom-children object)) acc)))
		    ((and (eq (dom-tag object) 'div) (string= (dom-attr object 'class) "claim")) (cons (gp-claim-text-renderer (dom-children object)) acc))
		    ((eq (dom-tag object) 'div)             (gp-claims-renderer-1 (dom-children object) acc))
		    (t acc)))))
		    
       dom :initial-value nil
      ) ;; cl-reduce
     result) ;;append
    );; defun
   

(defun gp-claims-renderer (dom)
  (mapconcat 'identity (nreverse (gp-claims-renderer-1 dom nil)) ""))

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
  (let ((image-urls (gp-get-image-urls patent-number))
        (figrefs    (gp-get-figrefs patent-number))
        (acc nil))
     (while image-urls
            (setq acc (cons (file-name-nondirectory (car image-urls)) acc))
	    (setq image-urls (cdr image-urls)))
     (while acc
            (insert (format "#+link: %s file:./figs/%s\n" (car figrefs) (car acc)))
	    (setq figrefs (cdr figrefs))
	    (setq acc (cdr acc)))
     (write-region (point-min) (point-max) "./image-aliases.org"))))

(defun gp-get-figrefs (patent-number)
     (let ((figref-list (dom-by-tag (gp-get-patent-as-dom patent-number) 'figref))
           (acc  nil))
       (while figref-list
         (setq acc (cons 
                    (replace-regexp-in-string (concat (regexp-quote "FIG.") "\\s-*\\([0-9]+\\)") "FIGREF-\\1" (dom-text (car figref-list))) acc))
	 (setq figref-list (cdr figref-list)))
	 (setq acc (cons "FIGREF-0" acc))
	 (delete-dups acc)))

(defun gp-import-figs-from-db (patent-number)
      (let ((image-store   (gp-full-path-to-images-store patent-number))
            (rawfile-store (gp-full-path-to-rawfile-store patent-number)))
	    (unless (file-exists-p local-image-store) 
	      (copy-directory image-store local-image-store))))

;; gp-print-specification
(defun gp-print-specification (patent-number)
  "Print specification as HTML"
  (gp-import-figs-from-db patent-number)
  (with-temp-buffer
    (insert (format "#+html: <h1 style=\"text-align: center;\">%s</h1>\n"
		    (replace-regexp-in-string "\\s-+$" "" (dom-text (gp-get-title patent-number)))))
    (insert (format "#+html: <h2 style=\"text-align: center;\">%s</h2>\n" (dom-text (gp-get-representative-publication patent-number))))
    (insert (format "#+author: %s\n" (nth 2 (gp-get-inventor patent-number))))
    (insert (format "#+date: %s\n" (nth 2 (gp-get-filing-date patent-number))))
    (insert (format "#+options: toc:nil\n"))
    (insert (format "#+TOC: headlines 2\n"))
    (insert (format "#+include: \"image-aliases.org\" \n"))
    (insert (gp-abstract-renderer (gp-get-abstract patent-number)))
    (insert (gp-description-renderer (gp-get-description patent-number)))
    (insert (gp-claims-renderer (gp-get-claims patent-number)))        
    (buffer-string)))


(provide 'gp-util)
