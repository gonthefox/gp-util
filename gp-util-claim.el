;;; gp-util-claim.el

(defun gp-get-claim (patent-number claim-id)
  "Get a claim specified by CLAIM-ID as string from dom"
;;  (dom-by-class (gp-get-claim-1 patent-number claim-id) "claim-text"))
  (dom-children (gp-get-claim-1 patent-number claim-id)))

(defun gp-get-claim-1 (patent-number claim-id)
  "Get a claim specified by claim-id from dom"
  (let ((div-list (dom-by-tag (gp-get-claims patent-number) 'div)))
    (cl-reduce (lambda (d a) (if (string= (dom-attr d 'id) claim-id) d a)) div-list :initial-value nil)))


(defun gp-claim-text-renderer (dom)
  "Receive claims as a dom and render it as text."
  (format "#+begin_quote\n%s\n#+end_quote\n"
	  (replace-regexp-in-string "^\\([0-9]+\\)\.\\s-*\\(\\S-+\\)" "Claim \\1. \\2" (mapconcat 'identity (nreverse (gp-claim-text-renderer-1 dom nil)) ""))
;;	  (replace-regexp-in-string "^\\([0-9]+\\)\.\\s-*\\(\\S-+\\)" "Cl.\\1. [@\\1] \\2" (mapconcat 'identity (nreverse (gp-claim-text-renderer-1 dom nil)) ""));;	  (mapconcat 'identity (nreverse (gp-claim-text-renderer-1 dom nil)) "")
	  ))

;;(claims-renderer (gp-get-claims patent-number))
;; domはcl-reduceに渡される．そのとき各要素nodeがdomとして処理できるように，domを含むリストでなければならない  
(defun gp-claim-text-renderer-1 (dom result)
  (append (cl-reduce (lambda (acc object)
		       (cond ((atom object) (cons (format "%s" object) acc))
			     ((listp object) 
			      (cond 
			       ((consp (car object)) acc)
		    ((and (eq (dom-tag object) 'div) (string= (dom-attr object 'class) "claim"))      
		     (gp-claim-text-renderer-1 (dom-children object) acc ))
		    ((and (eq (dom-tag object) 'div) (string= (dom-attr object 'class) "claim-text")) 
		                                     (gp-claim-text-renderer-1 (dom-children object) acc ))
                    ((eq (dom-tag object) 'claim-ref) 
;;		     (setq acc (cons (format "[[%s][%s]]" (dom-attr object 'idref) (dom-text object)) acc)))
		     (setq acc (cons (format "%s" (dom-text object)) acc)))		    
		    
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
	      ((eq (dom-tag object) 'h2)              (cons (format "* %s\n"  (gp-claims-count object)  ) acc)) 		    
	      ((eq (dom-tag object) 'claim-statement) (gp-claims-renderer-1 (dom-children object) (cons (format "%s\n" (dom-text object)) acc)))

	      ((and (eq (dom-tag object) 'div) (assoc 'id (dom-attributes object)))
	       (let  ((claim-id (dom-attr object 'id)))
		 (setq acc (cons (format "** <<%s>> Claim %s\n" claim-id (gp-get-claim-id claim-id) ) acc))		    
		 (cons (gp-claim-text-renderer (dom-children object)) acc)))
	      ((eq (dom-tag object) 'div)             (gp-claims-renderer-1 (dom-children object) acc))
	      (t acc)))))
		    
    dom :initial-value nil
    ) ;; cl-reduce
   result) ;;append
  );; defun

(defun gp-get-claim-id (text)
  (string-match "\\([0-9]+\\)" text)
  (int-to-string (string-to-number (match-string 1 text))))

(defun gp-claims-count (dom)
  (let ((children (dom-children dom))
	(header (dom-text dom)))
    (format "%s (%s)"
	    (progn (string-match "[a-zA-Z]+" header) (match-string 0 header))
	    (when (string= (dom-attr (nth 1 children) 'itemprop) "count") (dom-text (nth 1 children))))))

(defun gp-claims-renderer (dom)
  (mapconcat 'identity (nreverse (gp-claims-renderer-1 dom nil)) ""))

;; rendering a claim tree
(defun gp-claim-tree-renderer-asterisk-1 (claim-tree)
  (when (= (length depth) 1)
    (insert (format "%s Independent claim %s\n" (mapconcat 'identity depth "") (gp-get-claim-id (car claim-tree)))))
  (insert (format "#+name: %s\n" (gp-get-claim-id (car claim-tree))))
  (insert (gp-claim-text-renderer (gp-get-claim patent-number (car claim-tree))))
  (push "*" depth)
  (dolist (elt (cdr claim-tree))
    (if (listp elt) (gp-claim-tree-renderer-asterisk-1 elt)
      (progn
;;	(insert (format "%s Claim %s\n" (mapconcat 'identity depth "") (gp-get-claim-id elt)))
	(insert (format "#+name: %s\n" (gp-get-claim-id elt)))
	(insert (gp-claim-text-renderer (gp-get-claim patent-number elt))))))
  (pop depth))
			       
(defun gp-claim-tree-renderer-asterisk (claim-tree)
   (with-temp-buffer
     (let ((depth '("--")))
       (dolist (elt claim-tree)
          (gp-claim-tree-renderer-asterisk-1 elt)))
	  (buffer-string)))

;; below functions are for utility 
;; list of doms only. if single dom is provided, the very first tag will be omitted. 
(defun gp-make-claim-pairs-multi (result dom-list)
  (append (reverse (cl-reduce #'gp-make-claim-pairs-single dom-list :initial-value nil)) result))

;; single dom only. if list of doms is provided, nil will be produced.
(defun gp-make-claim-pairs-single (acc dom)
  (cond ((atom dom) acc)
	((symbolp (car dom))
	 (cond ((and (eq (dom-tag dom) 'div) (assoc 'id (dom-attributes dom)))
		(cons (cons (dom-attr dom 'id) (mapcar #'(lambda (dom) (dom-attr dom 'idref))(dom-by-tag dom 'claim-ref))) acc))
	       ((gp-make-claim-pairs-multi acc (dom-children dom)))))
	(t acc)) 
  )

(defun gp-make-claim-pairs-1 (dom-list)
  (gp-make-claim-pairs-multi nil dom-list))

(defun gp-make-claim-pairs (patent-number)
  (gp-make-claim-pairs-1 (gp-get-claims patent-number)))

(defun gp-independent-claims (claim-pairs)
  (let ((indeps nil))
    (dolist (claim-pair claim-pairs)
      (when (null (cdr claim-pair)) (setq indeps (cons (cons (car claim-pair) (list "claims")) indeps))))
    (reverse indeps)))


(provide 'gp-util-claim-tree)
