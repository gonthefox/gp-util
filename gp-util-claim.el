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
	  (replace-regexp-in-string "^\\([0-9]+\\)\.\\s-*\\(\\S-+\\)" "\\1. [@\\1] \\2" (mapconcat 'identity (nreverse (gp-claim-text-renderer-1 dom nil)) ""))
;;	  (mapconcat 'identity (nreverse (gp-claim-text-renderer-1 dom nil)) "")
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


(defun gp-make-claim-pairs-1 (dom result)

  (append
   (cl-reduce 
    (lambda (acc object)
      (if (listp object) 
	  (cond 
	   ((and (eq (dom-tag object) 'div) (assoc 'id (dom-attributes object)))
	    (cons
	     (cons (dom-attr object 'id)
		   (mapcar #'(lambda (object) (dom-attr object 'idref))
			   (dom-by-tag object 'claim-ref))
		   )
	     acc))
	   ((eq (dom-tag object) 'div) (gp-make-claim-pairs-1 object acc))
	   (t acc))
	acc))

    dom :initial-value nil
    ) ;; cl-reduce
   result) ;;append
  );; defun

(defun gp-independent-claims (claim-pairs)
  (let ((indeps nil))
    (while claim-pairs
      (when (null (cdr (car claim-pairs))) (setq indeps (cons (car claim-pairs) indeps)))
      (setq claim-pairs (cdr claim-pairs))) 
    indeps))

(defun gp-make-claim-pairs (patent-number)
  (gp-make-claim-pairs-1 (gp-get-claims patent-number) nil))

(defun gp-make-claim-tree-1 (claim-pairs indep-claim-pair)
  (let ((acc nil))
    (dolist (claim-pair claim-pairs) 
      (when (equal (cdr claim-pair) indep-claim-pair) 
	(setq acc (cons (gp-make-claim-tree-1 claim-pairs (list (car claim-pair))) acc))))
    (if (null acc) (car indep-claim-pair) (cons (car indep-claim-pair) acc))))

(defun gp-make-claim-tree (patent-number)
  (let* ((claim-pairs (gp-make-claim-pairs patent-number))
	 (indeps (gp-independent-claims claim-pairs))
	 (acc nil))
    (dolist (indep indeps)
      (setq acc (cons (gp-make-claim-tree-1 claim-pairs indep) acc)))
    (reverse acc)  ))

;; rendering a claim tree
(defun gp-claim-tree-renderer-asterisk-1 (claim-tree)
  (insert (format "%s claim %s\n" (mapconcat 'identity depth "") (gp-get-claim-id (car claim-tree))))
  (insert (gp-claim-text-renderer (gp-get-claim patent-number (car claim-tree))))
  (push "*" depth)
  (dolist (elt (cdr claim-tree))
    (if (listp elt) (gp-claim-tree-renderer-asterisk-1 elt)
      (progn
	(insert (format "%s claim %s\n" (mapconcat 'identity depth "") (gp-get-claim-id elt)))
	(insert (format "#+name: %s\n" elt))
	(insert (gp-claim-text-renderer (gp-get-claim patent-number elt))))))
  (pop depth))
			       
(defun gp-claim-tree-renderer-asterisk (claim-tree)
   (with-temp-buffer
     (let ((depth '("**")))
       (dolist (elt claim-tree)
          (gp-claim-tree-renderer-asterisk-1 elt)))
	  (buffer-string)))

(provide 'gp-util-claim-tree)
