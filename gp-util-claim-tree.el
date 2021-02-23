(defun gp-claim-ref-detector (dom)
  (let ((acc nil))
    (cl-reduce (lambda (acc object)
		 (cond ((atom object) acc)
		       ((listp object) 
			(cond 
			 ((consp (car object)) acc)
			 ((eq (dom-tag object) 'claim-ref) (cons (dom-attr object 'idref) acc))
			 ((eq (dom-tag object) 'div)       (gp-claim-ref-detector (dom-children object)))
			 (t acc)))))

	       dom :initial-value nil)))

(defun gp-claims-analyzer (dom result)
  
  (append
   (cl-reduce 
    
    (lambda (acc object) 
      (cond ((atom object) acc)
	    ((listp object) 
	     (cond 
	      ((consp (car object)) acc)
	      ((and (eq (dom-tag object) 'div) (assoc 'id (dom-attributes object)))
	       (let  ((claim-id (dom-attr object 'id)))
		 (cons (cons claim-id (gp-claim-ref-detector (dom-children object))) acc)))
	      ((eq (dom-tag object) 'div)      (gp-claims-analyzer (dom-children object) acc))
	      (t acc)))))

    dom :initial-value nil
    ) ;; cl-reduce
   result) ;;append
  );; defun

(defun gp-get-claim-id (text)
  (string-match "\\([0-9]+\\)" text)
  (int-to-string (string-to-number (match-string 1 text))))

(defun gp-independent-claims (claim-pairs)
  (let ((indeps nil))
    (while claim-pairs
      (when (null (cdr (car claim-pairs))) (setq indeps (cons (car claim-pairs) indeps)))
      (setq claim-pairs (cdr claim-pairs))) 
    indeps))


(defun gp-make-claim-pairs (patent-number)
  (gp-claims-analyzer (gp-get-claims patent-number) nil))

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
       (insert (format "%s %s\n" (mapconcat 'identity depth "") (car claim-tree)))
       (push "*" depth)
       (dolist (elt (cdr claim-tree))
               (if (listp elt) (gp-claim-tree-renderer-asterisk-1 elt)
                               (insert (format "%s %s\n" (mapconcat 'identity depth "") elt))))
			       (pop depth)
			       )
(defun gp-claim-tree-renderer-asterisk (claim-tree)
   (with-temp-buffer
     (let ((depth '("*")))
       (dolist (elt claim-tree)
          (gp-claim-tree-renderer-1 elt)))
	  (buffer-string)))

(provide 'gp-util-claim-tree)
