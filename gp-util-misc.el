;; To take two multi-line strings and diff them word-by-word
(defun wdiff (a-region b-region)
  (with-temp-buffer
    (insert a-region)
    (write-region (point-min) (point-max) "*a-region*"))
  (with-temp-buffer
    (insert b-region)
    (write-region (point-min) (point-max) "*b-region*"))

  (shell-command-to-string
   (mapconcat #'identity
	      (list "wdiff" "*a-region*" "*b-region*" "-n -w ' +' -x '+' -y ' _' -z '_ '")
	      " ")))

 (defun gp-make-claim-table (col2 col3)
      (let ((col2s (split-string col2 "\n"))
	    (col3s (split-string col3 "\n")))
	(with-temp-buffer
	  (insert "#+attr_html: :id claim\n")
	  (insert "#+BEGIN_QUOTE\n")
	  (insert "|REF| claim language|参考訳|\n")
	  (insert "||<40>|<40>|\n")
	  (insert "|-\n")
	  (dolist (col2si col2s)
	    (setq col3si (car col3s))
	    (insert (concat (replace-regexp-in-string "\n" "" (format "| %s | %s | %s|" " " col2si col3si)) "\n"))
	    (setq col3s (cdr col3s))
	    )
	  (insert "#+END_QUOTE\n")
	  (buffer-string))))
