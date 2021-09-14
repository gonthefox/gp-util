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

(defun gp-make-claim-table (col1 col2 col3)
  (let (
	(col1s (split-string col1 "\\\\"))
	(col2s (split-string col2 "\\\\"))
	(col3s (split-string col3 "\\\\"))
	)
    (with-temp-buffer
      (insert "#+attr_html: :id claim\n")
      (insert "#+BEGIN_QUOTE\n")
      (insert "|REF| claim language|参考訳|\n")
      (insert "||<40>|<40>|\n"))
      (insert "|-\n")
      (dolist (col2si col2s)
	(setq col3si (car col3s))
	(setq col1si (car col1s))
	(insert (concat (replace-regexp-in-string "\n" "" (format "| %s | %s | %s|" col1si col2si col3si)) "\n"))
	(setq col3s (cdr col3s))
	(setq col1s (cdr col1s)))	    
      (insert "#+END_QUOTE\n")
      (buffer-string)))


(defun gp-make-claim-table-single (col)
  (let (
	(cols (split-string col "\\\\"))
	)
    (with-temp-buffer
      (insert "#+attr_html: :id claim\n")
      (insert "#+BEGIN_QUOTE\n")
      (insert "|REF| claim language|\n")
      (insert "||<40>|\n"))
      (insert "|-\n")
      (dolist (colsi cols)
	(insert (concat (replace-regexp-in-string "\n" "" (format "| %s | %s |" " " colsi)) "\n"))
	(setq cols (cdr cols)))	    
      (insert "#+END_QUOTE\n")
      (buffer-string)))

(setq description-template "~/github/gp-util/description.org")
(defun gp-make-description ()
  (interactive)
  (insert-file-contents description-template))


(defun gp-make-image-aliases-config ()
  (interactive)
  (with-temp-file "./image-aliases.inc"
    (dolist (item (directory-files "./figs/"))
      (if (string-match "png$" item)
	  (insert
	   (format "#+link: FIGREF-%s file:./figs/%s\n"
		   (progn
		     (string-match "-\\([0-9a-z]+\\).png" item)
		     (upcase (match-string 1 item))  ) item))))
    (buffer-string)))


(defun gp-make-image-embeded-files ()
  (interactive)
  (dolist (item (directory-files "./figs/"))
    (if (string-match "png$" item)
	(with-temp-buffer
	  (progn 
	    (insert (format "#+attr_html: :style transform:rotate(0deg) :width 450px\n"))
	    (insert (format "[[%s:]]\n"
			    (progn
			      (string-match "-\\([0-9a-z]+\\).png" item)
			      (format "FIGREF-%s" (upcase (match-string 1 item))))))
	    (write-region (point-min) (point-max)
			  (format "./figref-%s.org" (match-string 1 item))))))))

(defun gp-figure-config-on-the-spot ()
  (interactive)
  (gp-make-image-aliases-config)
  (gp-make-iamge-embedded-files)
  )
