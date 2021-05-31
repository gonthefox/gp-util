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

