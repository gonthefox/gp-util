;;; gp-util-if.el --- Google Patent utilities

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

(add-to-list 'load-path "~/github/gp-util/" )

(defcustom db-path "/var/db/patent/"
  "full path to the directory where rawfiles will be stored.")

(defcustom rawfile-name "raw.el"
  "filename for the raw files.")


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


(defun gp-save-patent-as-dom (patent-number)
  (message "gp-save-patent-as-dom")
  (setq local-file (concat db-path patent-number "/" rawfile-name))
  (with-temp-buffer
    (insert (gp-get-patent-as-dom patent-number))
    (write-file local-file)))

(defun gp-get-patent-as-dom (patent-number)
  (message "gp-get-patent-as-dom")  
  (pp (gp-get-document-as-dom (concat gp-url patent-number))))

(defun gp-get-document-as-dom (url)
  (message "gp-get-document-as-dom")
  (gp-convert-html-to-dom
   (with-temp-buffer
     (insert (gp-get-document url))
     (buffer-substring-no-properties (point-min) (point-max)))))

(defun gp-get-document (url)
  "Retrieve patent publication from Google patents"
  (message "gp-get-document")
  (with-current-buffer (url-retrieve-synchronously url)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun gp-convert-html-to-dom (STRING)
  ;; Convert a html string to a dom.
  (message "gp-convert-html-to-dom")
  (with-temp-buffer 
    (insert STRING)
;;    (insert "<html><title>test</title></html>")
    (libxml-parse-html-region (point-min) (point-max))))


(provide 'gp-util-if)
