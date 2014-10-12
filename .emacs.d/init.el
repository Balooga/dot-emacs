;;; http://orgmode.org/worg/org-contrib/babel/intro.html
;;; init.el --- Where all the magic begins
;;
(defvar dotfiles-dir (file-name-directory (or (buffer-file-name)
					      load-file-name)))

(defvar org-mode-path-name "~/site/org-mode")

;; Set the directory for the latest version of org-mode
(defvar org-dir (expand-file-name org-mode-path-name ""))
(defvar org/lisp-dir (expand-file-name "lisp" org-dir))
(defvar org/contrib-dir (expand-file-name "contrib/lisp" org-dir))
(add-to-list 'load-path org/lisp-dir)
(add-to-list 'load-path org/contrib-dir t)

(require 'org)
(require 'org-install)
(require 'ob-tangle)
(require 'org-id)

;; Load up all literate org-mode files in this directory
;; In other words, load emacs.org
(mapc #'org-babel-load-file 
      (list (expand-file-name "emacs.org" dotfiles-dir)))

;;; init.el ends here
