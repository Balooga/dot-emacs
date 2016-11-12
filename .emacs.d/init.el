;;; package --- Summary
;;; Commentary:

;; #+AUTHOR:    Luke Crook
;; #+EMAIL:     ljcrook@directv.com
;; #+TITLE:     Luke's .emacs file
;; #+DATE:      2015-02-30

;;; Code:

;;; http://orgmode.org/worg/org-contrib/babel/intro.html
;;; init.el --- Where all the magic begins
;;

;; Dependencies for Windows
;;
;; 1) Cygwin
;; 2) Cygwin - Python (Python->Python)
;; 3) Cygwin - emacs-w32 (Editors->Emacs)
;; 4) Cygwin - Hunspell (Text->Hunspell)
;; 5) Cygwin - Hunspell-en (Text->Hunspell)
;; 6) Cygwin - curl (Net->Curl)
;; 7) Cygwin - fortune-mod (Games->fortune-mod)
;; 8) Cygwin - graphviz (Graphics->Graphviz)
;; 9) Cygwin - git (Devel->git)
;; 10) Cygwin - python-sphinx (Python->python-sphinx)
;; 11) Cygwin - 

;; Set the HOME directory to %USERPROFILE% in Windows
;; ;; clone dot-emacs into HOM

;; mklink /j .emacs.d dot-emacs

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org-archive" . "http://orgmode.org/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish) ;;https://github.com/myrjola/diminish.el
(setq use-package-verbose t) ;;Set to t for performance profiling

(defvar load-org-emacs-config 'el)
(cond
 ((and (eq load-org-emacs-config 'org)
       (file-exists-p (expand-file-name "emacs.org" user-emacs-directory)) )
  (org-babel-load-file (expand-file-name "emacs.org" user-emacs-directory)))

 ((and (eq load-org-emacs-config 'el)
       (file-exists-p (expand-file-name "emacs.el" user-emacs-directory)))
  (load (expand-file-name "emacs.el" user-emacs-directory) 'noerror)))

;;; init.el ends here
