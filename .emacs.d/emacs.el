;;; package --- Summary
;;; Commentary:

;;; Code:

;; Use "M-x emacs-init-time" to return the time taken to initialize Emacs

(with-no-warnings
  (require 'cl))
(require 'browse-url) ;;http://www.emacswiki.org/emacs/BrowseUrl

(setq-default
 ;;Always load newest byte code
 load-prefer-newer t
 ;;GC every 50MB, instead of default the 0.76MB
 gc-cons-threshold 50000000)

;; Determine the system type 
(defvar mswindows-p (or (eq 'windows-nt system-type)
			(eq 'windows system-type)
			(eq 'cygwin system-type)))
(defvar win32-p (or (eq 'windows-nt system-type)
                    (eq 'windows system-type)))
(defvar cygwin-p (eq 'cygwin system-type))
(defvar osx-p (eq 'darwin system-type))
(defvar gnu/linux-p (eq 'gnu/linux system-type))

(defvar current-user (getenv (if win32-p "USERNAME" "USER")))
(defvar use-home
  (getenv "HOME")
  ;;(expand-file-name "~")
  "Users home directory.")

(defvar dotfiles-dir
  user-emacs-directory
  ;;(file-name-directory (or (buffer-file-name) load-file-name))
  "Path to the .emacs.d directory.")

(defvar personal-dir (expand-file-name "personal" dotfiles-dir)
  "This directory is for your personal configuration.
All Emacs Lisp files there are loaded automatically.")

(defvar dotfiles/win32-bin-dir (expand-file-name "win32-bin" dotfiles-dir)
  "This directory contains various binaries.")

(defvar dotfiles/site-dir (expand-file-name "site" dotfiles-dir)
  "Contains packages or files not available from a package archive site.")

;;(setq default-directory use-home)

;; ;;Add all subdirectories under ~/.emacs.d/site/ to the load path
;; ;;https://www.emacswiki.org/emacs/LoadPath
;; (let ((default-directory dotfiles/site-dir))
;;   (normal-top-level-add-to-load-path '("."))
;;   (normal-top-level-add-subdirs-to-load-path))

;;Use the plantuml jar file specified by the env variable when set
;;otherwise use the version included
(defvar plantuml-jar-path (if (getenv "PLANTUML_JAR")
                              (getenv "PLANTUML_JAR")
                            (if cygwin-p
                                (shell-command-to-string (concat
                                                          "`cygpath -m "
                                                          (expand-file-name "plantuml.jar" dotfiles/site-dir)
                                                          "`"))
                              (expand-file-name "plantuml.jar" dotfiles/site-dir))))

(unless (getenv "PLANTUML_JAR")
  (setenv "PLANTUML_JAR" plantuml-jar-path))

(when win32-p
  (add-to-list 'load-path dotfiles/win32-bin-dir 'append)
  
  ;;Add MS Windows paths to exec-path
  ;;Note the two ways of specifying the path;  setenv, and add-to-list
  ;;setenv must be used when emacs calls an external applications
  (setenv "PATH" (concat (getenv "PATH")
                         ";C:\\Program Files (x86)\\Java\\jre6\\bin"))
  (add-to-list 'exec-path "C:/Windows/system32" t)
  (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin" t)
  ;; For whatever reason Windows sometimes forgets
  ;; the path to the Java executable.  Setting the path explicitly
  ;; seems to fix the problem.
  (add-to-list 'exec-path "C:/Program Files (x86)/Java/jre6/bin" t)

  (add-to-list 'exec-path (expand-file-name "win32-bin" (getenv "EMACS_DIR")) t)
  (add-to-list 'exec-path (expand-file-name "zip" dotfiles/win32-bin-dir) t)
  (add-to-list 'exec-path (expand-file-name "libxml2/bin" dotfiles/win32-bin-dir) t))

(when (and nil osx-p)
  ;;http://stackoverflow.com/questions/15752083/prevent-java-app-name-from-appearing-in-menu-bar-and-process-dock-on-mac/17951720#17951720
  ;;Prevent java app name from appearing in menu bar and process dock on Mac
  ;;Java prints out "Picked up JAVA_TOOL_OPTIONS: -Dapple.awt.UIElement=true",
  ;;so this does not work.
  (setenv "JAVA_TOOL_OPTIONS"
          "-Djava.awt.headless=true"
          ;;"-Dapple.awt.UIElement=true"
          )
  (when osx-p
    (setenv "JVM_OPTS" "-Dapple.awt.UIElement=true")))

;; Set UTF-8 as the default encoding for everything,
;; when not in terminal/tty mode.
(when (display-graphic-p)
  (setq-default locale-coding-system 'utf-8
                org-export-coding-system 'utf-8
                default-process-coding-system '(utf-8-unix . utf-8-unix))
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8) 
  (prefer-coding-system 'utf-8)
  (set-charset-priority 'unicode))

;; Mouse Events
;; Disable mouse-2 event that was appending text into documents
(global-set-key [mouse-2] nil)

;;(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
;;(global-set-key "\C-x\ b" 'switch-to-buffer)

;; (global-set-key [(control x) (p)] #'(lambda ()
;;                                     (interactive)
;;                                     (other-window -1)))

;;replace y-e-s by y
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default
 ;;confirm kill emacs
 confirm-kill-emacs 'y-or-n-p

 ;; Well, it's more so that you know this option
 kill-ring-max 120

 ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Killing-by-Lines.html
 kill-whole-line t

 ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Kill-Options.html
 kill-read-only-ok t

 ;;Sentences
 ;;http://www.gnu.org/software/emacs/manual/html_node/emacs/Sentences.html
 ;;http://www.heracliteanriver.com/?p=324
 sentence-end-double-space nil ;;Single space ends sentences :(

 ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Customize-Save.html
 require-final-newline t

 ;; Normally, C-n on the last line of a buffer appends a newline to it.
 ;; If the variable next-line-add-newlines is nil, then C-n gets an error instead.
 ;; (like C-p on the first line)
 next-line-add-newlines nil

 ;; Use space for tabulation
 indent-tabs-mode nil

 ;; Use Tab to Indent or Complete
 ;; http://emacsredux.com/blog/2016/01/31/use-tab-to-indent-or-complete/
 tab-always-indent 'complete

 ;; Sets gap between lines
 line-spacing 1

 ;; ;;Smooth keyboard scrolling
 ;; (setq
 ;;  redisplay-dont-pause t                 ;; scrolling
 ;;  scroll-step 1                          ;; scroll line per line (1 instead of 3)
 ;;  scroll-margin 0                        ;; do smooth scrolling, ...
 ;;  scroll-conservatively 100000           ;; ... the defaults ...
 ;;  scroll-up-aggressively 0               ;; ... are very ...
 ;;  scroll-down-aggressively 0             ;; ... annoying
 ;;  auto-window-vscroll nil)

 ;; Number of lines of continuity when scrolling by screenfuls.
 next-screen-context-lines 3

 ;; Scrolling done right
 redisplay-dont-pause t
 scroll-error-top-bottom t
 scroll-step 1
 scroll-conservatively 10000
 scroll-margin 1
 scroll-preserve-screen-position 1)

;; Mode line settings
(column-number-mode t)
(line-number-mode t)
(size-indication-mode 1)
(display-time-mode 1)
(setq-default display-time-24hr-format t
              display-time-day-and-date t)

(global-unset-key (kbd "C-z"))
(define-key global-map (kbd "C-z") 'undo) ;Map C-z back to undo

(global-unset-key (kbd "C-/")) ;; Remove the mapping of undo.
(global-unset-key (kbd "C-,")) ;; Remove the mapping of undo.
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
(global-unset-key (kbd "C-x c"))
(global-unset-key (kbd "C-x C-b")) ;; 'switch-to-buffer

(transient-mark-mode t)          ;Set to 't' in Emacs 23 & onwards
(delete-selection-mode 1)        ;;Typed text repaces the selection.

(setq-default
 backup-inhibited t         ;disable backup
 make-backup-files nil      ;do not make backup files
 use-backup-dir t           ;use backup directory
 auto-save-default nil      ;disable auto save
 ;; Specify where backup files are stored
 ;; Backups have to be enabled
 backup-directory-alist (quote ((".*" . "~/.backups"))))

(when (display-graphic-p)
  ;; This kind of stuff is set via customize
  ;;(tool-bar-mode -1) ;; Remove the toolbar
  ;;(menu-bar-mode -1)  ;; Turn of the menu bar
  ;;(scroll-bar-mode -1) ;; Turn off the scroll bar
  (tooltip-mode -1)

  (setq-default tooltip-delay 3.0
                cursor-type 'box)
  
  ;; Prevent the cursor from blinking
  (blink-cursor-mode -1)) 

;;(require 'latex-preview-pane)

;;(setq bidi-display-reordering nil)
;;(setq enable-local-variables t)
;;(setq enable-local-eval t)

;; Let's get a backtrace on errors
;;(setq debug-on-error t)
;; Display byte-compiler warnings on error
;;(setq byte-compile-debug t)

;; Add backward delete word.
;; So that deleting a word does not add it to the kill-ring.
;; http://stackoverflow.com/questions/6133799/delete-a-word-without-adding-it-to-the-kill-ring-in-emacs
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

;;(global-set-key [C-backspace] 'backward-delete-word)

;; C-w will perform Call 'kill-region' or 'backward-kill-word'
;; depending on whether or not a region is selected.
;; http://ruslanspivak.com/2010/09/22/c-w-to-delete-word-backward-in-conkeror/
(defun kill-region-or-delete-word ()
  "Call 'kill-region' or 'backward-delete-word'.
Depending on whether or not a region is selected."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (kill-region (point) (mark))
    ;;(backward-kill-word 1)
    (backward-delete-word 1)))

(global-set-key (kbd "C-w") #'kill-region-or-delete-word)

;; Enable Emacs functionality that is disabled by default
(put 'set-goal-column 'disabled nil)
;;(put 'narrow-to-page 'disabled nil)
;;(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;;(setq-default enable-recursive-minibuffers t)

;; Make the Emacs kill ring work nice with the system clipboard.
;; Otherwise copy/paste between Emacs and the system will not work.
(unless mswindows-p
  (setq-default x-select-enable-clipboard t))

;; Opposite of C-x o
;; (other-window -1)

;;Command as Meta for OSX
(when osx-p
  ;;Https://www.wisdomandwonder.com/article/10269/screencast-the-best-emacs-modifier-key-setup-for-os-x
  ;; mac-function-modifier
  ;; mac-control-modifier
  ;; mac-command-modifier
  ;; mac-option-modifier
  ;; mac-right-command-modifier
  ;; mac-right-control-modifier
  ;; mac-right-option-modifier
  ;; mac-function-modifier
  ;; values can be 'control, 'alt, 'meta, 'super, 'hyper, nil 
  ;;(setq mac-command-modifier 'meta)
  (setq-default mac-right-command-modifier 'control
                mac-command-modifier 'meta
                mac-option-modifier 'super
                mac-function-modifier 'hyper)
  
  (unless (display-graphic-p)
    (set-keyboard-coding-system nil))

  ;; Do not create a new workspace on OSX when going full-screen
  (setq ns-use-native-fullscreen nil))

(setq inhibit-splash-screen t)
;;(setq initial-scratch-message nil)
(setq inhibit-startup-message t) ;; inhibit startup message
(setq initial-major-mode 'fundamental-mode)

;; Cause EDIFF to skip over "uninteresting" difference regions,
;; which are the regions where the variants differ only in the amount of the
;; white space and newlines.
(defconst ediff-ignore-similar-regions t)

;; Force ediff to use the directories it had previously used for
;; files A, B, or C, respectively.
(defconst ediff-use-last-dir t)

;; Prefer single frames
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defun dot-set-frame-maximized ()
  (interactive)
  (when (not (eq (frame-parameter nil 'fullscreen) 'maximized))
    (set-frame-parameter nil 'fullscreen 'maximized)))

(defun dot-set-frame-fullscreen ()
  (interactive)
  (when (not (eq (frame-parameter nil 'fullscreen) 'fullboth))
    (set-frame-parameter nil 'fullscreen 'fullboth)))

;; (dot-set-maximized)
;; (dot-set-fullscreen)

(defvar preferred-proportional-font-size 11)
(defvar preferred-proportional-fonts
  '("Calibri"
    "Lucida Grande"
    "Segoe UI"
    "DejaVu Sans"
    "Bitstream Vera"
    "Tahoma"
    "Verdana"
    "Helvetica"
    "Arial Unicode MS"
    "Arial"))

(defvar preferred-monospace-font-size 11)
(defvar preferred-monospace-fonts 
  '("Hack"          ;https://github.com/chrissimpkins/Hack
    "Menlo"         ;Default OSX
    "Consolas"      ;Default MSWindows
    "Inconsolata"   ;http://levien.com/type/myfonts/inconsolata.html
    "DejaVu Sans Mono"       ;http://dejavu-fonts.org/wiki/Main_Page
    "Anonymous Pro" ;http://www.marksimonson.com/fonts/view/anonymous-pro 
    "MonoSpace"
    "Droid Sans Mono" ;http://damieng.com/blog/2007/11/14/droid-font-family-courtesy-of-google-ascender
    "Source Code Pro" ;http://sourceforge.net/projects/sourcecodepro.adobe/files/
    "Meslo LG M DZ"   ;https://github.com/andreberg/Meslo-Font
    "ProFontIIx"      ;http://tobiasjung.name/profont/
    "Monaco"          ;OSX
    "ProggyCleanTT"   ;http://www.proggyfonts.net
    ))

(defun return-first-font-found (fonts)
  (let ((found nil))
    (dolist (font-name fonts)
      (when (and (not found)
                 (member font-name (font-family-list))) 
        (setq found font-name)))
    found))

(defun set-preferred-face (face font-name font-size)
  (when (and font-name font-size face)
    (set-face-attribute face nil :family font-name)
    (set-face-attribute face nil :height (round (* 10 font-size)))))

(defun set-preferred-default-face ()
  (set-preferred-face 'default
                      (return-first-font-found preferred-monospace-fonts)
                      preferred-monospace-font-size))

(defun set-preferred-variable-face ()
  (set-preferred-face 'variable-pitch
                      (return-first-font-found preferred-proportional-fonts)
                      preferred-proportional-font-size))

(defun set-preferred-fixed-face ()
  (set-preferred-face 'fixed-pitch
                      (return-first-font-found preferred-monospace-fonts)
                      preferred-monospace-font-size))

(defun create-custom-face (inherit family)
  (list (list t (list :inherit inherit :family family))))

(defun org-faces-to-fixed-pitch (family org-faces)
  (mapcar #'(lambda (element)
              (let ((face (car element))
                    (inherit (cdr element)))
                (face-spec-set face (create-custom-face inherit family))))
          org-faces))

(when (display-graphic-p)
  (set-preferred-default-face)
  (set-preferred-fixed-face)
  (set-preferred-variable-face)
  (add-hook 'emacs-lisp-mode-hook
            #'(lambda ()
                (buffer-face-set
                 (list
                  :family (return-first-font-found preferred-monospace-fonts)
                  :height (round (* 0.9 (face-attribute 'fixed-pitch :height)))))
                (buffer-face-mode t)))

  (add-hook 'dired-mode-hook
            #'(lambda ()
                (buffer-face-set
                 (list :family (return-first-font-found preferred-monospace-fonts)
                       :height (round (* 0.9
                                         (face-attribute 'fixed-pitch :height)))))
                (buffer-face-mode t))))

;; ;; What is this in the new org?
;; (set-face-attribute 'org-block-background nil 
;;                     :height (round (* 0.9 (face-attribute 'fixed-pitch
;;                                                           :height))))
;; (set-face-attribute 'org-block nil 
;;                     :height (round (* 0.9 (face-attribute 'fixed-pitch
;;                                                           :height))))


;; (set-face-attribute 'org-column-title nil
;;                     :family (face-attribute 'default :family)
;;                     ;;:height (face-attribute 'default :height)
;;                     :height (round (* 1.0 (face-attribute 'fixed-pitch
;;                                                           :height)))
;;                     )

;; (set-face-attribute 'org-table nil 
;;                     :height (round (* 0.9 (face-attribute 'fixed-pitch
;;                                                           :height))))

(defun org-variable-pitch-mode (enable)
  (if enable
      (progn
        (add-hook 'org-mode-hook #'variable-pitch-mode)
        (org-faces-to-fixed-pitch
         (return-first-font-found preferred-monospace-fonts)
         '((org-level-1 outline-1)
           (org-level-2 outline-2)
           (org-level-3 outline-3)
           (org-level-4 outline-4)
           (org-level-5 outline-5)
           (org-level-6 outline-6)
           (org-level-7 outline-7)
           (org-level-8 outline-8)
           (org-meta-line font-lock-comment-face)
           (org-document-info-keyword shadow)
           ;;(font-lock-comment-face)
           (org-verbatim shadow)
           (org-link link)
           ;;(org-block-background nil)
           (org-table nil)

           (org-formula nil)
           (org-block shadow)
           (org-special-keyword font-lock-keyword-face)
           (org-property-value nil)
           (org-code nil)
           (org-column-title org-column)
           ;;(org-block-begin-line nil)
           ;;(org-block-end-line nil)
           ))
        (face-all-attributes 'org-level-2 (selected-frame))
        (face-attribute 'org-level-2 :family))
    (remove-hook 'org-mode-hook #'variable-pitch-mode)))

;;(org-variable-pitch-mode nil)

;; (dolist (face '(org-column
;;                 org-block-background
;;                 org-table
;;                 org-block
;;                 org-code
;;                 ;;org-verbatim 
;;                 ;;org-special-keyword
;;                 ))
;;   (set-face-attribute face nil :inherit 'fixed-pitch))

;; (set-face-attribute 'org-level-2 nil
;;                     :inherit 'org-outline-2
;;                     :family (return-first-font-found
;;                              preferred-monospace-fonts))

;; (set-face-attribute 'org-level-1 nil
;;                     :inherit 'org-outline-1
;;                     :family (return-first-font-found
;;                              preferred-monospace-fonts)
;;                     :height (round (* 0.9 (face-attribute 'fixed-pitch
;;                                                           :height))))


;; (set-face-attribute 'org-table nil 
;;                     :height (round (* 0.9 (face-attribute 'fixed-pitch
;;                                                           :height))))
;; (set-face-attribute 'org-formula nil 
;;                     :height (face-attribute 'org-table :height))

(when nil
  ;; Highlight the current cursor line
  ;;(global-hl-line-mode t)
  (add-hook 'prog-mode-hook #'hl-line-mode 'append))

;; Don't let Emacs hurt your ears
(when (boundp 'visible-bell)
  (setq visible-bell t))

;; Should be enabled by default
(when (boundp 'mac-allow-anti-aliasing)
  (unless mac-allow-anti-aliasing
    (setq mac-allow-anti-aliasing t)))

;; http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)

;; When set to 't', will make the MSWindows EOL character ^M
;; appear in the buffer.
;;(setq inhibit-eol-conversion nil)

;; Set the name of the host and current path/file in title bar:
;; (setq frame-title-format
;;       (list (format "%s %%S: %%j " (system-name))
;;             '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '(
        ;; ""
        ;; invocation-name
        ;; " DIRECTV - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;
;; Color Theme
;;
;; http://pawelbx.github.io/emacs-theme-gallery/
;; https://emacsthemes.com/
;;
;; Decent themes include;
;; | adwaita    | deeper-blue | dichromacy      | light-blue       |
;; | manoj-dark | misterioso  | solarized-dark* | solarized-light* |
;; | tango      | tango-dark  | tsdh-dark       | tsdh-light       |
;; | wheatgrass | whiteboard  | wombat          | zenburn          |
(when (file-directory-p (expand-file-name "themes" dotfiles-dir))
  (add-to-list 'custom-theme-load-path
               (expand-file-name "themes" dotfiles-dir)
               t))

;;(custom-set-faces
;;'(default ((t (:background "black" :foreground "grey")))))

;;(load-theme 'tsdh-dark 'no-confirm)

(use-package solarized-theme
  :disabled t
  :ensure t
  :config (load-theme 'solarized-dark 'no-confirm))

(use-package aurora-theme
  :disabled t
  :ensure t
  :config (load-theme 'aurora 'no-confirm))

(use-package zenburn-theme
  :disabled t
  :ensure t 
  :config (load-theme 'zenburn 'no-confirm))

(use-package material-theme
  :disabled t
  :ensure t
  :config (load-theme
           'material
           ;;'material-light
           'no-confirm))

(use-package color-theme-sanityinc-tomorrow
  :disabled t
  :ensure t
  :config (load-theme
           'sanityinc-tomorrow-eighties
           ;; 'sanityinc-tomorrow-day
           ;; 'sanityinc-tomorrow-night
           ;; 'sanityinc-tomorrow-blue
           ;; 'sanityinc-tomorrow-bright
           'no-confirm))

(use-package labburn-theme ;;https://github.com/ksjogo/labburn-theme
  :ensure t)

;; Working with Asynchronous External Processes
;;(require 'comint)

;; May be necessary for very large files
;; See "http://www.sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/emacs_18.html"
;; for a description of when to use font-lock versus jit-lock modes.
;; (global-font-lock-mode t) ;; colorize all buffers
;; (setq jit-lock-stealth-time 16
;;       jit-lock-defer-contextually t
;;       jit-lock-stealth-nice 0.5)

;; This will actually slow things down.
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Multiline.html
;;(setq font-lock-multiline t)

;; Add PNG support to emacs
;; http://www.libpng.org/pub/png/libpng.html
;; http://openil.sourceforge.net/
;; http://stackoverflow.com/questions/2650041/emacs-under-windows-and-png-files

;; (defun smart-beginning-of-line ()
;;   "Move point to first non-whitespace character or beginning-of-line.

;; Move point to the first non-whitespace character on this line.
;; If point was already at that position, move point to beginning of line."
;;   (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
;;   (let ((oldpos (point)))
;;     (back-to-indentation)
;;     (and (= oldpos (point))
;;          (beginning-of-line))))

;;(global-set-key [home] 'smart-beginning-of-line)

(use-package dash
  ;;A modern list api for Emacs
  ;;https://github.com/magnars/dash.el
  :ensure t
  :config (dash-enable-font-lock))

(use-package names
  ;;Namespaces for Emacs. https://github.com/Malabarba/names/
  :ensure t) 

(use-package exec-path-from-shell
  ;;https://github.com/purcell/exec-path-from-shell
  :if osx-p ;;(memq window-system '(mac ns))
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package recentf ;; http://www.emacswiki.org/RecentFiles
  :ensure t
  :bind (("C-x C-r" . helm-recentf)
         ("C-x f" . helm-recentf))
  :config (recentf-mode 1))

(use-package flycheck ;;https://github.com/flycheck/flycheck  
  :if osx-p
  :ensure t
  :defer t
  ;;  :diminish (flycheck-mode "Fly")
  :init (add-hook 'prog-mode-hook #'flycheck-mode 'append)
  :config
  ;; (setq-default flycheck-highlighting-mode 'lines)
  ;;                         '(columns symbols sexps lines))
  (setq-default flycheck-indication-mode 'right-fringe))

(use-package diff-hl
  :if (and osx-p (display-graphic-p)) ;;Broken windows  
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'diff-hl-mode 'append)
  (add-hook 'org-mode-hook #'diff-hl-mode 'append)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  :config
  (diff-hl-mode)
  (unless osx-p ;; Disabled in osx due to an issue with Magit
    (diff-hl-flydiff-mode t)))

(use-package dired
  :defer t
  :config
  (add-hook 'dired-mode-hook #'hl-line-mode)
  (add-hook 'dired-mode-hook #'stripe-listify-buffer) 
  (setq dired-listing-switches "-l")
  (when osx-p (setq dired-use-ls-dired nil))
  (use-package stripe-buffer ;;https://github.com/sabof/stripe-buffer
    :ensure t))

(use-package dired+
  ;; http://www.emacswiki.org/DiredPlus
  :disabled t
  :ensure t
  :defer t
  :bind (("C-x C-d" . ido-dired))
  :init
  ;; Must be set prior to loading dired+
  (setq diredp-hide-details-initially-flag nil)
  ;; Activate hl-line minor mode
  (add-hook 'dired-mode-hook #'hl-line-mode)
  :config
  ;; Using 'a' to open a directory in the same buffer
  ;;(put 'dired-find-alternative-file 'disabled nil)  

  ;; Refresh also dired buffer
  ;; From Magnars blog
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  ;; Other
  (setq dired-listing-switches "-l")
  (when osx-p (setq dired-use-ls-dired nil)))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  ;; (add-hook 'prog-mode-hook #'yas-minor-mode)
  :commands (yas-minor-mode yas-global-mode)
  :defer t
  :mode ("\\.yasnippet" . snippet-mode)
  :config (yas-reload-all)
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

(use-package deft
  ;;https://github.com/jrblevin/deft
  :ensure t
  :commands deft
  :defer t
  :config
  (setq deft-extensions '("txt" "tex" "org" "rst")
        deft-directory "~/Documents/EDS/eds_tasks"
        deft-recursive t))

(use-package popup
  :disabled t
  :ensure t
  :defer t
  ;; :bind (("C-c C-f" . describe-thing-in-popup))
  :config
  ;;http://blog.jenkster.com/2013/12/popup-help-in-emacs-lisp.html
  ;; (defun describe-thing-in-popup ()
  ;;   (interactive)
  ;;   (let* ((thing (symbol-at-point))
  ;;          (help-xref-following t)
  ;;          (description (save-window-excursion
  ;;                         (with-temp-buffer
  ;;                           (help-mode)
  ;;                           (help-xref-interned thing)
  ;;                           (buffer-string)))))
  ;;     (popup-tip description
  ;;                :point (point)
  ;;                :around t
  ;;                :height 30
  ;;                :scroll-bar t
  ;;                :margin t)))
  )

;; http://www.emacswiki.org/EmacsClient
;; http://www.emacswiki.org/emacs/EmacsMsWindowsIntegration

;;(setenv "CVS_RSH" "ssh")

;; User rights on Windows don't seem to allow this.
;; emacsclientw.exe -na runemacs.exe -c
;;(setenv "EDITOR" (executable-find "emacsclientw.exe"))

;; This is here to stop the following error in Magit;
;;
;; "The directory ~/.emacs.d/server is unsafe"
;;
;; Probably because ~/.emacs.d/ is a symlink
;; Emacs server
(use-package server
  :if nil
  :ensure t
  :defer 5
  :config
  (when win32-p
    (setq-default server-auth-dir (expand-file-name "server" use-home))
    (and (>= emacs-major-version 23)
         (defun server-ensure-safe-dir (dir) "Noop" t))
    (unless (file-exists-p server-auth-dir)
      (make-directory server-auth-dir)))
  (unless (server-running-p)
    (server-start)))

;;(require 'server)
;;(when (and (eq window-system 'w32) (file-exists-p (getenv "APPDATA")))
;;  (setq server-auth-dir (concat (getenv "APPDATA") "\\.emacs.d"))
;;  (unless (file-exists-p server-auth-dir)
;;    (make-directory server-auth-dir)))
;;(server-start)

(use-package uniquify
  ;;http://www.emacswiki.org/emacs/uniquify
  :config
  ;; Use pathnames instead of <n> to uniquify buffer names
  ;;(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq-default
   uniquify-buffer-name-style 'reverse
   uniquify-separator "|"
   uniquify-after-kill-buffer-p t ; rename after killing "uniquified" buffer
   uniquify-ignore-buffers-re "^\\*" ; don't muck with special buffers
   ))

(use-package whitespace
  ;; http://emacswiki.org/emacs/WhiteSpace
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'whitespace-mode)
  :config
  (setq-default whitespace-style '(
                                   face
                                   ;;empty
                                   ;;tabs
                                   lines-tail
                                   ;;trailing
                                   ;;newline
                                   ))
  (setq-default indicate-buffer-boundaries 'right
                ;;indicate-empty-lines t ;; Indicate empty lines
                whitespace-line-column 80))

(use-package markdown-mode
  ;; http://jblevins.org/projects/markdown-mode/
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :config
  (use-package markdown-toc
    :ensure t)
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  (add-hook 'gfm-mode-hook 'turn-on-orgtbl)
  ;;(add-hook 'markdown-mode-hook 'gfm-mode)
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook #'hl-line-mode)
  ;;(add-hook 'markdown-mode-hook 'auto-revert-mode)

  ;; Usage Example:
  ;;  
  ;; <!-- BEGIN RECEIVE ORGTBL ${1:YOUR_TABLE_NAME} -->
  ;; <!-- END RECEIVE ORGTBL $1 -->
  ;;  
  ;; <!-- 
  ;; #+ORGTBL: SEND $1 orgtbl-to-gfm
  ;; | $0 | 
  ;; -->
  ;;
  ;; <!--- BEGIN RECEIVE ORGTBL sample -->
  ;; <!--- END RECEIVE ORGTBL sample -->

  (defun orgtbl-to-gfm (table params)
    "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
    (let* ((alignment (mapconcat (lambda (x) (if x "|--:" "|---"))
                                 org-table-last-alignment ""))
           (params2
            (list
             :splice t
             :hline (concat alignment "|")
             :lstart "| " :lend " |" :sep " | ")))
      (orgtbl-to-generic table (org-combine-plists params2 params)))))

(use-package rst
  ;; http://docutils.sourceforge.net/docs/user/emacs.html
  :ensure t
  :mode ("\\.rst\\'" . rst-mode)
  :config
  (add-hook 'rst-mode-hook #'visual-line-mode)
  (add-hook 'rst-mode-hook #'table-recognize)
  (add-hook 'rst-mode-hook #'hl-line-mode)
  (use-package table ;; http://emacswiki.org/emacs/TableMode
    :ensure t))

(use-package puml-mode  
  ;;https://github.com/skuro/puml-mode
  :ensure t
  :defer t
  ;; :mode (("\\.puml\\'" . puml-mode)
  ;;        ("\\.plantuml\\'" . puml-mode))
  :init (setq-default puml-plantuml-jar-path plantuml-jar-path)
  :config
  (add-hook 'puml-mode-hook #'hl-line-mode 'append)
  ;;(add-hook 'puml-mode-hook #'dot-set-frame-maximized 'append)
  )

;; Load org-mode and contribs
(use-package org
  :ensure org-plus-contrib
  :pin org-archive
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ;; ("C-h" . org-delete-backward-char)
         ("C-c !" . org-time-stamp-inactive))
  :mode ("\\.org$" . org-mode)
  :config
  (require 'org-id)
  
  (add-hook 'org-mode-hook #'(lambda () (visual-line-mode 1))
            'append) ;; Wrap lines at window edge
  (add-hook 'org-mode-hook #'(lambda () (font-lock-mode 1))
            'append) ;; Org buffers only

  ;; Set C-a and C-e to behave specially,
  ;; considering the headline and not the leading stars,
  ;; todo keywords, or the trailing tags.
  (setq-default org-special-ctrl-a/e t)

  ;; Needed for 'plantuml-mode
  (setq-default org-plantuml-jar-path plantuml-jar-path) 
  
  ;;https://github.com/zwz/plantuml-mode
  ;;Old.  Use puml-mode instead
  (use-package plantuml-mode
    :disabled t
    :ensure t
    :if plantuml-jar-path
    :init (setq-default plantuml-jar-path plantuml-jar-path))

  ;; http://orgmode.org/worg/org-configs/org-customization-guide.html
  ;; When the cursor is at the beginning of a headline, kill
  ;; the entire line and possible the folded subtree below the
  ;; line.
  ;; When in the middle of the headline text, kill the
  ;; headline up to the tags.
  ;; When after the headline text, kill the tags.
  (setq-default org-special-ctrl-k t)

  ;; RET on a hyperlink follows link
  (setq-default org-return-follows-link t) 

  (setq-default org-log-done 'time)

  (setq org-agenda-show-all-dates nil) ;; Show only days with something
  (setq org-agenda-archives-mode nil)
  (setq org-agenda-skip-comment-trees nil)
  (setq-default org-agenda-skip-function nil)

  ;; Unset C-c SPC from org-mode as this mapping used in Ace-Jump-Mode
  ;; (add-hook 'org-mode-hook #'(lambda ()
  ;;                            (local-unset-key (kbd "C-c SPC"))))

  (org-babel-do-load-languages
   'org-babel-load-languages  '((emacs-lisp . t)
                                (ditaa      . t)
                                (dot        . t)
                                (plantuml   . t)
                                (sh         . t)
                                (js         . t)
                                (clojure    . t)
                                (python     . t)
                                (css        . t)))

  ;; (defun my-org-confirm-babel-evaluate (lang body)
  ;;   (and (not (string= lang "ditaa"))     ; don't ask for ditaa
  ;;        (not (string= lang "dot"))       ; don't ask for Graphviz (dot)
  ;;        (not (string= lang "emacs-lisp")) ; don't ask for emacs lisp
  ;;        (not (string= lang "plantuml")))) ; don't ask for plantuml

  ;; (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  
  (setq org-confirm-babel-evaluate nil) ;; Set to nil to allow all execution

  (setq org-use-tag-inheritance t)

  ;; Make headlines with inherited tags show up in tag searches
  (setq org-tags-match-list-sublevels t)

  ;; I can fit ~155 characters across the screen; 3 more are needed for the
  ;; ellipsis for folded items, so -150 is about right for the tag position.
  (setq org-tags-column -77)

  ;; Use the same settings in the agenda
  ;;(setq org-agenda-tags-column org-tags-column)

  ;; Automatically change TODO entry to DONE when all children are done:
  ;; http://orgmode.org/manual/Breaking-down-tasks.html#Breaking-down-tasks
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;;(add-hook 'org-mode-hook 'flyspell-prog-mode 'append)

  ;; This one is kind of important!
  ;; Copy/pasting a new heading will also
  ;; duplicate the unique ID for that heading. Now, not so unique.
  ;; This section should take care of creating a new ID for each cloned subtree.
  ;; C-c C-x c clone subtree
  (setq org-id-link-to-org-use-id t) ;; Create a unique id if none exists
  ;; Create a unique id when a new heading is created.
  ;;(add-hook 'org-insert-heading-hook 'org-id-get-create)
  (setq org-clone-delete-id t) ;; Delete the new ID on clone

  ;;(setq org-latex-listings t)
  ;;(add-to-list 'org-latex-packages-alist '("" "listings"))

  (setq org-src-fontify-natively t) ; Syntax highlight within code blocks.
  (setq org-src-tab-acts-natively nil)  ; 't' makes tab behave like it
                                        ; does in the corresponding
                                        ; major mode.
  (setq org-deadline-warning-days 30)
  (setq org-list-allow-alphabetical nil) ; Set to 't' to allow
                                        ; alphabetical lists.
  (setq org-clock-into-drawer t)

  ;; http://orgmode.org/manual/Clean-view.html
  (setq org-startup-indented t)   ;;#+STARTUP: indent
  (setq org-hide-leading-stars t) ;;#+STARTUP: hidestars
  ;;(setq org-odd-levels-only t)    ;;#+STARTUP: odd

  (use-package ox-rst                     ;https://github.com/masayuko/ox-rst
    :disabled t
    :ensure t)
  (use-package ox-gfm                     ;gfm
    :disabled t
    :ensure t) 
  (use-package ox-taskjuggler             ;Scheduling in Org-Mode
    :disabled t
    :ensure t)
  (use-package ox-md                      ;Markdown exported
    :disabled t
    :ensure t) 
  (use-package org-bullets
    :if (and osx-p (display-graphic-p)) ;;not working in windows
    :ensure t
    :pin melpa-stable ;; melpa seems to be broken as of [2016-05-26 Thu]
    :init (add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1))
                    'append))
  (use-package ox-reveal
    ;; https://github.com/yjwen/org-reveal/blob/master/Readme.org
    :disabled t
    :ensure t
    :config
    (require 'ox-reveal)
    (setq org-reveal-theme "night")
    (setq org-reveal-root (concat "file:///"
                                  (expand-file-name "reveal.js"
                                                    dotfiles/site-dir)))))

(defun vc-git-print-log-date-only (files buffer)
  "Get the date of the most recent commit associated with FILES."
  (let ((coding-system-for-read vc-git-commits-coding-system))
    ;; `vc-do-command' creates the buffer, but we need it before running
    ;; the command.
    (vc-setup-buffer buffer)
    ;; If the buffer exists from a previous invocation it might be
    ;; read-only.
    (let ((inhibit-read-only t))
      (with-current-buffer
          buffer
        (apply 'vc-git-command buffer
               'async files
               (append
                '("log" "--no-color")
                '("--pretty=format:%cd" "-1")
                '("--")))))))

(defun return-date-of-recent-commit (buffer)
  (require 'vc)
  (when (vc-find-backend-function (vc-backend (buffer-file-name buffer))
                                  'print-log-date-only)
    (let ((limit -1)
          (vc-fileset nil)
          (backend nil)
          (files nil))
      (with-current-buffer buffer
        (setq vc-fileset (vc-deduce-fileset t)) ;FIXME: Why t? --Stef
        (setq backend (car vc-fileset))
        (setq files (cadr vc-fileset)))
      (with-temp-buffer
        (let ((status (vc-call-backend backend
                                       'print-log-date-only
                                       files
                                       (current-buffer))))
          (when (and (processp status) ;; Make sure status is a process
                     (= 0 (process-exit-status status))) ;; And that it has not terminated
            (while (not (eq 'exit (process-status status))) ;; Loop and sleep until complete
              (sit-for 1 t)))
          (buffer-string))))))

;; (eval-after-load 'org-ac
;;   '(progn
;;      ;; Make config suit for you. About the config item, eval the following sexp.
;;      ;;(customize-group "org-ac")
;;      ;;(org-ac/config-default)
;;      ))

;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html

;; (defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
;;   "if RUN-TOGETHER is true, spell check the CamelCase words"
;;   (let (args)
;;     (cond
;;      ((string-match  "aspell$" ispell-program-name)
;;       ;; force the English dictionary, support Camel Case spelling check
;;       ;; (tested with aspell 0.6)
;;       (setq args (list "--sug-mode=ultra" "--lang=en_US"))
;;       (if RUN-TOGETHER
;;           (setq args (append args '("--run-together"
;;                                     "--run-together-limit=5"
;;                                     "--run-together-min=2")))))
;;      ((string-match "hunspell$" ispell-program-name)
;;       (setq args nil)))
;;     args))

;; ispell-cmd-args is useless, it's the list of *extra* arguments we will
;; append to the ispell process when "ispell-word" is called.
;; ispell-extra-args is the command arguments which will *always* be
;; used when start ispell process
;; (setq ispell-extra-args (flyspell-detect-ispell-args t))
;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
;; (defadvice ispell-word (around my-ispell-word activate)
;;   (let ((old-ispell-extra-args ispell-extra-args))
;;     (ispell-kill-ispell t)
;;     (setq ispell-extra-args (flyspell-detect-ispell-args))
;;     ad-do-it
;;     (setq ispell-extra-args old-ispell-extra-args)
;;     (ispell-kill-ispell t)
;;     ))

;; (defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word
;;                                               activate)
;;   (let ((old-ispell-extra-args ispell-extra-args))
;;     (ispell-kill-ispell t)
;;     ;; use emacs original arguments
;;     (setq ispell-extra-args (flyspell-detect-ispell-args))
;;     ad-do-it
;;     ;; restore our own ispell arguments
;;     (setq ispell-extra-args old-ispell-extra-args)
;;     (ispell-kill-ispell t)))

(when win32-p
  (add-to-list 'exec-path
               (expand-file-name "hunspell/bin" dotfiles/win32-bin-dir)
               'append))

(defvar use-spellcheck
  "Set the spell check back-end to use."
  ;;'hunspell
  ;;'aspell
  'auto)

(defvar personal-dict "personal-dict.en")

(use-package flyspell
  :ensure t
  :defer t
  :if (or (executable-find "hunspell") (executable-find "ispell"))
  :diminish (flyspell-mode "Spell")
  :init
  (add-hook 'prog-mode-hook #'flyspell-prog-mode 'append)
  (add-hook 'text-mode-hook #'flyspell-mode 'append)
  :bind ("<f8>" . flyspell-buffer)
  :config
  (require 'ispell)
  ;; Set to nil for performance
  ;; https://www.emacswiki.org/emacs/FlySpell#toc3
  ;;(setq-default flyspell-issue-message-flag nil)

  ;; curl https://cgit.freedesktop.org/libreoffice/dictionaries/tree/en/en_US.dic --output en_US.dic
  ;; curl https://cgit.freedesktop.org/libreoffice/dictionaries/tree/en/en_US.aff --output en_US.aff
  ;;
  ;; brew install aspell
  ;; brew install hunspell
  ;;
  ;; Dictionary files (*.aff and *.dic) should be placed in
  ;; ~/Library/Spelling/ or /Library/Spelling/.  Homebrew itself
  ;; provides no dictionaries for Hunspell, but you can download
  ;; compatible dictionaries from other sources, such as
  ;; https://wiki.openoffice.org/wiki/Dictionaries .

  (when (file-exists-p (expand-file-name personal-dict personal-dir))
    (setq-default ispell-personal-dictionary
                  (expand-file-name "personal-dict.en"
                                    personal-dir))
    ;; (concat "-p" " "(expand-file-name "personal-dict.en"
    ;;                                   personal-dir))
    )
  
  ;;(setq ispell-extra-args '("-d en_US"))   ;; Set for Hunspell
  (setenv "LANG" "en_US")   ;;(getenv "LANG")
  (setenv "DICTIONARY" "en_US")   ;;(getenv "DICTIONARY")

  (cond 
   ((eq use-spellcheck 'auto)
    (cond
     ((executable-find "hunspell")
      (setq-default
       ispell-program-name "hunspell"
       ispell-really-hunspell t))

     ((executable-find "aspell")
      (setq-default
       ispell-program-name "aspell"
       ;; http://aspell.net/man-html/Notes-on-the-Different-Suggestion-Modes.html#Notes-on-the-Different-Suggestion-Modes
       ispell-extra-args '("--sug-mode=slow" "--lang=en_US")
       ispell-list-command "--list")))

    ((eq use-spellcheck 'hunspell)
     (setq-default
      ispell-program-name "hunspell")
     ispell-really-hunspell t))

   ((eq use-spellcheck 'aspell)
    (setq-default
     ispell-program-name "aspell"
     ;; http://aspell.net/man-html/Notes-on-the-Different-Suggestion-Modes.html#Notes-on-the-Different-Suggestion-Modes
     ispell-extra-args '("--sug-mode=slow" "--lang=en_US")
     ispell-list-command "--list")))

  ;; Extra flyspell delayed commands
  (mapcar 'flyspell-delay-command '(scroll-up scroll-down scroll-up-line
                                              scroll-down-line scroll-up1
                                              scroll-down1))

  (use-package writegood-mode
    ;;https://github.com/bnbeckwith/writegood-mode
    :ensure t
    :defer t
    :init (add-hook 'text-mode-hook #'writegood-mode 'append))

  (use-package langtool
    ;; https://github.com/mhayashi1120/Emacs-langtool
    ;; brew install languagetool
    ;; which languagetool # to determine path to script
    ;; less /path/to/languagetool # to determine path to languagetool-commandline.jar
    :if osx-p
    :ensure t
    :defer t
    ;; :init (add-hook 'text-mode-hook #'langtool-check 'append) 
    :config
    ;;(file-exists-p "/usr/local/Cellar/languagetool/3.2/libexec/languagetool-commandline.jar")
    (setq-default
     langtool-language-tool-jar "/usr/local/Cellar/languagetool/3.2/libexec/languagetool-commandline.jar"
     langtool-mother-tongue "en-US"
     langtool-disabled-rules '("WHITESPACE_RULE"
                               "EN_UNPAIRED_BRACKETS"
                               "COMMA_PARENTHESIS_WHITESPACE"
                               "EN_QUOTES")))

  ;; (add-hook 'flyspell-mode-hook
  ;;           #'(lambda ()
  ;;             (if flyspell-mode
  ;;                 (progn
  ;;                   (flyspell-buffer)
  ;;                   (local-set-key [(control /)]
  ;;                                  #'flyspell-check-next-highlighted-word)
  ;;                   (local-set-key [(control \,)]
  ;;                                  #'flyspell-check-previous-highlighted-word))
  ;;               (progn
  ;;                 (local-unset-key [(control /)])
  ;;                 (local-unset-key [(control \,)])))))

  ;; (defun flyspell-check-next-highlighted-word ()
  ;;   "Custom function to spell check next highlighted word"
  ;;   (interactive)
  ;;   (flyspell-goto-next-error))
  
  ;; Pre http://emacswiki.org/emacs/InteractiveSpell#toc6,
  ;; Fixes the ispell-phaf: No matching entry for nil. error

  ;;(setq-default ispell-program-name (executable-find "hunspell"))
  ;; (ispell-change-dictionary "en_US" t)
  ;; (ispell-change-dictionary "american" t)

  ;;(setq ispell-dictionary "american")
  ;;(setq ispell-extra-args '("-i" "utf-8"))

  ;; (add-to-list 'ispell-local-dictionary-alist
  ;;              (list "american"
  ;;                 "[[:alpha:]]"
  ;;                 "[^[:alpha:]]"
  ;;                 "[']"
  ;;                 t
  ;;                 `("-d" "en_US" ;; ,personal-dict
  ;;                      )
  ;;                 nil
  ;;                 'utf-8) t)

  ;; (setq ispell-personal-dictionary personal-dict)

  ;;(setq ispell-local-dictionary "en_US")
  ;; (setq ispell-local-dictionary-alist
  ;;       '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

  )

;;(setq ispell-local-dictionary-alist nil)

;; Replace ispell with hunspell
(when nil
  ;;(setq ispell-really-hunspell t)

  ;;(setq ispell-dictionary "american-hunspell")
  ;;(setq ispell-extra-args '("-a" "-i" "utf-8"))
  
  ;;(setq ispell-extra-args '("-i" "utf-8"))

  ;;(setq ispell-extra-args nil)
  ;; (add-to-list 'ispell-local-dictionary-alist
  ;;              (list "american-hunspell"
  ;;                    "[[:alpha:]]"
  ;;                    "[^[:alpha:]]"
  ;;                    "[']"
  ;;                    t
  ;;                    (list "-d" 
  ;;                          (expand-file-name "dict/en_US" 
  ;;                                            use-hunspell))
  ;;                    nil
  ;;                    'utf-8))

  ;; (add-to-list 'ispell-local-dictionary-alist
  ;;              (list "american-hunspell"
  ;;                 "[[:alpha:]]"
  ;;                 "[^[:alpha:]]"
  ;;                 "[']"
  ;;                 t
  ;;                 (list "-d" "en_US")
  ;;                 ;;`("-d" "en_US" ,personal-dict)
  ;;                 nil
  ;;                 'iso-8859-1))

  ;; (add-to-list 'ispell-dictionary-alist
  ;;              '("american-hunspell"
  ;;                "[[:alpha:]]"
  ;;                "[^[:alpha:]]"
  ;;                "[']"
  ;;                t
  ;;                ("-d" "en_US")
  ;;                nil
  ;;                iso-8859-1))
  ;; (add-to-list 'ispell-dictionary-alist
  ;;              '(nil
  ;;                "[[:alpha:]]"
  ;;                "[^[:alpha:]]"
  ;;                "[']"
  ;;                t
  ;;                ("-d" "en_US")
  ;;                nil
  ;;                iso-8859-1))
  )

(use-package powerline
  :ensure t
  :config
  (powerline-center-theme)
  (when osx-p
    (setq ns-use-srgb-colorspace nil))
  (when (display-graphic-p)
    (setq-default powerline-default-separator 'wave)))

(use-package spaceline
  :disabled t
  :pin melpa
  :ensure t 
  :if (display-graphic-p)
  :config
  (require 'spaceline-config)
  ;;(spaceline-spacemacs-theme)
  (spaceline-emacs-theme)
  (spaceline-helm-mode))

(use-package rich-minority ;https://github.com/Malabarba/rich-minority
  :disabled t
  :ensure t
  ;;:init (rich-minority-mode)
  :config
  (use-package smart-mode-line
    :ensure t   
    :config
    (progn
      (setq sml/no-confirm-load-theme t)
      (sml/setup)

      ;;(sml/apply-theme 'dark)
      ;;(sml/apply-theme 'light)
      ;;(sml/apply-theme 'respectful)
      (sml/apply-theme 'automatic))))

;; Requires (cua-mode t)
(use-package cua-base ;; MS Windows Conventional mouse/arrow movement & selection
  ;; C-c=Copy, C-x=Cut, C-v=Paste
  :ensure t
  :init (cua-mode 1)
  :config 
  ;; Set transient mark mode. i.e C-SPACE
  ;;(setq cua-highlight-region-shift-only nil)
  ;;(setq cua-toggle-set-mark t)
  ;;(setq cua-enable-cua-keys nil)
  ;;(setq cua-toggle-set-mark nil)

  (cua-selection-mode t) ;; Use standard emacs keys for almost everything

  ;;Enable the cursor indications
  (setq-default
   cua-enable-cursor-indications t
   ;; Use an ORANGE cursor in normal (insertion) mode in read-write buffers,
   cua-normal-cursor-color "orange"
   ;; a RED cursor in overwrite mode in read-write buffers,
   cua-overwrite-cursor-color "red"
   ;; and a GREEN cursor read-only buffers
   cua-read-only-cursor-color "green"))

(use-package gist
  :disabled t
  :ensure t
  :defer t)

(use-package git-timemachine
  ;; https://github.com/pidu/git-timemachine
  ;;:disabled t
  :ensure t
  :defer t
  :commands git-timemachine)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  ;; (add-hook 'magit-mode-hook #'hl-line-mode)
  (setenv "GIT_PAGER" "")

  (when nil
    (magit-auto-revert-mode -1))

  (use-package gitignore-mode
    :ensure t)
  (use-package gitconfig-mode
    :ensure t)
  (use-package gitattributes-mode
    :ensure t)

  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1)
  
  ;; (use-package magit-backup
  ;;   :ensure t
  ;;   :commands magit-backup-mode
  ;;   :config
  ;;   (magit-backup-mode -1))

  ;; (use-package magit-commit
  ;;   :ensure t
  ;;   :config
  ;;   (remove-hook 'server-switch-hook 'magit-commit-diff))

  ;; (unbind-key "M-h" magit-mode-map)
  ;; (unbind-key "M-s" magit-mode-map)
  ;; (unbind-key "M-m" magit-mode-map)
  ;; (unbind-key "M-w" magit-mode-map)

  ;; (bind-key "M-H" #'magit-show-level-2-all magit-mode-map)
  ;; (bind-key "M-S" #'magit-show-level-4-all magit-mode-map)
  ;; (bind-key "U" #'magit-unstage-all magit-mode-map)

  ;; (add-hook 'magit-status-mode-hook #'(lambda () (magit-monitor t)))
  
  ;; (add-hook 'magit-log-edit-mode-hook
  ;;           #'(lambda ()
  ;;               (set-fill-column 72)
  ;;               (flyspell-mode)))
  ;; (add-hook 'magit-commit-mode-hook
  ;;           #'(lambda ()
  ;;               (set-fill-column 50)
  ;;               (flyspell-mode)))
  )

;;(require 'git-commit-mode)
;;(require 'git-rebase-mode)

(use-package aggressive-indent ;;https://github.com/Malabarba/aggressive-indent-mode
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'aggressive-indent-mode 'append)
  ;; Unsure if inferior-lisp-mode-hook inherits from prog-mode-hook
  ;; (add-hook 'inferior-lisp-mode-hook #'aggressive-indent-mode 'append)
  )

(use-package olivetti
  ;;https://github.com/rnkn/olivetti
  ;;:disabled t
  :ensure t
  :defer t
  :init
  (add-hook 'rst-mode-hook #'(lambda () (olivetti-mode t)))
  (add-hook 'org-mode-hook #'(lambda () (olivetti-mode t)))
  (add-hook 'markdown-mode-hook #'(lambda () (olivetti-mode t)))
  :config (setq-default olivetti-body-width 0.8))

(use-package swiper
  ;;https://github.com/abo-abo/swiper
  :disabled t
  :ensure t
  :init (add-hook 'prog-mode-hook #'ivy-mode 'append)
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume))
  :config
  (use-package ivy
    ;;https://github.com/abo-abo/swiper
    :ensure t)
  
  (use-package counsel
    ;;http://melpa.org/#/counsel
    :disabled t
    :ensure t)
  
  (setq-default ivy-use-virtual-buffers t)
  (ivy-mode 1)
  
  ;; (global-set-key (kbd "<f6>") 'ivy-resume)
  ;; (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  ;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  ;; (global-set-key (kbd "<f1> l") 'counsel-load-library)
  ;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  ;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  ;; (global-set-key (kbd "C-c g") 'counsel-git)
  ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;; (global-set-key (kbd "C-c k") 'counsel-ag)
  ;; (global-set-key (kbd "C-x l") 'counsel-locate)
  ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  )

(use-package helm
  :ensure t
  :diminish helm-mode
  :defer 5
  :bind (("C-c h" . helm-command-prefix)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-mini)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ;;("C-c h o" . helm-occur)   ;??
         ("C-h a"   . helm-apropos)
         ;; ("C-x f"   . helm-multi-files)
         ;; ("M-s b"   . helm-occur)
         ;; ("M-s n"   . my-helm-find)
         ;; ("M-H"     . helm-resume)
         :map helm-map 
         ;; rebind TAB to run persistent action
         ("<tab>" . helm-execute-persistent-action)
         ;; make TAB works in terminal
         ("C-i" . helm-execute-persistent-action)
         ;; list actions using C-z
         ("C-z" . helm-select-action))
  :config
  (require 'helm-config)

  (setq-default
   helm-buffers-fuzzy-matching t
   helm-lisp-fuzzy-completion  t
   helm-recentf-fuzzy-match    t
   ;; helm-split-window-in-side-p t     ; open helm buffer inside
                                        ; current window, not occupy ;
                                        ; whole other window ;
   helm-move-to-line-cycle-in-source t ; move to end or beginning
                                        ; of source when reaching ;
                                        ; top or bottom of source. ;
   helm-ff-search-library-in-sexp    t ; search for library in
                                        ; `require' and ;
                                        ; `declare-function' sexp. ;
   helm-scroll-amount                8 ; scroll 8 lines other
                                        ; window using ;
                                        ; M-<next>/M-<prior> ;
   helm-ff-file-name-history-use-recentf t)
  
  (when (executable-find "curl")
    (setq-default helm-net-prefer-curl t))

  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (eshell-cmpl-initialize)
                (define-key eshell-mode-map [remap pcomplete] ;; Remap command
                  'helm-esh-pcomplete)
                (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))

  ;;(helm-autoresize-mode 1)
  (helm-mode 1)

  (use-package helm-descbinds
    :ensure t
    :bind ("C-h b" . helm-descbinds)
    :config (fset 'describe-bindings 'helm-descbinds))

  (use-package helm-ls-git ;;https://github.com/emacs-helm/helm-ls-git
    :disabled t
    :ensure t)

  (use-package swiper-helm ;; https://github.com/abo-abo/swiper
    :ensure t
    :bind (("C-s" . swiper)))

  (use-package helm-swoop
    :ensure t
    :bind (("M-i" . helm-swoop)
           ("M-I" . helm-swoop-back-to-last-point)
           ("C-c M-i" . helm-multi-swoop)
           ("C-x M-i" . helm-multi-swoop-all)
           :map isearch-mode-map
           ;; When doing isearch, hand the word over to helm-swoop
           ("M-i" . helm-swoop-from-isearch)   
           :map helm-swoop-map
           ;; From helm-swoop to helm-multi-swoop-all
           ("M-i" . helm-multi-swoop-all-from-helm-swoop)
           ;; Instead of helm-multi-swoop-all, you can also use
           ;; helm-multi-swoop-current-mode
           ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop) 
           ;; Move up and down like isearch
           ("C-r" . helm-previous-line)
           ("C-s" . helm-next-line)
           :map helm-multi-swoop-map
           ("C-r" . helm-previous-line)
           ("C-s" . helm-next-line)
           ;; When doing evil-search, hand the word over to helm-swoop
           ;;:map evil-motion-state-map
           ;; ("M-i" . helm-swoop-from-evil-search)
           ) 
    :config
    ;; Save buffer when helm-multi-swoop-edit complete
    (setq helm-multi-swoop-edit-save t)
    ;; If this value is t, split window inside the current window
    (setq helm-swoop-split-with-multiple-windows nil)
    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
    (setq helm-swoop-split-direction 'split-window-vertically)
    ;; If nil, you can slightly boost invoke speed in exchange for text color
    (setq helm-swoop-speed-or-color t)
    ;; Go to the opposite side of line from the end or beginning of line
    (setq helm-swoop-move-to-line-cycle t)
    ;; Optional face for line numbers
    ;; Face name is `helm-swoop-line-number-face`
    (setq helm-swoop-use-line-number-face t)
    ;; If you prefer fuzzy matching
    (setq helm-swoop-use-fuzzy-match t)
    ;; (setq helm-yas-display-key-on-candidate t)
    )

  (use-package helm-flyspell
    :ensure t
    :defer t
    :config
    (define-key flyspell-mode-map
      [remap flyspell-auto-correct-word] ;; Remap command
      'helm-flyspell-correct))

  (use-package projectile
    :ensure t
    ;; :bind (("C-c p" . projectile-command-map))
    :bind (("C-c p p" . helm-projectile-switch-project))
    :diminish projectile-mode
    :commands projectile-global-mode
    :config
    (use-package helm-projectile
      :ensure t
      :config
      (setq projectile-completion-system 'helm)
      (helm-projectile-on))
    (projectile-global-mode)
    (when mswindows-p
      (setq projectile-indexing-method 'alien))))

(use-package company
  :ensure t
  :diminish (company-mode "CO")
  :init (add-hook 'prog-mode-hook #'company-mode 'append)
  :bind (:map company-mode-map
              ("C-;" . helm-company)
              :map company-active-map
              ("C-;" . helm-company))
  :config  
  (use-package helm-company
    :ensure t)
  ;; (defadvice company-pseudo-tooltip-unless-just-one-frontend
  ;;     (around only-show-tooltip-when-invoked activate)
  ;;   (when (company-explicit-action-p)
  ;;     ad-do-it))
  )

;; auto-complete is an alternative to company-mode
;; Do not load auto-complete if company mode is loaded.
(use-package auto-complete
  :disabled t
  :ensure t
  :config 
  (require 'auto-complete-config)
  (ac-config-default)
  (use-package ac-helm
    :ensure t
    :bind (:map ac-complete-mode-map
                ("C-;" . ac-complete-with-helm))
    :config
    ;; (define-key ac-complete-mode-map (kbd "C-;") 'ac-complete-with-helm)
    ))

(use-package avy ;; https://github.com/abo-abo/avy
  :ensure t
  :bind (
         ;;("C-x C-;" . avy-goto-char-2)
         ;;("M-g w" . avy-goto-word-1)
         ("C-c j" . avy-goto-word-1)
         ;;("C-c C-j" . avy-goto-char-2) ;; Clashes with org-goto
         ;;("s-." . avy-goto-word-or-subword-1) 
         ("M-p" . ace-window)
         ("C-x o" . ace-window))
  :config
  (use-package ace-window
    ;;https://github.com/abo-abo/ace-window
    :ensure t    
    :config
    (setq-default
     avy-all-windows nil
     aw-keys '(?a ?s ?d ?f ?j ?k ?l)
     aw-scope 'global
     aw-background t
     aw-dispatch-always t)))

(use-package rainbow-mode
  ;;https://julien.danjou.info/projects/emacs-packages#rainbow-mode
  ;;rainbow-mode is a minor mode for Emacs which displays strings
  ;;representing colors with the color they represent as background.
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'rainbow-mode 'append))

(use-package winner
  ;;http://www.emacswiki.org/emacs/WinnerMode
  ;;When activated, it allows you to “undo” (and “redo”) changes in
  ;;the window configuration with the key commands ‘C-c left’ and ‘C-c right’
  :ensure t
  :if (not noninteractive)
  :diminish winner-mode
  :defer t
  :config (winner-mode +1))

(use-package windmove
  ;;https://www.emacswiki.org/emacs/WindMove
  :disabled t
  :ensure t)

(use-package beacon
  ;;https://github.com/Malabarba/beacon
  :if (display-graphic-p)
  :ensure t
  :diminish beacon-mode
  :config (beacon-mode 1))

(use-package git-gutter-fringe
  ;;https://github.com/syohex/emacs-git-gutter-fringe
  :disabled t
  :ensure t
  :bind (("C-x C-g" . git-gutter-mode)
         ("C-x v =" . git-gutter:popup-hunk)
         ;; Jump to next/previous hunk
         ("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)
         ;; Stage current hunk
         ("C-x v s" . git-gutter:stage-hunk)
         ;; Revert current hunk
         ("C-x v r" . git-gutter:revert-hunk))
  :commands (global-git-gutter-mode)
  :config
  ;;(setq-default git-gutter:lighter " GG")
  (setq-default
   git-gutter:update-interval 2
   ;; first character should be a space
   git-gutter:lighter " GG"
   ;; ignore all spaces
   git-gutter:diff-option "-w"))

(use-package hydra
  :disabled t
  :ensure t
  :init
  (defhydra hydra-zoom (global-map "<f7>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

(use-package volatile-highlights
  ;;https://github.com/k-talo/volatile-highlights.el
  :ensure t
  :config (volatile-highlights-mode t))

(use-package sublimity
  ;;https://github.com/zk-phi/sublimity
  :disabled t
  :if (display-graphic-p)
  :ensure t
  :config
  (require 'sublimity-map)
  ;;(require 'sublimity-attractive)
  ;;(require 'sublimity-scroll)
  (sublimity-mode 1)
  (sublimity-map-set-delay nil)

  ;; (setq sublimity-attractive-centering-width 100)    
  ;; (sublimity-attractive-hide-bars)
  ;; (sublimity-attractive-hide-vertical-border)
  ;; (sublimity-attractive-hide-fringes)
  ;; (sublimity-attractive-hide-modelines)
  )

(use-package smartparens ;; https://github.com/Fuco1/smartparens
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'smartparens-strict-mode)
  ;;(add-hook 'inferior-lisp-mode-hook #'smartparens-mode)
  :config
  (require 'smartparens-config)
  (smartparens-strict-mode t)
  (show-smartparens-mode t))

(use-package paredit
  ;;Alternate to smartparens
  ;;http://danmidwood.com/content/2014/11/21/animated-paredit.html
  ;;https://www.emacswiki.org/emacs/ParEdit
  :disabled t
  :ensure t 
  :bind (:map paredit-mode-map
              ("[" . paredit-open-round)
              ("]" . paredit-close-round)
              ("M-[" . paredit-wrap-round)
              ("(" . paredit-open-square)
              (")" . paredit-close-square)
              ;; C-M-f paredit-forward
              ;; C-M-f paredit-backward              
              ("C-e" . paredit-forward)
              ("C-a" . paredit-backward)
              ("C-<up>" . paredit-backward-up)
              ("C-<down>" . paredit-forward-down)
              ("M-d" . paredit-forward-kill-word)
              ;; C-w will perform Call 'kill-region' or 'backward-kill-word'
              ;; depending on whether or not a region is selected.
              ;; http://ruslanspivak.com/2010/09/22/c-w-to-delete-word-backward-in-conkeror/
              ("C-w" . paredit-kill-region-or-delete-word))
  :init
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'inferior-lisp-mode-hook    #'enable-paredit-mode)
  
  :config 
  (defun paredit-kill-region-or-delete-word ()
    "Call 'kill-region' or 'backward-delete-word'
     depending on whether or not a region is selected."
    (interactive)
    (if (and transient-mark-mode mark-active)
        (kill-region (point) (mark))
      ;;(backward-kill-word 1)
      (paredit-backward-kill-word))))

(when nil
  ;; Don't use when smartparens is enabled.
  ;; Highlights only the start/end parenthesis when cursor is over.
  ;; Only enabled when smartparens-mode is not enabled.

  ;; 'parenthesis highlights just brackets
  ;; 'expression highlights entire bracket expression
  ;; (setq-default show-paren-style 'expression)

  ;; (set-face-background 'show-paren-match (face-background 'dark))
  ;; (set-face-foreground 'show-paren-match "#def")
  ;; (set-face-attribute 'show-paren-match nil :weight 'extra-bold)

  (show-paren-mode +1))

;;  M-x list-colors-display
(when nil
  ;;Highlight the surrounding nested S-expression's surrounding the point.
  ;;https://www.emacswiki.org/emacs/HighlightSexps
  (require 'highlight-sexps)
  (add-hook 'lisp-mode-hook #'highlight-sexps-mode)
  (add-hook 'emacs-lisp-mode-hook #'highlight-sexps-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'highlight-sexps-mode)
  (add-hook 'lisp-interaction-mode-hook #'highlight-sexps-mode)
  (add-hook 'inferior-lisp-mode-hook #'highlight-sexps-mode)
  (highlight-sexps-mode +1)
  (setq hl-sexp-background-colors '("Gray10" "Gray20")))

(when nil
  ;; Highlights the sexp at the current position
  ;;https://www.emacswiki.org/emacs/HighlightSexp
  (require 'highlight-sexp)
  (add-hook 'lisp-mode-hook #'highlight-sexp-mode)
  (add-hook 'emacs-lisp-mode-hook #'highlight-sexp-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'highlight-sexp-mode)
  (add-hook 'lisp-interaction-mode-hook #'highlight-sexp-mode)
  (add-hook 'inferior-lisp-mode-hook #'highlight-sexp-mode)
  (highlight-sexp-mode +1))

(use-package paren-face
  ;;https://github.com/tarsius/paren-face
  ;; Makes parenthesis less visible 
  :disabled t
  :ensure t 
  :defer t
  :init (add-hook 'prog-mode-hook #'paren-face-mode 'append)
  :config
  (custom-set-faces 
   ;;'(parenthesis ((t (:foreground "DarkSlateGrey"))))
   '(parenthesis ((t (:foreground "DimGrey"))))))

(use-package rainbow-delimiters
  ;;Crazy multi-color highlighting of parenthesis
  ;;https://github.com/Fanael/rainbow-delimiters
  :defer t
  :ensure t
  :init
  ;; (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  ;; (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  ;; (add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
  ;; (add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
  ;; (add-hook 'inferior-lisp-mode-hook #'rainbow-delimiters-mode)

  ;; (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ido
  ;;Disabled - use Helm instead
  ;;http://www.masteringemacs.org/article/introduction-to-ido-mode
  :disabled t
  :ensure t
  :config
  (ido-mode)
  (setq ido-enable-flex-matching t)
  ;; .org files should be the first in the list.
  (setq ido-file-extensions-order '(".org" ".png" ".html" ".el" ".tex")) 
  (setq ido-everywhere t)
  (setq org-completion-use-ido t))

(use-package which-key
  ;;https://github.com/justbur/emacs-which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1)
  (setq-default which-key-idle-delay 0.5))

(use-package god-mode ;https://github.com/chrisdone/god-mode
  :disabled t
  :ensure t)

(use-package follow
  ;;https://www.gnu.org/software/emacs/manual/html_node/emacs/Follow-Mode.html
  ;;https://www.emacswiki.org/emacs/FollowMode
  :ensure t
  :bind (("C-c f" . follow-mode)))

;; http://www.emacswiki.org/emacs/UndoTree
;; http://ergoemacs.org/emacs/emacs_best_redo_mode.html
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind (("C-z" . undo)
         ("C-x u" . undo-tree-visualize)
         ("C-S-z" . redo))
  :init (global-undo-tree-mode)
  :config  
  (defalias 'redo 'undo-tree-redo))

(use-package adaptive-wrap
  :ensure t
  :defer t
  :init (add-hook 'text-mode-hook #'adaptive-wrap-prefix-mode 'append))

(use-package linum
  ;;Display line numbers
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'linum-mode 'append)
  ;; (add-hook 'rst-mode-hook #'linum-mode 'append)
  ;; (add-hook 'markdown-mode-hook #'linum-mode 'append)
  ;; (add-hook 'gfm-mode-hook #'linum-mode 'append)
  ;; (add-hook 'nxml-mode-hook #'linum-mode 'append)
  :config

  ;; (use-package hlinum
  ;;   ;;Highlight the current line number
  ;;   :ensure t
  ;;   :config
  ;;   (hlinum-activate)
  ;;   (set-face-attribute 'linum-highlight-face nil
  ;;                       :foreground "Yellow"
  ;;                       :background "DimGrey")
  ;;   (set-face-attribute 'linum nil
  ;;                       :foreground "Grey"
  ;;                       :background "DimGrey"))
  
  (defun linum-update-window-scale-fix (win)
    "fix linum for scaled text"
    (set-window-margins win
                        (ceiling (* (if (boundp 'text-scale-mode-step)
                                        (expt text-scale-mode-step
                                              text-scale-mode-amount) 1)
                                    (if (car (window-margins))
                                        (car (window-margins)) 1)))))
  (advice-add #'linum-update-window :after #'linum-update-window-scale-fix))

;; (use-package nlinum
;;   ;;https://www.emacswiki.org/emacs/LineNumbers#toc5  
;;   :ensure t)

(use-package zoom-frm
  ;;https://www.emacswiki.org/emacs/zoom-frm.el
  ;;https://github.com/emacsmirror/emacswiki.org/blob/master/zoom-frm.el
  :ensure t
  :bind (:map ctl-x-map
              ("C-=" . zoom-in/out)
              ("C-+" . zoom-in/out)
              ("C--" . zoom-in/out)
              ("C-0" . zoom-in/out))
  :config
  (setq-default zoom-frame/buffer 'buffer))

(use-package fill-column-indicator
  ;;https://github.com/alpaker/Fill-Column-Indicator
  ;;https://www.emacswiki.org/emacs/HighlightCurrentColumn
  ;;Unfortunately, does not work well when text is scaled.
  :disabled t
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'fci-mode 'append)
  :config (setq-default fci-rule-use-dashes t))

(use-package restclient
  ;;https://github.com/pashky/restclient.el
  :ensure t
  :defer t
  :config
  (use-package restclient-helm))

(use-package cider
  ;;https://github.com/clojure-emacs/cider
  :ensure t
  :defer t)

(use-package desktop
  ;;:disabled t
  :ensure t
  :if (display-graphic-p)
  :init (desktop-save-mode +1)
  :config
  (setq-default desktop-restore-eager 1)
  (setq-default desktop-save 'ask)     ;Always ask
  ;; Don't save the eww buffers
  ;; (setq desktop-buffers-not-to-save
  ;;       (concat desktop-buffers-not-to-save
  ;;               "\\|\\(^eww\\(<[0-9]+>\\)*$\\)"
  ;;               "\\|\\(^httpd\\(<[0-9]+>\\)*$\\)"))
  ;; (setq desktop-files-not-to-save
  ;;       (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)"
  ;;               "\\)$"))
  ;; (concat "\\(^/[^/:]*:\\|(ftp)$\\)" ; original value
  ;;         "\\|\\(\\.gpg$\\)"
  ;;         "\\|\\(\\.plstore$\\)"
  ;;         "\\|\\(\\.desktop$\\)"
  ;;         "\\|\\(\\TAGS$\\)") 

  ;; (setq desktop-buffers-not-to-save
  ;;       (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)"
  ;;               "\\)$"))(setq desktop-buffers-not-to-save
  ;;                             (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)"
  ;;                                     "\\)$"))
  ;;               (setq desktop-buffers-not-to-savessdd
  ;;                     (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)"
  ;;                             "\\)$")) 

  ;; (setq desktop-files-not-to-save
  ;;       (concat "\\(^/[^/:]*:\\|(ftp)$\\)" ; original value
  ;;               "\\("
  ;;               "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
  ;;               "\\|PUML Preview\\|\\.newsrc-dribble"
  ;;               "\\|*eww*\\|\\*httpd*"
  ;;               "\\)$"))
  (add-to-list 'desktop-modes-not-to-save 'eww-mode)
  (add-to-list 'desktop-modes-not-to-save 'httpd-mode)
  (add-to-list 'desktop-modes-not-to-save 'magit-mode)
  (add-to-list 'desktop-modes-not-to-save 'magit-rev-mode)
  (add-to-list 'desktop-modes-not-to-save 'magit-diff-mode)
  (add-to-list 'desktop-modes-not-to-save 'metaediff-mode)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  )

(use-package wttrin
  ;;http://pragmaticemacs.com/emacs/weather-in-emacs/
  ;;https://github.com/bcbcarl/emacs-wttrin
  :ensure t
  :defer t
  :config
  (add-to-list 'wttrin-default-cities "Los Angeles")
  (add-to-list 'wttrin-default-cities "Torrance")
  (add-to-list 'wttrin-default-cities "London")
  (add-to-list 'wttrin-default-cities "LAX")
  (add-to-list 'wttrin-default-cities "Cape Town"))

(use-package doc-view
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Document-View.html
  :ensure t
  :defer t
  :config
  (setq doc-view-continuous t
        doc-view-resolution 300))

(use-package comment-dwim-2
  ;; https://github.com/remyferre/comment-dwim-2
  :ensure t
  :bind (("M-;" . comment-dwim-2)))

(use-package shrink-whitespace
  ;; https://github.com/jcpetkovich/shrink-whitespace.el
  ;; See also https://www.emacswiki.org/emacs/DeletingWhitespace
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Useless-Whitespace.html
  :ensure t
  :defer t
  :commands shrink-whitespace)

(use-package impatient-mode
  :ensure t
  :defer t)

(use-package multiple-cursors
  ;;https://github.com/magnars/multiple-cursors.el
  :if osx-p
  :ensure t
  :bind (("C-c m" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(defvar puml-impatient-mode-load-path
  (expand-file-name "puml-impatient-mode" dotfiles/site-dir))

(use-package puml-impatient-mode
  :load-path puml-impatient-mode-load-path
  :pin manual
  :bind (:map puml-impatient-mode-map
              ("C-c C-p" . puml-imp-execute)
              ("C-c C-o" . puml-imp-visit-buffer))
  :commands puml-impatient-mode
  :config
  (use-package impatient-mode
    :ensure t)
  (add-hook 'puml-impatient-mode-hook #'hl-line-mode 'append)
  (setq plantuml-jar-path plantuml-jar-path)  
  (httpd-start))

;; Start eshell on Emacs launch.
;; https://github.com/emacs-helm/helm/wiki#eshell-command-on-files
(when nil
  (add-hook 'emacs-startup-hook
            #'(lambda ()
                (let ((default-directory (getenv "HOME")))
                  (command-execute 'eshell)
                  (bury-buffer)))))

(when (file-exists-p personal-dir)
  ;; Changes made through "M-x customize" will be stored here
  (setq-default custom-file (expand-file-name "custom.el" personal-dir))

  
  ;; Load all '.org' files in PERSONAL-DIR.
  (mapc 'org-babel-load-file
        (directory-files personal-dir
                         't
                         "^[^#].*org$"))
  
  ;; Load the remaining '.el' files without corresponding '.org' files
  ;; with the exception of custom.el, which is loaded later if present.
  (mapc #'(lambda (file)
            (load file 'noerror))
        (remove-if '(lambda (el-file) ;; Removes files with matching '.org'
                      (find-if '(lambda (org-file)
                                  (string= (file-name-sans-extension el-file)
                                           (file-name-sans-extension org-file)))
                               (directory-files personal-dir
                                                t
                                                "^[^#].*org$")))
                   (remove-if #'(lambda (file) ;; Removes custom.el
                                  (string= file custom-file))
                              (directory-files personal-dir
                                               t
                                               "^[^#].*el$"))))
  (load custom-file 'noerror))

(use-package fortune-cookie ;https://github.com/andschwa/fortune-cookie
  ;; Sets INITIAL-SCRATCH-MESSAGE to a fortune quote rendered by cowsay.
  :ensure t
  :if (and (executable-find "cowsay") (executable-find "fortune"))
  :config
  ;; Set fortune-cookie-cowsay-args within M-x customize to change
  ;; cowsay defaults
  (setq fortune-cookie-cowsay-args "-f ghostbusters -s")
  (fortune-cookie-mode))

;;Specify a starting directory
(when nil
  (find-file (or (when (and win32-p
                            (file-directory-p
                             (expand-file-name "Documents"
                                               (expand-file-name current-user
                                                                 "C:/users"))))
                   (expand-file-name "Documents"
                                     (expand-file-name current-user
                                                       "C:/users")))
                 use-home)))

;;Maximize the Emacs frame on use,
;;when menu, tool and scroll-bar modes are hidden
(when (and t
           (display-graphic-p)
           (>= emacs-major-version 24)
           (>= emacs-minor-version 4)
           (not (or menu-bar-mode tool-bar-mode scroll-bar-mode))
           (not (file-exists-p (expand-file-name ".emacs.desktop"
                                                 dotfiles-dir))))
  (toggle-frame-fullscreen))

;;(switch-to-buffer (get-buffer "*scratch*"))

;;; emacs.el ends here
