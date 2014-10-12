
(defvar *emacs-load-start* (current-time))

(require 'cl)
(require 'browse-url)

(defvar mswindows-p (string-equal "windows" (symbol-name system-type)))
(defvar macosx-p (string-equal "darwin" (symbol-name system-type)))
(defvar gnu/linux-p (string-equal "gnu/linux" (symbol-name system-type)))

(defvar use-home (expand-file-name "~"))
(defvar use-home/site (expand-file-name "site" use-home))
(defvar use-home/bin (expand-file-name "bin" use-home))

(add-to-list 'load-path use-home)  
(add-to-list 'load-path use-home/site)
(add-to-list 'load-path use-home/bin)

;;Replace the default Ispell dictionary with "Hunspell"
;;http://hunspell.sourceforge.net/
(defvar use-hunspell 
  (when (file-directory-p (expand-file-name "hunspell" use-home/site))
    (expand-file-name "hunspell" use-home/site)))

;; Add Windows paths to exec-path
(when mswindows-p
  (add-to-list 'exec-path (expand-file-name "bin" (getenv "EMACS_DIR")))
  (add-to-list 'exec-path "C:/Windows/system32")
  (add-to-list 'exec-path "C:/Windows")
  (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin"))

;(setenv "PATH" (concat (getenv "PATH") hunspell-path))
;; Only set the path if it exists
(when use-hunspell
  (add-to-list 'exec-path (expand-file-name "bin" use-hunspell)))

;; Check if path exists prior to adding to 'load-path
(mapc (lambda (arg)
       (when (file-directory-p arg)
         (add-to-list 'load-path arg)))
     (list
      (expand-file-name "slime-master" use-home/site)
      (expand-file-name "auto-complete-master" use-home/site)
      (expand-file-name "popup-el-master" use-home/site)
      (expand-file-name "parenface-master" use-home/site)
      (expand-file-name "emacs-color-theme-solarized-master" use-home/site)
      (expand-file-name "dash.el-master" use-home/site)
      (expand-file-name "smart-mode-line-master" use-home/site)
      (expand-file-name "powerline-master" use-home/site)
      (expand-file-name "company-mode-master" use-home/site)
      (expand-file-name "git-modes-master" use-home/site)
      (expand-file-name "magit-master" use-home/site)
      (expand-file-name "git-modes-master" use-home/site)
      (expand-file-name "rich-minority-master" use-home/site)
      (expand-file-name "rainbow-delimiters-master" use-home/site)
      (expand-file-name "highlight-sexp-master" use-home/site)))

;; See https://github.com/aki2o/org-ac
;(add-to-list 'load-path (expand-file-name "auto-complete-pcmp" use-home/site))
;(add-to-list 'load-path (expand-file-name "log4e" use-home/site))
;(add-to-list 'load-path (expand-file-name "org-ac" use-home/site))
;(add-to-list 'load-path (expand-file-name "yaxception" use-home/site))

;; Color theme
(let ((dir (expand-file-name "themes" dotfiles-dir)))
  (when (file-directory-p dir)
    (add-to-list 'custom-theme-load-path dir)))

(defvar org-plantuml-jar-path 
  (when (file-exists-p (expand-file-name "plantuml.jar" use-home/site))
    (expand-file-name "plantuml.jar" use-home/site)))

(setq default-directory use-home)

;; Specify where backup files are stored
;; Backups have to be enabled
(setq backup-directory-alist (quote ((".*" . "~/.backups"))))

(setq custom-file (expand-file-name ".emacs-custom.el" dotfiles-dir))
(load custom-file 'noerror)

;; Let's get a backtrace when errors are
(setq debug-on-error t)

;; Scrolling done right
(setq scroll-error-top-bottom t)

;; Display byte-compiler warnings on error
(setq byte-compile-debug t)

;; Well, it's more so that you know this option
(setq kill-ring-max 120)
(setq kill-whole-line t)
(setq kill-read-only-ok t)

;; Tooltips
(tooltip-mode -1)
(setq tooltip-delay 3.0)

;; Sentences
(setq sentence-end-double-space t)
(setq require-final-newline t)

;; Number of lines of continuity when scrolling by screenfuls.
(setq next-screen-context-lines 0)

;; (setq bidi-display-reordering nil)
(setq enable-local-variables t)
(setq enable-local-eval t)

;replace y-e-s by y
(fset 'yes-or-no-p 'y-or-n-p)

;;(setenv "EDITOR" "emacsclient")
;;(setenv "CVS_RSH" "ssh")

;; Set UTF-8 as the defaut encoding
(set-terminal-coding-system    'utf-8)
(set-keyboard-coding-system    'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq org-export-coding-system 'utf-8)

(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))



;;Smooth keyboard scrolling
(setq
 redisplay-dont-pause t                 ;; scrolling
 scroll-step 1                          ;; scroll line per line (1 line instead of 3)
 scroll-margin 0                        ;; do smooth scrolling, ...
 scroll-conservatively 100000           ;; ... the defaults ...
 scroll-up-aggressively 0               ;; ... are very ...
 scroll-down-aggressively 0             ;; ... annoying
 auto-window-vscroll nil)

(require 'recentf)
(recentf-mode 1)

(require 'uniquify)

;; Use pathnames instead of <n> to uniquify buffer names
;;(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(defun ljc-set-font (fnt)
  (unless fnt
    (when macosx-p (setq fnt 'menlo))
    (when mswindows-p (setq fnt 'consolas)))
  
  (cond ((and (equal fnt 'anonymous)
              (member "Anonymous Pro" (font-family-list)))
         ;; http://www.marksimonson.com/fonts/view/anonymous-pro
         (set-frame-font "Anonymous Pro 9"))
        ((and (equal fnt 'dejavu-mono)
              (member "DejaVu Sans Mono" (font-family-list)))
         ;; Really nice font for text editing
         ;; http://dejavu-fonts.org/wiki/Main_Page
         (set-frame-font "DejaVu Sans Mono:antialias=natural") ; antialias=natural/none/standard/subpixel
         (set-face-attribute 'default nil :family "DejaVu Sans Mono" :weight 'normal :height 110))
        ((and (equal fnt 'inconsolata)
              (member "Inconsolata" (font-family-list)))
         ;; http://levien.com/type/myfonts/inconsolata.html
         ;;(set-frame-font "Inconsolata:antialias=natural") ; antialias=natural/none/standard/subpixel
         ;;(set-face-attribute 'default nil :family "Menlo" :weight 'normal :height 120)
         (set-frame-font "Inconsolata 13"))
        ((and (equal fnt 'menlo)
              (member "Menlo" (font-family-list)))
         ;; Default OSX font
         (set-frame-font "Menlo:antialias=natural") ; antialias=natural/none/standard/subpixel
         (set-face-attribute 'default nil :family "Menlo" :weight 'normal :height 110))
        ((and (equal fnt 'monospace)
              (member "MonoSpace" (font-family-list)))
         ;; TODO
         (set-frame-font "Monospace 10"))
        ((and (equal fnt 'consolas)
              (member "Consolas" (font-family-list)))
         ;; Default font on MSWindows
         (set-frame-font "Consolas 12"))
        ((and (equal fnt 'droid-sans)
              (member "Droid Sans Mono" (font-family-list)))
         ;; http://damieng.com/blog/2007/11/14/droid-font-family-courtesy-of-google-ascender
         (set-frame-font "Droid Sans Mono 11"))
        ((and (equal fnt 'source-code)
              (member "Source Code Pro" (font-family-list)))
         ;; http://sourceforge.net/projects/sourcecodepro.adobe/files/
         (set-frame-font "Source Code Pro 11"))
        ((and (equal fnt 'meslo)
              (member "Meslo LG M DZ" (font-family-list)))
         ;; https://github.com/andreberg/Meslo-Font
         (set-frame-font "Meslo LG M DZ 10"))
        ((and (equal fnt 'profont)
              (member "ProFontIIx" (font-family-list)))
         ;; http://tobiasjung.name/profont/
         (set-frame-font "ProFontIIx 9"))
        ((and (equal fnt 'monaco)
              (member "Monaco" (font-family-list)))
         ;; Default on OSX
         (set-frame-font "Monaco 10"))
        ((and (equal fnt 'proggy)
              (member "ProggyCleanTT" (font-family-list)))
         ;; http://www.proggyfonts.net
         (set-frame-font "ProggyCleanTT"))
        ))

(ljc-set-font 'inconsolata)

;; Use space for tabulation
(set-default 'indent-tabs-mode nil)

;; Indicate empty lines
(set-default 'indicate-empty-lines t)

(setq next-line-add-newlines nil) ;; Normally, C-n on the last line of a buffer appends a newline to it. 
                                  ;; If the variable next-line-add-newlines is nil, then C-n gets an error instead 
                                  ;; (like C-p on the first line).

;;(setq inhibit-eol-conversion t) ; This makes the windows EOL character ^M appear in the buffer

(setq scroll-step 1)
(setq scroll-conservatively 5)

;; Enable emacs functionality that is disabled by default
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq enable-recursive-minibuffers t)

;; Iswitchb is much nicer for inter-buffer navigation.
(iswitchb-mode 1)

;; highlight matches from searches
(setq isearch-highlight t) ;???

(setq query-replace-highlight t) ; highlight during searching ; Highlight query object
(setq search-highlight t) ; highlight incremental search ; Highlight search object
(setq mouse-sel-retain-highlight t) ; Keep mouse high-lightening

(setq backup-inhibited t) ;disable backup  
(setq make-backup-files nil) ;do not make backup files
(setq use-backup-dir t)  ;use backup directory

(setq auto-save-default nil)            ;disable auto save

(cua-mode t)

;; MS Windows Conventional mouse/arrow movement & selection
;;(delete-selection-mode t) ; Set in cua-mode
;;(transient-mark-mode t) ; org-mode
  
(cua-selection-mode t)    ;; nil == C-c=Copy, C-x=Cut, C-v=Paste
                          ;; t == Use the standard Emacs keys for copy/paste etc.

;; This is set by CUA-mode.  Defining this anyway in case CUA editing keys are disabled.
(define-key global-map [?\C-z] 'undo) ;; Map C-z back to undo

;; (setq cua-highlight-region-shift-only nil) ;; Set transient mark mode. i.e C-SPACE
;; (setq cua-toggle-set-mark t)
  
;; Make the Emacs kill ring work nice with the system clipboard.
;; Otherwise copy/paste between Emacs and the system will not work.
(unless mswindows-p
  (setq x-select-enable-clipboard t))

;; This requires (cua-mode t), set previously.
(setq cua-enable-cursor-indications t)  ; Enable the cursor indications
(setq cua-normal-cursor-color "orange") ; The following will use a ORANGE cursor 
                                        ; in normal (insertion) mode in read-write buffers, 
(setq cua-overwrite-cursor-color "red") ; a RED cursor in overwrite mode in read-write buffers, 
(setq cua-read-only-cursor-color "green") ; and a GREEN cursor read-only buffers

;; (require 'font-lock)

;; Necessary for very large files
;; See "http://www.sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/emacs_18.html"
;; for a description of when to use font-lock versus jit-lock modes.
(global-font-lock-mode t) ;; colorize all buffers
(setq jit-lock-stealth-time 16
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)
;(setq font-lock-multiline t)

(require 'dired)

;; Using 'a' to open a directory in the same buffer
(put 'dired-find-alternative-file 'disabled nil)

;; Activate hl-line minor mode
(add-hook 'dired-mode-hook
          (lambda ()
            (hl-line-mode t)))

;; Refresh also dired buffer
;; From Magnars blog
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Other
(setq dired-listing-switches "-l")
(when macosx-p (setq dired-use-ls-dired nil))

;; Set the name of the host and current path/file in title bar:
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Column & line numbers in mode bar
(column-number-mode t)
(line-number-mode t)

(setq indicate-buffer-boundaries 'left)

(size-indication-mode 1)
(setq display-time-24hr-format t)
(display-time-mode 1)

;; Add PNG support to emacs
;; http://www.libpng.org/pub/png/libpng.html
;; http://openil.sourceforge.net/
;; http://stackoverflow.com/questions/2650041/emacs-under-windows-and-png-files

;; Disable mouse-2 event that was appending text into documents
(global-set-key [mouse-2] nil)

;; Shells are assigned to f11
;; F11 - eshell
;; (global-set-key [f11] '(lambda ()
;;                     (interactive)
;;                     (eshell)))
  
;; ;; ctrl-F11 cmdproxy
;; (global-set-key [(control f11)]
;;                 '(lambda ()
;;                   (interactive)
;;                   (cond
;;                     (mswindows-p
;;                      (let ((explicit-shell-file-name
;;                             (expand-file-name "bin/cmdproxy.exe" (getenv "EMACS_DIR")))
;;                            (shell-file-name "cmdproxy.exe"))
;;                        (shell)))
;;                     (t (shell)))))
;; ;; Alt-F11 bash
;; (global-set-key [(meta f11)]
;;                 '(lambda ()
;;                   (interactive)
;;                   (let ((explicit-shell-file-name
;;                          (if mswindows-p
;;                              "bash.exe"
;;                              "bash"))
;;                         (shell-file-name
;;                          (if mswindows-p
;;                              "bash.exe"
;;                              "bash")))
;;                     (shell))))

;; Opposite of C-x o
;; (other-window -1)  
(global-set-key [(control x) (p)] '(lambda () 
                                    (interactive)
                                    (other-window -1)))

(global-set-key (quote [f3]) 'bzg-big-fringe-mode)

;; Prevent accidentally killing emacs.
(global-set-key [(control x) (control c)] 
                '(lambda ()
                   (interactive)
                   (if (y-or-n-p-with-timeout "Do you really want to exit Emacs ? " 4 nil)
                       (save-buffers-kill-emacs))))

;; Get rid of set-fill-column, and instead open a list of recent files
(global-set-key [(control x) (f)] 'recentf-open-files)
      
;; Shortcuts to common functions
(global-set-key [(control c) (f)] 'find-function-at-point)
(global-set-key [(control c) (g)] 'goto-line)
(global-set-key [(control c) (control g)] 'goto-line)
  
;; keys for buffer creation and navigation.  
;;  Use C-x C-b instead of the usual C-x b
(global-set-key [(control x) (control b)] 'iswitchb-buffer)
  
;; Set C-c h to help
(global-set-key [(control c) (h)] 'help-command)

;; For whatever reason, the HELLO file locks up this version of Emacs for Windows.
;; Unset this command.
(global-unset-key [(control c) (h) (h)])
   
;; http://sites.google.com/site/steveyegge2/effective-emacs
;; Item 2: Invoke M-x without the Alt key
(global-set-key [(control x) (control m)] 'execute-extended-command)
  
;;Item 3: Prefer backward-kill-word over Backspace
(global-set-key [(control w)] 'backward-kill-word)
(global-set-key [(control x) (control k)] 'kill-region) ;; Instead of C-w 
;;(global-set-key "\C-c\C-k" 'kill-region) ; org-mode has this mapped      

;; Map pcomplete to C-<tab>, otherwise clobbered by flyspell mode
;(global-set-key [(control c) (tab)] 'pcomplete)

;; (setq ns-right-command-modifier 'meta)

(when (boundp mac-allow-anti-aliasing) 
  (setf mac-allow-anti-aliasing t))

;; Note that the cua-emul, gnuserve & cua libraries are optional
;; (if mswindows-p
;;     (progn
;;       ;; Grep equivalent on Windows
;;       ;;(setq grep-command "c:/cygwin/bin/grep -n -a -e ")
;;       (setq grep-command "findstr /n /s ")

;;       ;; Windows Execute from dired
;;       (define-key dired-mode-map "w"
;;         (function
;;          (lambda ()
;;            (interactive)
;;            (setq w32-shellex-no-dired-hook t)
;;            (require 'w32-shellex)
;;            (w32-shellex-dired-on-objects))))

;;       ;; Start gnuserv on Windows 
;;       (if (or (eq window-system 'w32) (eq window-system 'win32))
;;           (when (require 'gnuserv nil 'noerror)
;;             (setq server-done-function 'bury-buffer 
;;                   gnuserv-frame (car (frame-list))) 
;;             (gnuserv-start) 
;;             ;; Open buffer in existing frame instead of creating new one... 
;;             (setq gnuserv-frame (selected-frame)) 
;;             (message "gnuserv started.")))))

;; (require 'comint)

(if t
    (progn
      (require 'company)
      (add-hook 'emacs-lisp-mode-hook 'company-mode)
      (add-hook 'emacs-lisp-mode-hook 'company-mode 'append))
  (progn
    ;; (require 'auto-complete)
    ;; (require 'auto-complete-config)
    ;; (ac-config-default)
    ))

;(require 'undo-tree)

;(global-undo-tree-mode)

;(defalias 'redo 'undo-tree-redo)
;(global-set-key [(control z)] 'undo) ; [Ctrl+z]
;(global-set-key [(control shift z)] 'redo) ; [Ctrl+Shift+z];  Mac style  
;(global-set-key [(control y)] 'redo) ; [Ctrl+y]; Microsoft Windows style

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

;;(global-set-key [home] 'smart-beginning-of-line)

(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode)) ;; Allow ASDF files to be edited as lisp files

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)

(eval-after-load 'paredit
  '(define-key paredit-mode-map (kbd "[") 'paredit-open-round))
(eval-after-load 'paredit
  '(define-key paredit-mode-map (kbd "]") 'paredit-close-round))
(eval-after-load 'paredit
  '(define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-round))
(eval-after-load 'paredit
  '(define-key paredit-mode-map (kbd "(") 'paredit-open-square))
(eval-after-load 'paredit
  '(define-key paredit-mode-map (kbd ")") 'paredit-close-square))

(eval-after-load 'paredit
  '(define-key paredit-mode-map (kbd "C-c C-f") 'paredit-forward))
(eval-after-load 'paredit
  '(define-key paredit-mode-map (kbd "C-c C-b") 'paredit-backward))

;;  M-x list-colors-display 

(when nil
  (require 'highlight-sexps) ;; Highlights the nested expressions surrounding the point
  (autoload 'highlight-sexps-mode "highlight-sexps-mode" "Turn on highlighting of sexps." t)
  (add-hook 'lisp-mode-hook 'highlight-sexps-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-sexps-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'highlight-sexps-mode)
  (add-hook 'lisp-interaction-mode-hook 'highlight-sexps-mode)
  (add-hook 'slime-repl-mode-hook 'highlight-sexps-mode)
  (setq hl-sexp-background-colors '("Gray10" "Gray40" "Gray50" "Gray60" "Gray70")))

;; show-paren-mode allows one to see matching pairs of parentheses and other characters. 
;; When point is on one of the paired characters, the other is highlighted. 
(when nil
  (show-paren-mode 0)
  (setq show-paren-style 'expression))  ; 'parenthesis highlight just brackets
                                        ; 'expression highlight entire bracket expression

(when nil
  (require 'parenface) ; Dims parenthesis
  (eval-after-load 'parenface
    (progn
      (set-face-foreground 'parenface-paren-face "SteelBlue4")
      (set-face-foreground 'parenface-bracket-face "SteelBlue4")
      (set-face-foreground 'parenface-curly-face "SteelBlue4"))))

(when t
  (require 'rainbow-delimiters) ;; highlights parens, brackets, and braces according to their depth
  ;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ; Add to all programming modes
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode))

(when t
  (require 'highlight-sexp) ;; Highlights the sexp at the current position
  (add-hook 'lisp-mode-hook 'highlight-sexp-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'highlight-sexp-mode)
  (add-hook 'lisp-interaction-mode-hook 'highlight-sexp-mode)
  (add-hook 'slime-repl-mode-hook 'highlight-sexp-mode))

;; Prevent the cursor from blinking
(blink-cursor-mode 0)

;; Use a minimal cursor
(setq default-cursor-type 'hbar)

;; You need to set `inhibit-startup-echo-area-message' from the
;; customization interface:
;; M-x customize-variable RET inhibit-startup-echo-area-message RET
;; then enter your username
;;(setq inhibit-startup-echo-area-message "guerry")

;; Remove the toolbar
(tool-bar-mode 0)

;; Turn of the menu bar
(menu-bar-mode 0)

;; Turn off the scroll bar
(scroll-bar-mode 0)

;; See http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

  (define-minor-mode hidden-mode-line-mode
    "Minor mode to hide the mode-line in the current buffer."
    :init-value nil
    :global nil
    :variable hidden-mode-line-mode
    :group 'editing-basics
    (if hidden-mode-line-mode
        (setq hide-mode-line mode-line-format
              mode-line-format nil)
      (setq mode-line-format hide-mode-line
            hide-mode-line nil))
    (force-mode-line-update)
    ;; Apparently force-mode-line-update is not always enough to
    ;; redisplay the mode-line
    (redraw-display)
    (when (and (called-interactively-p 'interactive)
               hidden-mode-line-mode)
      (run-with-idle-timer
       0 nil 'message
       (concat "Hidden Mode Line Mode enabled.  "
               "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

;; Command to toggle the display of the mode-line as a header
(defvar-local header-line-format nil)
(defun mode-line-in-header ()
  (interactive)
  (if (not header-line-format)
      (setq header-line-format mode-line-format
            mode-line-format nil)
    (setq mode-line-format header-line-format
          header-line-format nil))
  (set-window-buffer nil (current-buffer)))
(global-set-key (kbd "C-s-SPC") 'mode-line-in-header)


;; Activate hidden-mode-line-mode
(hidden-mode-line-mode 1)

;; A small minor mode to use a big fringe
(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 100 (frame-char-width)))
        2))))

;; Now activate this global minor mode
(bzg-big-fringe-mode 0)

;; To activate the fringe by default and deactivate it when windows
;; are split vertically, uncomment this:
;; (add-hook 'window-configuration-change-hook
;;           (lambda ()
;;             (if (delq nil
;;                       (let ((fw (frame-width)))
;;                         (mapcar (lambda(w) (< (window-width w) fw))
;;                                 (window-list))))
;;                 (bzg-big-fringe-mode 0)
;;               (bzg-big-fringe-mode 1))))

;; Get rid of the indicators in the fringe
(mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
        fringe-bitmaps)

;; Set the color of the fringe
;;(custom-set-faces
;; '(fringe ((t (:background "white")))))
 
;;(custom-set-faces
;;  '(default ((t (:background "black" :foreground "grey"))))
;;  '(fringe ((t (:background "black")))))

;; Replace ispell with hunspell
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t)
  ;;(setq ispell-extra-args '("-a" "-i" "utf-8"))
  (setq ispell-extra-args '("-i" "utf-8"))
  ;;(setq ispell-extra-args nil)
  (setq ispell-local-dictionary-alist (list 
                                       (list "american-hunspell"
                                             "[[:alpha:]]"
                                             "[^[:alpha:]]"
                                             "[']"
                                             t
                                             (list "-d" 
                                                   (expand-file-name "dict/en_US" 
                                                                     use-hunspell))
                                             nil
                                             'utf-8)))
  (setq ispell-dictionary "american-hunspell"))

(global-hl-line-mode t)

(defconst ediff-ignore-similar-regions t)
(defconst ediff-use-last-dir t)
(defconst ediff-diff-options " -b ")

(require 'git-commit-mode)
(require 'git-rebase-mode)
(require 'magit)
  
(global-set-key [(control x) (g)] 'magit-status)
(global-set-key (quote [f9]) 'magit-status)

;; This is here to stop the following error in Magit;
;; "The directory ~/.emacs.d/server is unsafe"
(and (>= emacs-major-version 23)
     (defun server-ensure-safe-dir (dir) "Noop" t))

;(load-theme 'manoj-dark t)
(load-theme 'zenburn t)
;(load-theme 'whiteboard t)

(require 'rich-minority)
(require 'smart-mode-line)

(setq sml/no-confirm-load-theme t)
(sml/setup)

(sml/apply-theme 'dark)
;(sml/apply-theme 'light)
;(sml/apply-theme 'respectful)
;(sml/apply-theme 'automatic)

(require 'powerline)

;(powerline-default-theme)
(powerline-center-theme)

(require 'ox-taskjuggler)

;; (require 'ox-confluence)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode)) ;; Specify files to be edited in org-mode

;(require 'org-ac)

;; Make config suit for you. About the config item, eval the following sexp.
;; (customize-group "org-ac")

;(org-ac/config-default)

(define-key org-mode-map "\C-h" 'org-delete-backward-char)
(global-set-key [(control c) (l)] 'org-store-link)
(global-set-key [(control c) (c)] 'org-capture)
(global-set-key [(control c) (a)] 'org-agenda)
(global-set-key [(control c) (b)] 'org-iswitchb) ; Similar to iswitchb, but filters to org buffers
(global-set-key [(control x) (b)] 'org-iswitchb) ; Override default iswitchb-buffer keys

(setq org-special-ctrl-a/e t) ; Set C-a and C-e to behave specially, 
                              ; considering the headline and not the leading stars, 
                              ; todo keywords, or the trailing tags.

(setq org-special-ctrl-k t) ; When the cursor is at the beginning of a headline, kill
                            ; the entire line and possible the folded subtree below the
                            ; line.
                            ; When in the middle of the headline text, kill the
                            ; headline up to the tags.
                            ; When after the headline text, kill the tags.

(org-babel-do-load-languages 
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ditaa . t)
   (dot . t)
   (plantuml . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  (and (not (string= lang "ditaa"))      ; don't ask for ditaa
       (not (string= lang "dot"))        ; don't ask for Graphviz (dot)
       (not (string= lang "emacs-lisp")) ; don't ask for emacs lisp   
       (not (string= lang "plantuml")))) ; don't ask for plantuml

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; Turn on tag inheritance
(setq org-use-tag-inheritance t)
;; Make headlines with inherited tags show up in tag searches
(setq org-tags-match-list-sublevels t)

;; I can fit ~155 characters across the screen; 3 more are needed for the
;; ellipsis for folded items, so -150 is about right for the tag position.
(setq org-tags-column -77)
; Use the same settings in the agenda
;(setq org-agenda-tags-column org-tags-column)

;; Automatically change TODO entry to DONE when all children are done:
;; http://orgmode.org/manual/Breaking-down-tasks.html#Breaking-down-tasks
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(setq org-log-done t)
(add-hook 'org-mode-hook 'turn-on-font-lock)  ;Org buffers only
(setq org-agenda-show-all-dates t)

(setq org-agenda-archives-mode nil)
(setq org-agenda-skip-comment-trees nil)
(setq org-agenda-skip-function nil)

(add-hook 'org-mode-hook 
          'turn-on-visual-line-mode) ;Wrap lines at window edge

(when (fboundp 'set-face-attribute)
    ;; Make sure that a fixed-width face is used when we have a column
    ;; table.
    (set-face-attribute 'org-column nil
                        :height (face-attribute 'default :height)
                        :family (face-attribute 'default :family)))

(when (fboundp 'set-face-attribute)
    ;; Make sure that a fixed-width face is used when we have a column
    ;; table.
    (set-face-attribute 'org-table nil
                        :height (face-attribute 'default :height)
                        :family (face-attribute 'default :family)))

;;(add-hook 'org-mode-hook 'flyspell-prog-mode 'append)
 
;;(setq flyspell-issue-message-flag nil) ; for speed apparently

;; some extra flyspell delayed command
;; (mapcar 'flyspell-delay-command      
;;      '(scroll-up1 scroll-down1))

(global-unset-key [(control /)]) ;; Remove the mapping of undo.  Reused in flyspell.
(global-unset-key [(control \,)]) ;; Remove the mapping of undo.  Reused in flyspell.
(global-set-key [(f8)] 'flyspell-mode)
  
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
 (interactive)
  (flyspell-goto-next-error))
                                    
(add-hook 'flyspell-mode-hook (lambda ()
                                (if flyspell-mode
                                    (progn
                                      (flyspell-buffer)
                                      (local-set-key [(control /)] 
                                                     'flyspell-check-next-highlighted-word)
                                      (local-set-key [(control \,)] 
                                                     'flyspell-check-previous-highlighted-word))
                                  (progn
                                    (local-unset-key [(control /)])
                                    (local-unset-key [(control \,)])))))

;;(setq org-footnote-auto-adjust t) ;Or place the following in the .org file 
                                    ;#+STARTUP: fnadjust

(setq org-id-link-to-org-use-id t)

(setq org-latex-listings t)

(add-to-list 'org-latex-packages-alist '("" "listings"))

(setq org-src-fontify-natively t)

(setq org-deadline-warning-days 30)

(setq org-clone-delete-id t)

(setq org-list-allow-alphabetical t)

(setq org-clock-into-drawer t)

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

(setq org-hide-leading-stars t)

(setq org-reveal-theme "night")

(setq org-publish-project-alist 
      '(
        ("org-balooga"
         ;; Path to your org files.
         :base-directory "~/blog/org/"
         :base-extension "org"

         ;; Path to your Jekyll project.
         :publishing-directory "~/blog/balooga/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4 
         :html-extension "html"
         :body-only t ;; Only export section between <body> </body>
         )


        ("org-static-blaooga"
         :base-directory "~/blog/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
         :publishing-directory "~/blog/"
         :recursive t
         :publishing-function org-publish-attachment)

        ("balooga" :components ("org-balooga" "org-static-balooga"))

        ))

;; Setup autoloads
(require 'slime-autoloads)

;; Set your lisp system and some contribs
;; (setq inferior-lisp-program (concat "~/lw-console.exe -init "
;;                                     use-home
;;                                     "/lispworks.lisp"))
;; (setf inferior-lisp-program (concat "/Applications/Clozure CL.app/Contents/MacOS/dx86cl64 " "-K utf-8 -tty"))
;(setf inferior-lisp-program (concat "/Applications/Clozure CL.app/Contents/MacOS/dx86cl64"))
(setf inferior-lisp-program "/usr/local/bin/sbcl")
;; (file-exists-p (concat "/Applications/Clozure CL.app/Contents/MacOS/dx86cl64"))
(setf slime-net-coding-system 'utf-8-unix)
(setf slime-contribs '(slime-fancy))

;; Bound to f11 in Emacs 24.4
(when (and (>= emacs-major-version 24)
           (>= emacs-minor-version 4))
  (toggle-frame-fullscreen))

;;;;    Start Directory
;(find-file use-git-repo)
(find-file use-home)

(setq inhibit-splash-screen t)

;; Don't use messages that you don't read
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

(setq initial-major-mode 'org-mode)
;; Don't let Emacs hurt your ears when t
(setq visible-bell t)

(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

(defvar *emacs-load-end* (current-time))

(defun dot-load-time ()
  (interactive)
  (message ".emacs loaded in %ds" 
           (destructuring-bind (hi lo ms ps) *emacs-load-end*
             (- (+ hi lo)
                (+ (first *emacs-load-start*) (second *emacs-load-start*))))))
