;; -*-lisp-*-
;;
;; Here is a sample .stumpwmrc file

(in-package :stumpwm)

;; change the prefix key to something else
(set-prefix-key (kbd "C-z"))

;; prompt the user for an interactive command. The first arg is an
;; optional initial contents.
(defcommand colon1 (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))

;; Read some doc
(define-key *root-map* (kbd "d") "exec gv")
;; Browse somewhere
(define-key *root-map* (kbd "b") "colon1 exec firefox http://www.")
;; Ssh somewhere
(define-key *root-map* (kbd "C-s") "colon1 exec xterm -e ssh ")
;; Lock screen
(define-key *root-map* (kbd "C-l") "colon1 exec xrandr --output VGA-0 --mode 1280x800")
;; Read some doc
(define-key *root-map* (kbd "C-c") "exec gnome-terminal")

;; Web jump (works for Google and Imdb)
(defmacro make-web-jump (name prefix)
  `(defcommand ,(intern name) (search) ((:rest ,(concatenate 'string name " search: ")))
    (substitute #\+ #\Space search)
    (run-shell-command (concatenate 'string ,prefix search))))

(make-web-jump "google" "firefox http://www.google.fr/search?q=")
(make-web-jump "imdb" "firefox http://www.imdb.com/find?q=")

;; C-t M-s is a terrble binding, but you get the idea.
(define-key *root-map* (kbd "M-s") "google")
(define-key *root-map* (kbd "i") "imdb")

;; Message window font
(set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")

;;; Define window placement policy...

;; Clear rules
(clear-window-placement-rules)

;; Last rule to match takes precedence!
;; TIP: if the argument to :title or :role begins with an ellipsis, a substring
;; match is performed.
;; TIP: if the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; TIP: if the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.
(define-frame-preference "Default"
  ;; frame raise lock (lock AND raise == jumpto)
  (0 t nil :class "Konqueror" :role "...konqueror-mainwindow")
  (1 t nil :class "XTerm"))

(define-frame-preference "Ardour"
  (0 t   t   :instance "ardour_editor" :type :normal)
  (0 t   t   :title "Ardour - Session Control")
  (0 nil nil :class "XTerm")
  (1 t   nil :type :normal)
  (1 t   t   :instance "ardour_mixer")
  (2 t   t   :instance "jvmetro")
  (1 t   t   :instance "qjackctl")
  (3 t   t   :instance "qjackctl" :role "qjackctlMainForm"))

(define-frame-preference "Shareland"
  (0 t   nil :class "XTerm")
  (1 nil t   :class "aMule"))

(define-frame-preference "Emacs"
  (1 t t :restore "emacs-editing-dump" :title "...xdvi")
  (0 t t :create "emacs-dump" :class "Emacs"))

;; Turn on the modeline
(if (not (head-mode-line (current-head)))
    (toggle-mode-line (current-screen) (current-head)))

(setf stumpwm:*screen-mode-line-format*
      (list "%w | "
            '(:eval (stumpwm:run-shell-command "date" t))))

(set-fg-color "GreenYellow")
(set-bg-color "#001921")
(set-msg-border-width 1)
(set-border-color "#001921")
(setf *message-window-padding* 50)
(setf *message-window-gravity* :top
      *input-window-gravity* :top)

(set-font "-b&h-lucidatypewriter-medium-r-normal-sans-11-80-100-100-m-70-iso10646-1")
;;(set-unfocus-color "darkgray")
;;(setf *window-border-style* :thin)

