(let ((default-directory "~/.emacs.d/.cask/24.3.1/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))
(eval-after-load 'parenface
  (progn
    (set-face-foreground 'parenface-paren-face "red")
    (set-face-foreground 'parenface-bracket-face "red")
    (set-face-foreground 'parenface-curly-face "red")))
(require 'parenface)
