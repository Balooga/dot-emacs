(defun dot-run-maven ()
  (interactive)
  (when (executable-find "mvn")
    (shell-command-to-string (concat "mvn"
                                     " -f "
                                     (projectile-project-root)
                                     "pom.xml"
                                     " install"))))

(defconst puml-interactive-preview-buffer "*PUML-interactive Preview*")

(defun dot-impatient-sentinel (http-buffer puml-input-buffer)
  (lambda (ps event)
    (unless (equal event "finished\n")
      (error "PUML Preview failed: %s" event))

    (with-current-buffer puml-input-buffer
      (base64-encode-region (point-min) (point-max))
      (goto-char (point-min))
      (insert 
       "<html><body>"
       "<img src=\"data:image/png;base64,")
      (goto-char (point-max))
      (insert
       "\">"
       "<p>"
       "This is a test"
       "</p>"
       "</body></html>")
      (let ((id (number-to-string imp-last-state))
            (user-filter imp-user-filter))
        (with-temp-buffer
          (if user-filter
              (funcall user-filter buffer)
            (insert-buffer-substring buffer))
          (httpd-send-header (pop imp-client-list) "text/html" 200
                             :Cache-Control "no-cache"
                             :X-Imp-Count id)))
      )
    (with-current-buffer http-buffer
      (insert-buffer-substring puml-input-buffer))))


;;;###autoload
(define-minor-mode puml-impatient-mode
  "Serves the buffer live over HTTP, specifically for PlantUML files."
  :lighter " puml-imp" 
  (imp--on-change)
  (imp-remove-user-filter))

;;(defalias 'puml-preview 'imp--on-change)
(global-set-key (kbd "C-c C-e") 'puml-immediate-mode-execute)

(defun puml-immediate-mode-config ()
  (interactive)
  (remove-hook 'after-change-functions 'imp--on-change nil t))

(defun puml-immediate-mode-execute ()
  (interactive)
  (imp--on-change))

(defun impatient-puml-filter (puml-buffer)
  ;; dot-puml-impatient-mode      
  (interactive "p")
  (message "impatient-puml-filter")
  (let* ((process-connection-type nil) 
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (current (current-buffer)))

    (with-temp-buffer
      (let ((temp (current-buffer)))

        (with-current-buffer puml-buffer
          (call-process-region (point-min) (point-max)
                               "java" 
                               nil
                               temp
                               nil
                               ;;"-Djava.awt.headless=true"
                               "-jar" (shell-quote-argument puml-plantuml-jar-path)
                               (puml-output-type-opt)
                               "-p"))

        (base64-encode-region (point-min) (point-max))
        (goto-char (point-min))
        (insert 
         "<html><body>"
         "<img src=\"data:image/png;base64,")
        (goto-char (point-max))
        (insert
         "\">" 
         "</body></html>")

        (with-current-buffer current
          (insert-buffer-substring temp))))))

;; (set-process-sentinel ps
;;                       (lambda (ps event)
;;                         (unless (equal event "finished\n")
;;                           (error "PUML Preview failed: %s" event))
;;                         (cond
;;                          ((= prefix 16)
;;                           (window-buffer (selected-window))
;;                           (unless (get-buffer-window puml-preview-buffer 'all-frames)
;;                             (switch-to-buffer-other-frame puml-preview-buffer)))
;;                          ((= prefix 4)
;;                           (switch-to-buffer-other-window puml-preview-buffer))
;;                          (t
;;                           (unless (get-buffer-window puml-preview-buffer 'all-frames)
;;                             (switch-to-buffer puml-preview-buffer))))
;;                         (with-current-buffer puml-preview-buffer
;;                           (when imagep
;;                             (message "Here we are 2" )
;;                             (image-mode)
;;                             (set-buffer-multibyte t)))
;;                         )))))

(defun puml-preview (prefix)
  ;; dot-puml-impatient-mode      
  (interactive "p")
  (let* ((process-connection-type nil)
         (buf (if (get-buffer puml-interactive-preview-buffer)
                  (get-buffer puml-interactive-preview-buffer)
                (get-buffer-create puml-interactive-preview-buffer)))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary))

    (with-current-buffer buf
      (erase-buffer))

    (let ((ps (start-process "PUML" buf "java"
                             ;;"-Djava.awt.headless=true"
                             "-jar" (shell-quote-argument puml-plantuml-jar-path) 
                             (puml-output-type-opt) "-p")))
      
      (process-send-region ps (point-min) (point-max))
      (process-send-eof ps)
      
      (set-process-sentinel ps
                            (lambda (ps event)
                              (unless (equal event "finished\n")
                                (error "PUML Preview failed: %s" event)) 

                              (with-current-buffer buf
                                (combine-after-change-calls
                                  (base64-encode-region (point-min) (point-max))
                                  (goto-char (point-min))
                                  (insert 
                                   "<html><body>"
                                   "<img src=\"data:image/png;base64,")
                                  (goto-char (point-max))
                                  (insert
                                   "\">" 
                                   "</body></html>"))
                                ))))))

(defvar puml-preview-state nil)
(make-local-variable 'puml-preview-state)

(defun puml-preview (prefix)
  "Preview diagram, using prefix (as PREFIX) to choose where to display it:
- 4  (when prefixing the command with C-u) -> new window
- 16 (when prefixing the command with C-u C-u) -> new frame.
- else -> new buffer"
  (interactive "p")
  
  (let* ((process-connection-type nil)
         (imagep (and (display-images-p)
                      (puml-is-image-output-p)))
         (coding-system-for-read (and imagep 'binary))
         (coding-system-for-write (and imagep 'binary))
         
         (buf (if (get-buffer puml-preview-buffer)
                  (get-buffer puml-preview-buffer)
                (get-buffer-create puml-preview-buffer))))
    
    (with-current-buffer puml-preview-buffer
      (if (equal major-mode 'image-mode)
          (image-mode-as-text))
      (erase-buffer)
      (fundamental-mode))

    (let ((ps (start-process "PUML" buf
                             "java" "-Djava.awt.headless=true"
                             "-jar" (shell-quote-argument puml-plantuml-jar-path) 
                             (puml-output-type-opt) "-p")))
      (process-send-region ps (point-min) (point-max))
      (process-send-eof ps)
      (set-process-sentinel ps
                            (lambda (ps event)
                              (unless (equal event "finished\n")
                                (error "PUML Preview failed: %s" event))
                              
                              (with-current-buffer puml-preview-buffer
                                (when imagep
                                  (image-mode)
                                  (set-buffer-multibyte t)))

                              (unless (get-buffer-window puml-preview-buffer 'all-frames)
                                (switch-to-buffer-other-frame puml-preview-buffer)))))
    ))

;; (insert 
;;  "<html><body>"
;;  "<img src=\"data:image/png;base64,")
;; (insert-buffer-substring buf)
;; (insert
;;  "\">"
;;  "</body></html>")))

;;(insert-buffer-substring buf)

;; (princ (format  "<html><body>Testing...</body></html>")
;;        buf)

;; (princ (concat
;;         "<html><body>"
;;         "<p>"
;;         "Testing"
;;         "</p>"
;;         "</body></html>")
;;        buf)

;; (with-current-buffer buf
;;   (base64-encode-region (point-min) (point-max))
;;   ;; (goto-char (point-min))
;;   ;; (insert 
;;   ;;  "<html><body>"
;;   ;;  "<img src=\"data:image/png;base64,")
;;   ;; (goto-char (point-max))
;;   ;; (insert
;;   ;;  "\">"
;;   ;;  "</body></html>")
;;   )

;; (mapc #'(lambda (str)
;;           (princ str ;; (current-buffer)
;;                  buf))
;;       "<html><body>"
;;       "<p>"
;;       "This is a test"
;;       "</p>"
;;       "</body></html>" 
;;       )



;; (let ((count 
;;        (with-current-buffer buf
;;          (count-words-region (point-min) (point-max)))))
;;   (princ (format  "<html><body>Characters: - %d</body></html>" count)
;;          (current-buffer)))
