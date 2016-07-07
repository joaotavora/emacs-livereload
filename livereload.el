;;; livereload.el --- Livereload server for Emacs    -*- lexical-binding: t; -*-

;; Copyright (C) 2015  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'eieio)
(require 'websocket)
(require 'json)

(defvar livereload--package-directory (file-name-directory load-file-name))

(defvar livereload--server nil)
(defvar livereload--connections nil)

(defun livereload--message (control &rest args)
  (message "[livereload] %s" (apply #'format control args)))

(defun livereload--debug (control &rest args)
  (message "[livereload, debug] %s" (apply #'format control args)))

(defun livereload--opened (connection)
  (livereload--message "%s being told hello!" connection)
  (websocket-send-text (process-get connection :websocket)
                       (json-encode '((command . "hello")
                                      (protocols . ["http://livereload.com/protocols/connection-check-1" "http://livereload.com/protocols/official-7"])
                                      (serverName . "Emacs livereload"))))
  (push connection livereload--connections))

(defun livereload--closed (connection)
  (setq livereload--connections (remove connection livereload--connections))
  (delete-process connection)
  (livereload--message "%s closed!" connection))

(defun livereload--keywordize (string)
  (intern (concat ":"
                  (replace-regexp-in-string "_" "-" string))))

(defun livereload--process (connection message)
  (let ((command (alist-get 'command message)))
    (unless command (error "no command in %s!" message))
    (livereload--event (livereload--keywordize command) message connection)))

(defun livereload--handle-websocket-error (connection &rest args)
  (apply #'websocket-default-error-handler (process-get connection :websocket) args))

(cl-defgeneric livereload--event (command message connection))

(cl-defmethod livereload--event ((_command (eql :hello)) message connection)
  (livereload--message "%s client says hello: %s!" connection message)
  (let* ((client-protocols (alist-get 'protocols message))
         (preferred (aref client-protocols (1- (length client-protocols)))))
    (when (and preferred
               (string-match "connection-check-1" preferred))
      (livereload--message "%s connection-check left to linger!" connection message)
      (process-put connection 'livereload--connection-check message)
      ;; (livereload--message "%s connection-check killed!" connection message)
      ;; (livereload-close connection)
      )))

(cl-defmethod livereload--event ((_command (eql :info)) message connection)
  (livereload--message "%s sends info: %s!" connection message)
  (let ((url (alist-get 'url message)))
    (process-put connection 'livereload--url url)
    (livereload--debug "%s registering url %s" connection url)))

(defun livereload--prompt-for-connection ()
  (unless livereload--connections
    (error "No livereload connections active"))
  (let* ((names (mapcar (lambda (conn)
                          (propertize (format "server:%s" (process-contact conn :service))
                                      'livereload--connection conn))
                        livereload--connections))
         (chosen (ido-completing-read "Which server? " names nil t)))
    (get-text-property 0 'livereload--connection chosen)))

(defun livereload-close (connection)
  (interactive
   (list (livereload--prompt-for-connection)))
  (websocket-close (process-get connection :websocket)))

(defun livereload--send-livereload-js-file (process)
  (let* ((file (concat livereload--package-directory "/livereload.js"))
         (slurped (with-temp-buffer
                   (insert-file-contents-literally file)
                   (buffer-string))))
    (livereload--message "%s sending %s " process file)
    (process-send-string
     process
     (concat "HTTP/1.1 200 OK\r\n"
             "Connection: close\r\n"
             "Content-Type: application/x-javascript\r\n"
             (format "Content-Length: %d\r\n" (string-bytes slurped))
             "\r\n"
             slurped))))

(defun livereload--unjack-websocket (process incoming)
  (livereload--message "%s being unjacked to HTTP since %s"
                       process 
                       (save-match-data
                         (let ((string incoming))
                           (string-match "^GET.*\r\n" string)
                           (match-string 0 string))))
  (let ((websocket (process-get process :websocket)))
    (unless websocket
      (error "Cannot unjack websocket process %s: no associated websocket!" process))
    (setq websocket-server-websockets
          (delq websocket websocket-server-websockets))
    (process-put process :websocket nil)
    (set-process-sentinel
     process
     (lambda (process change)
       (livereload--message "%s (unjacked to HTTP) closing, since %s" process change)
       (set-process-sentinel process nil)
       (delete-process process)))
    (livereload--send-livereload-js-file process)))

(defun livereload--server-filter (process incoming)
  (if (string-match
       (concat "GET /livereload.js.* HTTP/1.1\r\n"
               "\\([[:alpha:]-]+: .+\r\n\\)+"
               "\r\n")
       incoming)
      (livereload--unjack-websocket process incoming)
    (websocket-server-filter process incoming)))

(defun livereload-shutdown-server ()
  (interactive)
  (delete-process livereload--server)
  (livereload--message "Server shutdown"))

(cl-defun livereload-start-server (&optional (listen-port 35729))
  (interactive)
  (when (and livereload--server
             (process-live-p livereload--server))
    (livereload--message "deleting process %s first" livereload--server)
    (livereload-shutdown-server))
  (setq livereload--server
        ;; we can't use `websocket-server' directly, since we want
        ;; to use a slightly different filter function that serves
        ;; livereload.js via plain HTTP
        ;; 
        (make-network-process
         :name "lrconn"
         :server t
         :family 'ipv4
         :filter 'livereload--server-filter
         :log 'websocket-server-accept
         :filter-multibyte nil
         :plist (list :on-open (lambda (ws)
                                 (livereload--opened (websocket-conn ws)))
                      :on-message (lambda (ws f)
                                    (livereload--process (websocket-conn ws)
                                                         (json-read-from-string
                                                          (websocket-frame-payload f))))
                      :on-close (lambda (ws)
                                  (livereload--closed (websocket-conn ws)))
                      :on-error (lambda (ws &rest args)
                                  (apply #'livereload--handle-websocket-error (websocket-conn ws) args)))
         :service listen-port
         :sentinel (lambda (process change)
                     (livereload--message "%s changed state to %s. Killing." process change)
                     (set-process-sentinel process nil)
                     (delete-process process))))
  (livereload--message "%s listening on %s" livereload--server listen-port))

(define-minor-mode livereload-mode
  "Toggle file change notification service LiveReload."
  :global t :group 'convenience
  (cond (livereload-mode
         (livereload-start-server)
         (add-hook 'after-save-hook 'livereload--notify-maybe 'append))
        (t
         (livereload-shutdown-server)
         (mapc #'livereload-close livereload--connections)
         (remove-hook 'after-save-hook 'livereload--notify-maybe))))


;;; Event stuff
;;;

(defvar livereload--calculated-targets '())
(make-variable-buffer-local 'livereload--calculated-targets)

(defvar livereload-potential-targets
  'livereload-default-potential-targets
  "Identifies target URLs based on visited URL and current buffer.

Can be a list of strings, the symbol t, or a symbol denoting a
function of one argument producing one of the two preceding
types.

A list of strings identifies target URLs that livereload clients
connected to Emacs are told to reload. If it is empty (or nil)
saving the buffer will never cause clients to be notified.

If a function, the argument passed to the function is the URL of
the webpage that a livereload client is visiting.

If t, it is up to any functions in `livereload-notify-hook',
which see, to compute and perform any notifications.

This variable is most likely useful if set buffer-locally.

The default value is the function
`livereload-default-potential-targets' which considers the
visited URL argument as well as the name and type of the buffer
being saved to disk, returning target URLs that it believes need
updating based on a simple heuristic. It is perhaps only suited
for static websites of plain HTML and CSS files.

More sophisticated functions may better guess the target URLs
that need to be reloaded. Such a function might, for example,
scan the buffer's contents to discover these targets, or even
request via HTTP the URL being visited to scan the resulting
HTML code.

Another, simpler alternative, might be to manually set this
variable file-locally to a list of strings.  See Info node
`Specifying File Variables'.

Finally, setting this variable to t is useful if the target URLs
cannot be discovered a priori. They may, for example, be some
preprocessing to be done on the saved file by the functions in
`livereload-notify-hook' and only from the results of that
processing can the target URLs be revealed.")

(defvar livereload-notify-hook nil
  "Hooks to run before notifying livereload clients.

Each hook is a unary function that returns a boolean. The single
argument passed to the function is a list of visited URL's for
which `livereload-potentitial-targets' identified a need of
reloading.

If the hook is empty, or every function returns nil,
`livereload-notify' is automatically called at the end with no
arguments, meaning that connected clients are told to reload any
targets previously identified via `livereload-potential-targets'.

A function placed in this hook that returns non-nil prevents the
rest of hook from running as well as this default behaviour.

Such a function would normally be responsible, unless it wants to
abort the notification process, for eventually calling
`livereload-notify' itself, regardless of whether it chooses to
call `livereload-notify' immediately, or somehow schedule a call
to it in the future.")

(defun livereload--notify-maybe ()
  (let* ((calculated
          (cl-loop for conn in livereload--connections
                   for url = (process-get conn 'livereload--url)
                   for targets = (and url
                                      (if (and (not (eq t livereload-potential-targets))
                                               (symbolp livereload-potential-targets))
                                          (funcall livereload-potential-targets url)
                                        livereload-potential-targets))
                   when targets
                   collect (list url conn targets))))
    (setq-local livereload--calculated-targets calculated)
    (cond (calculated
           (let ((hookage (run-hook-with-args-until-success
                           'livereload-notify-hook
                           (mapcar
                            #'car livereload--calculated-targets))))
             (unless hookage
               (livereload-notify))))
          (t
           (livereload--debug "No one to notify for %s" (current-buffer))))))

(defun livereload-notify (&optional targets)
  (cl-loop
   for (_url conn calculated-targets) in livereload--calculated-targets
   do (cl-loop for target in (or targets
                                 (unless (eq calculated-targets t)
                                   calculated-targets))
               do (websocket-send-text
                   (process-get conn :websocket)
                   (json-encode `((command . :reload)
                                  (path . ,target)
                                  (liveCSS . t))))))
  ;; prevent accidental overcalling of `livereload-notify'
  (setq livereload--calculated-targets nil))

(defun livereload-default-potential-targets (url)
  (when (and (string-match "^\\(https?://\\(?:localhost\\|127.0.0.1\\)\\(?::[[:digit:]]+\\)?\\)\\(.*\\)"
                           url)
             buffer-file-name
             (save-match-data
               (string-match "\\(css\\|\\js\\|html?\\)" (file-name-extension buffer-file-name))))
    (let* ((_address (match-string 1 url))
           (path (match-string 2 url))
           (project-dir (and buffer-file-name
                             (locate-dominating-file buffer-file-name ".git")))
           (relative (and project-dir
                          (substring buffer-file-name
                                     (cl-mismatch project-dir buffer-file-name)))))
      (cond ((and relative
                  (string-match "\\(css\\|\\js\\)" (file-name-extension relative)))
             (list relative))
            ((and relative
                  (string-match "index.html?" (file-name-nondirectory relative)))
             (let ((relative-path (concat "/"
                                          (file-name-directory relative))))
               (when (string= (regexp-quote relative-path) path)
                 (list relative-path))))))))



(provide 'livereload)
;;; livereload.el ends here
