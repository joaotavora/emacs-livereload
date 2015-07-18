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
      ;; (livereload-close connection)
      )))

(cl-defmethod livereload--event ((_command (eql :info)) message connection)
  (livereload--message "%s sends info: %s!" connection message))

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
  (let ((slurped (with-temp-buffer
                   (insert-file-contents-literally
                    (concat livereload--package-directory "/livereload.js"))
                   (buffer-string))))
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
       (livereload--message "%s (unjacked to HTTP) saying bye bye" process change)
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

(cl-defun livereload (&optional (listen-port 35729))
  (interactive)
  (when (and livereload--server
             (process-live-p livereload--server))
    (livereload--message "deleting process %s first" livereload--server)
    (delete-process livereload--server))
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
         :service listen-port)))

(provide 'livereload)
;;; livereload.el ends here
