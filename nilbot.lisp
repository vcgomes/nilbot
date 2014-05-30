(defpackage #:nilbot
  (:use #:cl-irc #:cl)
  (:export :handle-message))

(in-package #:nilbot)

(defvar *connection*)
(defvar *process*)
(defparameter *server* "irc.freenode.net")
(defparameter *channel* "#nilbot")
(defparameter *botname* "nilbot-mark0")

(defun privmsg-hook (message)
  (let ((sender (first (irc:arguments message)))
        (text (second (irc:arguments message))))
    (let ((respond-to (if (string-equal sender *botname*)
                          (irc:source message)
                          sender)))
      (irc:privmsg (irc:connection message) respond-to
                   (handle-message (irc:source message) text)))))

(defun start-process (function)
  "Internal helper for the DEPRECATED function
START-BACKGROUND-MESSAGE-HANDLER and therefore DEPRECATED itself."
  (sb-thread:make-thread function))

(defun start-background-handler (connection)
  "Read messages from the `connection', parse them and dispatch
irc-message-event on them. Returns background process ID if available."
  (flet ((do-loop ()
           (loop
              (handler-bind
                  ((irc:no-such-reply
                    #'(lambda (c)
                        (declare (ignore c))
                        (invoke-restart 'continue))))
                (irc:read-message-loop connection)))))
    (start-process #'do-loop)))

(defun stop-background-handler (process)
  "Stops a background message handler process returned by the start function."
  (sb-thread:destroy-thread process))

(defun start ()
  (setf *connection* (irc:connect :nickname *botname*
                                  :server *server*
                                  :port *server-port*
                                  :connection-security :ssl))
  (irc:add-hook *connection* 'irc-privmsg-message #'privmsg-hook)
  (irc:join *connection* *channel*)
  (setf *process* (start-background-handler *connection*)))

(defun reload ()
  (when (null *process*)
    (error "Not connected."))
  (irc:remove-hooks *connection* 'irc-privmsg-message)
  (irc:add-hook *connection* 'irc-privmsg-message #'privmsg-hook))

(defun stop ()
  (irc:quit *connection*)
  (stop-background-handler *process*)
  (setf *process* nil))
