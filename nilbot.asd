(in-package :cl-user)

(defpackage #:nilbot-system
  (:use #:cl #:asdf))

(defpackage #:nilbot-tests
  (:use #:cl #:lisp-unit))

(defpackage #:nilbot
  (:use #:cl-irc #:cl)
  (:export :handle-message))

(in-package :nilbot-system)

(defsystem nilbot
  :name "nilbot"
  :description "nilbot: Nilbot Is a Lisp irc BOT"
  :version "0.1"
  :author "Vinicius Gomes <vcgomes@hostisdown.org>"
  :licence "MIT"
  :depends-on (#:cl-irc
	       #:lisp-unit)
  :serial t
  :components ((:file "commands")
               (:file "nilbot")
	       (:file "nilbot-tests")))
