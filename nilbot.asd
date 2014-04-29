(defpackage #:nilbot-system
  (:use #:cl #:asdf))

(in-package :nilbot-system)

(defsystem :nilbot
  :name "nilbot"
  :description "nilbot: Nilbot Is a Lisp irc BOT"
  :version "0.1"
  :author "Vinicius Gomes <vcgomes@hostisdown.org>"
  :licence "MIT"
  :depends-on (:cl-irc
	       :lisp-unit)
  :serial t
  :components ((:file "nilbot")
	       (:file "commands")
	       (:file "nilbot-tests")))
