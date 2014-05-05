(defpackage #:nilbot-tests
  (:use #:cl #:lisp-unit))

(in-package :nilbot-tests)

(define-test test-whoami
  (assert-equal "test" (nilbot:handle-message "test" "!whoami")))

(define-test test-sum
  (assert-equal "sum is 6" (nilbot:handle-message "test" "!sum 1 2 3"))
  (assert-equal "sum is 7" (nilbot:handle-message "test" "this should not matter !sum 2 5"))
  (assert-equal "sum is 10" (nilbot:handle-message "test" "!sum 3 bla 7")))
