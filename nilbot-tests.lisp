(in-package :nilbot-tests)

(define-test test-whoami
  (assert-equal "test" (nilbot:handle-message "test" "!whoami")))
