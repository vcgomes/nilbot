(in-package #:nilbot)

(defun command-p (string)
  (and (stringp string) (eql (char string 0) #\!)))

(defun command-trim (split)
  (if (or (null split) (command-p (car split)))
      split
      (command-trim (cdr split))))

(defun command-run (source args)
  (let ((sym (gethash (string-upcase (car args)) *commands*)))
    (when sym
      (funcall sym source args))))

(defun handle-message (source text)
  (let ((split (split-by-one-space text)))
    (let ((args (command-trim split)))
      (if args
	  (command-run source args)
          ""))))

(defun split-by-one-space (string)
  (split-sequence:split-sequence #\Space string :remove-empty-subseqs t))

(defmacro defcommand (cmd args &rest body)
  (defvar *commands* (make-hash-table :test #'equal))
  (setf (gethash (string cmd) *commands*) cmd)
  `(defun ,cmd ,args ,@body))

(defcommand !whoami (source args)
  (declare (ignorable args))
  (format nil "~A" source))

(defun safe-parse-integer (str &optional (default 0))
  (let ((num (parse-integer str :junk-allowed t)))
    (if (null num)
	default
	num)))

(defcommand !sum (source args)
  (declare (ignorable source))
  (format nil "sum is ~A" (apply #'+ (mapcar #'safe-parse-integer (cdr args)))))
