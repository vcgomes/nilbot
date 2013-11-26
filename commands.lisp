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
  "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defmacro defcommand (cmd args &rest body)
  (defvar *commands* (make-hash-table :test #'equal))
  (setf (gethash (string cmd) *commands*) cmd)
  `(defun ,cmd ,args ,@body))

(defcommand !whoami (source args)
  (declare (ignorable args))
  (format nil "~A" source))
