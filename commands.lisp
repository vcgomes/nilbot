(in-package #:nilbot)

(setf drakma:*header-stream* nil)
(push '("application" . "json") drakma:*text-content-types*)

(defun command-p (string)
  (and (stringp string) (eql (char string 0) #\!)))

(defun command-trim (split)
  (if (or (null split) (command-p (car split)))
      split
      (command-trim (cdr split))))

(defun command-run (source args)
  (let ((sym (gethash (string-upcase (car args)) *commands*)))
    (if sym
      (funcall sym source args)
      "")))

(defun handle-message (source text)
  (let* ((split (split-by-one-space text))
         (args (command-trim split)))
    (if args
        (command-run source args)
        "")))

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

(defun get-day-of-week ()
  (multiple-value-bind
        (second minute hour date month year day daylight-p zone) (get-decoded-time)
    day))

(defun theme-at-s-u (day-of-week)
  (case day-of-week
    ((5 6) "closed?")
    (0 "no theme")
    (1 "arabian")
    (2 "feijoada")
    (3 "asian")
    (4 "mexican")))

(defcommand !s-u (source args)
  (declare (ignorable source args))
  (format nil "today's theme at s-u is ~A" (theme-at-s-u (get-day-of-week))))

(defcommand !dance (source args)
  (declare (ignorable source args))
  "<(*.*<) (^*.*^) (>*.*)>")

(defcommand !help (source args)
  (declare (ignorable source args))
  (let ((acc nil))
    (maphash #'(lambda (key value) (push (string-downcase key) acc)) *commands*)
    (format nil "~{~A~^, ~}" acc)))

(defcommand !oka (source args)
  (declare (ignorable source args))
  "valeu")

(defun get-result-from-team (team)
  (cons (cdr (assoc :country team)) (cdr (assoc :goals team))))

(defun get-matches-results (matches)
  (let ((result nil))
    (dolist (match matches result)
      (let ((home (get-result-from-team (cdr (assoc :home--team match))))
	    (away (get-result-from-team (cdr (assoc :away--team match)))))
	(push (format nil "~A ~A x ~A ~A" (car home) (cdr home) (cdr away) (car away))
	      result)))))

(defun worldcup-today-to-json ()
  (json:decode-json-from-string
   (drakma:http-request "http://worldcup.sfg.io/matches/today")))

(defcommand !worldcup (source args)
  (declare (ignorable source args))
    (format nil "today's results: ~{~A~^, ~}" (get-matches-results (worldcup-today-to-json))))
