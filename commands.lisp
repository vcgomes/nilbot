(in-package #:nilbot)

(setf drakma:*header-stream* nil)
(push '("application" . "json") drakma:*text-content-types*)

(defvar *commands* (make-hash-table :test #'equal))
(defvar *dictionary* nil)

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

(defun dirty-chars (c)
  (case c
    ((#\\ #\") t)
    (otherwise nil)))

(defun handle-message (source text)
  (let* ((clean-string (remove-if #'dirty-chars text))
         (split (split-by-one-space clean-string))
         (args (command-trim split)))
    (if args
        (command-run source args)
        "")))

(defun split-by-one-space (string)
  (split-sequence:split-sequence #\Space string :remove-empty-subseqs t))

(defmacro defcommand (cmd args &rest body)
  `(progn
     (setf (gethash (string ',cmd) *commands*) ',cmd)
     (defun ,cmd ,args ,@body)))

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
  (nth-value 6 (get-decoded-time)))

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
    (loop for key being the hash-keys of *commands*
       do (push key acc))
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

(defcommand !copa (source args)
  (declare (ignorable source args))
  "tem bolo")

(defun stock-exchange-quote (secode code)
  (let ((request (format nil "http://finance.google.com/finance/info?client=ig\&q=~A:~A" code secode)))
    (let ((body (drakma:http-request request)))
      (if (= (length body) 0)
          nil
          (subseq body 5 (- (length body) 2))))))

(defun results-from-quote (quote)
  (let ((json (json:decode-json-from-string quote)))
    (values (cdr (assoc :l--cur json)) (cdr (assoc :c json)) (cdr (assoc :cp json)))))

(defun stock-exchange (args secode)
  (let* ((code (string-upcase (second args)))
         (result (stock-exchange-quote secode code)))
    (if (null result)
        (format nil "Invalid ~A code (~A)" secode code)
        (multiple-value-bind (cur c cp) (results-from-quote (stock-exchange-quote secode code))
          (format nil "~A:~A ~A ~A (~A%)" secode code cur c cp)))))


(defcommand !nasdaq (source args)
  (declare (ignorable source))
  (if (> (length args) 1)
      (stock-exchange args "NASDAQ")
      "Usage: !nasdaq <code>"))

(defcommand !bovespa (source args)
  (declare (ignorable source))
  (if (> (length args) 1)
      (stock-exchange args "BVMF")
      "Usage: !bovespa <code>"))

(defcommand !nyse (source args)
  (declare (ignorable source))
  (if (> (length args) 1)
      (stock-exchange args "NYSE")
      "Usage: !nyse <code>"))

(defcommand !intc (source args)
  (declare (ignorable source args))
  (let ((code "INTC")
        (secode "NASDAQ"))
    (multiple-value-bind (cur c cp) (results-from-quote (stock-exchange-quote secode code))
      (format nil "NASDAQ:~A ~A ~A (~A%)" code cur c cp))))

(defun load-dictionary ()
  (setf *dictionary* (cl-csv:read-csv (open "dictionary.csv"))))

(defun lookup-definition (str &optional (short t))
  (if (null *dictionary*)
      nil
      (loop for elem in *dictionary* do
           (when (and (= (length elem) 3)
                      (string-equal (string-upcase str) (string-upcase (first elem))))
             (return (if short
                         (second elem)
                         (third elem)))))))

(defun print-definition (what short)
  (let ((result (find-intel-definition what short)))
    (if (null result)
        (format nil "~A not found" what)
        (format nil "~A: ~A" what result))))

(defcommand !wtf (source args)
  (declare (ignorable source))
  (if (< (length args) 2)
      "Usage: !wtf <string>"
      (print-definition (second args) t)))

(defcommand !whatis (source args)
  (declare (ignorable source))
  (if (< (length args) 2)
      "Usage: !whatis <string>"
      (print-definition (second args) nil)))

(defun fortune-to-string ()
  (let* ((buf (make-string 512))
         (end (read-sequence buf (sb-ext:process-output
                                  (sb-ext:run-program "/usr/bin/fortune" '("-a" "-s") :output :stream)))))
    (subseq buf 0 (- end 1))))

(defcommand !fortune (source args)
  (declare (ignorable source args))
  (fortune-to-string))
