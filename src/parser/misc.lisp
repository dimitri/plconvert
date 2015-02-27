;;;
;;; Oracle PL/SQL quick parser
;;;

(in-package #:plconvert.parser)

(defrule single-line-comment (and "--" (* (not #\Newline)) #\Newline)
  (:constant :comment))

(defrule multi-line-comment (and "/*" (+ (not "*/")) "*/")
  (:constant :comment))

(defrule comments (or single-line-comment multi-line-comment))

(defrule whitespace (+ (or #\space #\tab #\newline #\linefeed #\return comments))
  (:constant 'whitespace))

(defrule ignore-whitespace (* whitespace)
  (:constant nil))

(defrule punct (or #\- #\_)
  (:text t))

(defrule namestring (and (or #\_ (alpha-char-p character))
			 (* (or (alpha-char-p character)
				(digit-char-p character)
				punct)))
  (:text t))

(defrule maybe-qualified-namestring (and namestring
                                         (? (and "." namestring))
                                         (? (and "." namestring)))
  (:lambda (pn)
    (destructuring-bind (a b c) pn
      (cond ((and a b c)
             (make-qname :schema a :package (second b) :name (second c)))
            ((and a b)
             (make-qname :schema nil :package a :name (second b)))
            (t
             (make-qname :schema nil :package nil :name a))))))

(defrule dec-number (and (? "-") (+ (digit-char-p character)))
  (:lambda (digits)
    (parse-integer (text digits))))

(defrule dollar-varname (and "$$" namestring)
  (:destructure (dollar name)
                (declare (ignore dollar))
                (list 'compiler-variable name)))

(defrule varname-%option (and namestring "%" namestring)
  ;; my_prov_status_list%ROWCOUNT
  ;; (rowcount my_prov_status_list)
  (:destructure (varname option) (list option varname)))

(defrule var  (or dollar-varname varname-%option maybe-qualified-namestring))

(defrule quoted-text (and "'" (* (or double-quote (not "'"))) "'")
  (:destructure (open text close) (declare (ignore open close)) (text text)))

(defrule double-quote "''" (:constant "'"))
