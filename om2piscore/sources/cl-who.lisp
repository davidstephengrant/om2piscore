;;; Copyright (c) 2003-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)

(defpackage :cl-who
  (:use :cl)
  (:nicknames :who)
  #+:sbcl (:shadow :defconstant)
  #+:sb-package-locks (:lock t)
  (:export :*attribute-quote-char*
           :*escape-char-p*
           :*prologue*
           :*downcase-tokens-p*
           :*html-no-indent-tags*
           :*html-empty-tags*
           :*html-empty-tag-aware-p*
           :conc
           :convert-attributes
           :convert-tag-to-string-list
           :esc
           :escape-char
           :escape-char-all
           :escape-char-iso-8859-1
           :escape-char-minimal
           :escape-char-minimal-plus-quotes
           :escape-string
           :escape-string-all
           :escape-string-iso-8859-1
           :escape-string-minimal
           :escape-string-minimal-plus-quotes
           :fmt
           :htm
           :html-mode
           :str
           :with-html-output
           :with-html-output-to-string))

(pushnew :cl-who *features*)

(in-package :cl-who)

#+:sbcl
(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defvar *prologue*
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
  "This is the first line that'll be printed if the :PROLOGUE keyword
argument is T")

(defvar *escape-char-p*
  (lambda (char)
    (or (find char "<>&'\"")
        (> (char-code char) 127)))
  "Used by ESCAPE-STRING to test whether a character should be escaped.")

(defvar *indent* nil
  "Whether to insert line breaks and indent. Also controls amount of
indentation dynamically.")

(defvar *html-mode* :xml
  ":SGML for \(SGML-)HTML, :XML \(default) for XHTML, :HTML5 for HTML5.")

(defvar *downcase-tokens-p* t
  "If NIL, a keyword symbol representing a tag or attribute name will
not be automatically converted to lowercase.  This is useful when one
needs to output case sensitive XML.")

(defvar *attribute-quote-char* #\'
  "Quote character for attributes.")

(defvar *empty-tag-end* " />"
  "End of an empty tag.  Default is XML style.")

(defvar *html-no-indent-tags*
  '(:pre :textarea)
  "The list of HTML tags that should disable indentation inside them. The initial
value is a list containing only :PRE and :TEXTAREA.")

(defvar *html-empty-tags*
  '(:area
    :atop
    :audioscope
    :base
    :basefont
    :br
    :choose
    :col
    :command
    :embed
    :frame
    :hr
    :img
    :input
    :isindex
    :keygen
    :left
    :limittext
    :link
    :meta
    :nextid
    :of
    :over
    :param
    :range
    :right
    :source
    :spacer
    :spot
    :tab
    :track
    :wbr)
  "The list of HTML tags that should be output as empty tags.
See *HTML-EMPTY-TAG-AWARE-P*.")

(defvar *html-empty-tag-aware-p* t
  "Set this to NIL to if you want to use CL-WHO as a strict XML
generator.  Otherwise, CL-WHO will only write empty tags listed
in *HTML-EMPTY-TAGS* as <tag/> \(XHTML mode) or <tag> \(SGML
mode and HTML5 mode).  For all other tags, it will always generate
<tag></tag>.")

(defconstant +newline+ (make-string 1 :initial-element #\Newline)
  "Used for indentation.")

(defconstant +spaces+ (make-string 2000
                                   :initial-element #\Space
                                   :element-type 'base-char)
  "Used for indentation.")

(in-package :cl-who)

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'lw:with-unique-names))

#-:lispworks
(defmacro with-unique-names ((&rest bindings) &body body)
  "Syntax: WITH-UNIQUE-NAMES ( { var | (var x) }* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar #'(lambda (binding)
                     (check-type binding (or cons symbol))
                     (if (consp binding)
                       (destructuring-bind (var x) binding
                         (check-type var symbol)
                         `(,var (gensym ,(etypecase x
                                          (symbol (symbol-name x))
                                          (character (string x))
                                          (string x)))))
                       `(,binding (gensym ,(symbol-name binding)))))
                 bindings)
         ,@body))

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (macro-function 'with-rebinding)
          (macro-function 'lw:rebinding)))

#-:lispworks
(defmacro with-rebinding (bindings &body body)
  "WITH-REBINDING ( { var | (var prefix) }* ) form*

Evaluates a series of forms in the lexical environment that is
formed by adding the binding of each VAR to a fresh, uninterned
symbol, and the binding of that fresh, uninterned symbol to VAR's
original value, i.e., its value in the current lexical environment.

The uninterned symbol is created as if by a call to GENSYM with the
string denoted by PREFIX - or, if PREFIX is not supplied, the string
denoted by VAR - as argument.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3wv0fya0p.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  (loop for binding in bindings
        for var = (if (consp binding) (car binding) binding)
        for name = (gensym)
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let ,renames
                          (with-unique-names ,bindings
                            `(let (,,@temps)
                              ,,@body))))))

;; TODO...
#+(or)
(defun apply-to-tree (function test tree)
  (declare (optimize speed space))
  (declare (type function function test))
  "Applies FUNCTION recursively to all elements of the tree TREE \(not
only leaves) which pass TEST."
  (cond
    ((funcall test tree)
      (funcall function tree))
    ((consp tree)
      (cons
       (apply-to-tree function test (car tree))
       (apply-to-tree function test (cdr tree))))
    (t tree)))

(defmacro n-spaces (n)
  "A string with N spaces - used by indentation."
  `(make-array ,n
               :element-type 'base-char
               :displaced-to +spaces+
               :displaced-index-offset 0))

(declaim (inline escape-char))
(defun escape-char (char &key (test *escape-char-p*))
  (declare (optimize speed))
  "Returns an escaped version of the character CHAR if CHAR satisfies
the predicate TEST.  Always returns a string."
  (if (funcall test char)
    (case char
      (#\< "&lt;")
      (#\> "&gt;")
      (#\& "&amp;")
      (#\' "&#039;")
      (#\" "&quot;")
      (t (format nil (if (eq *html-mode* :xml) "&#x~x;" "&#~d;")
                 (char-code char))))
    (make-string 1 :initial-element char)))

(defun escape-string (string &key (test *escape-char-p*))
  (declare (optimize speed))
  "Escape all characters in STRING which pass TEST. This function is
not guaranteed to return a fresh string.  Note that you can pass NIL
for STRING which'll just be returned."
  (let ((first-pos (position-if test string))
        (format-string (if (eq *html-mode* :xml) "&#x~x;" "&#~d;")))
    (if (not first-pos)
      ;; nothing to do, just return STRING
      string
      (with-output-to-string (s)
        (loop with len = (length string)
              for old-pos = 0 then (1+ pos)
              for pos = first-pos
                  then (position-if test string :start old-pos)
              ;; now the characters from OLD-POS to (excluding) POS
              ;; don't have to be escaped while the next character has to
              for char = (and pos (char string pos))
              while pos
              do (write-sequence string s :start old-pos :end pos)
                 (case char
                   ((#\<)
                     (write-sequence "&lt;" s))
                   ((#\>)
                     (write-sequence "&gt;" s))
                   ((#\&)
                     (write-sequence "&amp;" s))
                   ((#\')
                     (write-sequence "&#039;" s))
                   ((#\")
                     (write-sequence "&quot;" s))
                   (otherwise
                     (format s format-string (char-code char))))
              while (< (1+ pos) len)
              finally (unless pos
                        (write-sequence string s :start old-pos)))))))

(defun minimal-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-MINIMAL functions to determine
whether CHAR must be escaped."
  (find char "<>&"))

(defun escape-char-minimal (char)
  "Escapes only #\<, #\>, and #\& characters."
  (escape-char char :test #'minimal-escape-char-p))

(defun escape-string-minimal (string)
  "Escapes only #\<, #\>, and #\& in STRING."
  (escape-string string :test #'minimal-escape-char-p))

(defun minimal-plus-quotes-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-MINIMAL-PLUS-QUOTES functions to
determine whether CHAR must be escaped."
  (find char "<>&'\""))

(defun escape-char-minimal-plus-quotes (char)
  "Like ESCAPE-CHAR-MINIMAL but also escapes quotes."
  (escape-char char :test #'minimal-plus-quotes-escape-char-p))

(defun escape-string-minimal-plus-quotes (string)
  "Like ESCAPE-STRING-MINIMAL but also escapes quotes."
  (escape-string string :test #'minimal-plus-quotes-escape-char-p))

(defun iso-8859-1-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-ISO-8859-1 functions to
determine whether CHAR must be escaped."
  (or (find char "<>&'\"")
      (> (char-code char) 255)))

(defun escape-char-iso-8859-1 (char)
  "Escapes characters that aren't defined in ISO-8859-9."
  (escape-char char :test #'iso-8859-1-escape-char-p))

(defun escape-string-iso-8859-1 (string)
  "Escapes all characters in STRING which aren't defined in ISO-8859-1."
  (escape-string string :test #'iso-8859-1-escape-char-p))

(defun non-7bit-ascii-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-ISO-8859-1 functions to
determine whether CHAR must be escaped."
  (or (find char "<>&'\"")
      (> (char-code char) 127)))

(defun escape-char-all (char)
  "Escapes characters which aren't in the 7-bit ASCII character set."
  (escape-char char :test #'non-7bit-ascii-escape-char-p))

(defun escape-string-all (string)
  "Escapes all characters in STRING which aren't in the 7-bit ASCII
character set."
  (escape-string string :test #'non-7bit-ascii-escape-char-p))

(defun extract-declarations (forms)
  "Given a FORM, the declarations - if any - will be extracted
   from the head of the FORM, and will return two values the declarations,
   and the remaining of FORM"
  (loop with declarations
        for forms on forms
        for form = (first forms)
        while (and (consp form)
                   (eql (first form) 'cl:declare))
        do (push form declarations)
     finally (return (values (nreverse declarations) forms))))

(in-package :cl-who)

(defun html-mode ()
  "Returns the current HTML mode. :SGML for \(SGML-)HTML, :XML for
XHTML and :HTML5 for HTML5 (HTML syntax)."
  *html-mode*)

(defun (setf html-mode) (mode)
  "Sets the output mode to XHTML or \(SGML-)HTML.  MODE can be
:SGML for HTML, :XML for XHTML or :HTML5 for HTML5 (HTML syntax)."
  (ecase mode
    ((:sgml)
     (setf *html-mode* :sgml
           *empty-tag-end* ">"
           *prologue* "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"))
    ((:xml)
     (setf *html-mode* :xml
           *empty-tag-end* " />"
           *prologue* "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))
    ((:html5)
     (setf *html-mode* :html5
           *empty-tag-end* ">"
           *prologue* "<!DOCTYPE html>"))))

(defun process-tag (sexp body-fn)
  (declare (optimize speed space))
  "Returns a string list corresponding to the `HTML' \(in CL-WHO
syntax) in SEXP.  Uses the generic function CONVERT-TO-STRING-LIST
internally.  Utility function used by TREE-TO-TEMPLATE."
  (let (tag attr-list body)
    (cond
      ((keywordp sexp)
       (setq tag sexp))
      ((atom (first sexp))
       (setq tag (first sexp))
       ;; collect attribute/value pairs into ATTR-LIST and tag body (if
       ;; any) into BODY
       (loop for rest on (cdr sexp) by #'cddr
             if (keywordp (first rest))
               collect (cons (first rest) (second rest)) into attr
             else
               do (progn (setq attr-list attr)
                         (setq body rest)
                         (return))
             finally (setq attr-list attr)))
      ((listp (first sexp))
       (setq tag (first (first sexp)))
       (loop for rest on (cdr (first sexp)) by #'cddr
             if (keywordp (first rest))
               collect (cons (first rest) (second rest)) into attr
             finally (setq attr-list attr))
       (setq body (cdr sexp))))
    (convert-tag-to-string-list tag attr-list body body-fn)))

(defun convert-attributes (attr-list)
  "Helper function for CONVERT-TAG-TO-STRING-LIST which converts the
alist ATTR-LIST of attributes into a list of strings and/or Lisp
forms."
  (declare (optimize speed space))
  (loop with =var= = (gensym)
        for (orig-attr . val) in attr-list
        for attr = (if *downcase-tokens-p*
                     (string-downcase orig-attr)
                     (string orig-attr))
        unless (null val) ;; no attribute at all if VAL is NIL
          if (constantp val)
            if (and (eq *html-mode* :sgml) (eq val t)) ; special case for SGML
              nconc (list " " attr)
            else
              nconc (list " "
                          ;; name of attribute
                          attr
                          (format nil "=~C" *attribute-quote-char*)
                          ;; value of attribute
                          (cond ((eq val t)
                                 ;; VAL is T, use attribute's name
                                 attr)
                                (t
                                 ;; constant form, PRINC it -
                                 ;; EVAL is OK here because of CONSTANTP
                                 (format nil "~A" (eval val))))
                          (string *attribute-quote-char*))
            end
          else
            ;; do the same things as above but at runtime
            nconc (list `(let ((,=var= ,val))
                          (cond ((null ,=var=))
                                ((eq ,=var= t)
                                 ,(case *html-mode*
                                    (:sgml
                                     `(fmt " ~A" ,attr))
                                    ;; otherwise default to :xml mode
                                    (t
                                     `(fmt " ~A=~C~A~C"
                                           ,attr
                                           *attribute-quote-char*
                                           ,attr
                                           *attribute-quote-char*))))
                                (t
                                 (fmt " ~A=~C~A~C"
                                      ,attr
                                      *attribute-quote-char*
                                      ,=var=
                                      *attribute-quote-char*)))))))

(defgeneric convert-tag-to-string-list (tag attr-list body body-fn)
  (:documentation "Used by PROCESS-TAG to convert `HTML' into a list
of strings.  TAG is a keyword symbol naming the outer tag, ATTR-LIST
is an alist of its attributes \(the car is the attribute's name as a
keyword, the cdr is its value), BODY is the tag's body, and BODY-FN is
a function which should be applied to BODY.  The function must return
a list of strings or Lisp forms."))

(defmethod convert-tag-to-string-list (tag attr-list body body-fn)
  "The standard method which is not specialized.  The idea is that you
can use EQL specializers on the first argument."
  (declare (optimize speed space))
  (let ((tag (if *downcase-tokens-p* (string-downcase tag) (string tag)))
        (body-indent
          ;; increase *INDENT* by 2 for body -- or disable it
          (when (and *indent* (not (member tag *html-no-indent-tags* :test #'string-equal)))
            (+ 2 *indent*))))
    (nconc
     (if *indent*
       ;; indent by *INDENT* spaces
       (list +newline+ (n-spaces *indent*)))
     ;; tag name
     (list "<" tag)
     ;; attributes
     (convert-attributes attr-list)
     ;; body
     (if body
       (append
        (list ">")
        ;; now hand over the tag's body to TREE-TO-TEMPLATE
        (let ((*indent* body-indent))
          (funcall body-fn body))
        (when body-indent
          ;; indentation
          (list +newline+ (n-spaces *indent*)))
        ;; closing tag
        (list "</" tag ">"))
       ;; no body, so no closing tag unless defined in *HTML-EMPTY-TAGS*
       (if (or (not *html-empty-tag-aware-p*)
               (member tag *html-empty-tags* :test #'string-equal))
         (list *empty-tag-end*)
         (list ">" "</" tag ">"))))))

(defun tree-to-template (tree)
  "Transforms an HTML tree into an intermediate format - mainly a
flattened list of strings. Utility function used by TREE-TO-COMMANDS-AUX."
  (loop for element in tree
        if (or (keywordp element)
                 (and (listp element)
                      (keywordp (first element)))
                 (and (listp element)
                      (listp (first element))
                      (keywordp (first (first element)))))
        ;; the syntax for a tag - process it
        nconc (process-tag element #'tree-to-template)
        ;; list - insert as sexp
        else if (consp element)
        collect `(let ((*indent* ,*indent*))
                   nil ;; If the element is (declare ...) it
                       ;; won't be interpreted as a declaration and an
                       ;; appropriate error could be signaled
                   ,element)
        ;; something else - insert verbatim
        else
        collect element))

(defun string-list-to-string (string-list)
  (declare (optimize speed space))
  "Concatenates a list of strings to one string."
  ;; note that we can't use APPLY with CONCATENATE here because of
  ;; CALL-ARGUMENTS-LIMIT
  (let ((total-size 0))
    (dolist (string string-list)
      (incf total-size (length string)))
    (let ((result-string (make-string total-size
                                      #+:lispworks #+:lispworks
                                      :element-type 'lw:simple-char))
          (curr-pos 0))
      (dolist (string string-list)
        (replace result-string string :start1 curr-pos)
        (incf curr-pos (length string)))
      result-string)))

(defun conc (&rest string-list)
  "Concatenates all arguments which should be string into one string."
  (funcall #'string-list-to-string string-list))

(defun tree-to-commands (tree stream &key prologue ((:indent *indent*) *indent*))
  (declare (optimize speed space))
  (when (and *indent*
             (not (integerp *indent*)))
    (setq *indent* 0))
  (let ((in-string-p t)
        collector
        string-collector
        (template (tree-to-template tree)))
    (when prologue
      (push +newline+ template)
      (when (eq prologue t)
        (setq prologue *prologue*))
      (push prologue template))
    (flet ((emit-string-collector ()
             "Generate a WRITE-STRING statement for what is currently
in STRING-COLLECTOR."
             (list 'write-string
                   (string-list-to-string (nreverse string-collector))
                   stream)))
      (dolist (element template)
        (cond ((and in-string-p (stringp element))
               ;; this element is a string and the last one
               ;; also was (or this is the first element) -
               ;; collect into STRING-COLLECTOR
               (push element string-collector))
              ((stringp element)
               ;; the last one wasn't a string so we start
               ;; with an empty STRING-COLLECTOR
               (setq string-collector (list element)
                     in-string-p t))
              (string-collector
               ;; not a string but STRING-COLLECTOR isn't
               ;; empty so we have to emit the collected
               ;; strings first
               (push (emit-string-collector) collector)
               (setq in-string-p nil
                     string-collector '())
               (push element collector))
              (t
               ;; not a string and empty STRING-COLLECTOR
               (push element collector))))
      (if string-collector
        ;; finally empty STRING-COLLECTOR if
        ;; there's something in it
        (nreverse (cons (emit-string-collector)
                        collector))
        (nreverse collector)))))

(defmacro with-html-output ((var &optional stream
                                 &rest rest
                                 &key prologue indent)
                            &body body)
  "Transform the enclosed BODY consisting of HTML as s-expressions
into Lisp code to write the corresponding HTML as strings to VAR -
which should either hold a stream or which'll be bound to STREAM if
supplied."
  (declare (ignore prologue))
  (multiple-value-bind (declarations forms) (extract-declarations body)
  `(let ((,var ,(or stream var)))
       ,@declarations
     (check-type ,var stream)
     (macrolet ((htm (&body body)
                  `(with-html-output (,',var nil :prologue nil :indent ,,indent)
                     ,@body))
                (fmt (&rest args)
                  `(format ,',var ,@args))
                (esc (thing)
                  (with-unique-names (result)
                    `(let ((,result ,thing))
                       (when ,result (write-string (escape-string ,result) ,',var)))))
                (str (thing)
                  (with-unique-names (result)
                    `(let ((,result ,thing))
                       (when ,result (princ ,result ,',var))))))
         ,@(apply 'tree-to-commands forms var rest)))))

(defmacro with-html-output-to-string ((var &optional string-form
                                           &key #-(or :ecl :cmu :sbcl)
                                                (element-type #-:lispworks ''character
                                                              #+:lispworks ''lw:simple-char)
                                                prologue
                                                indent)
                                      &body body)
  "Transform the enclosed BODY consisting of HTML as s-expressions
into Lisp code which creates the corresponding HTML as a string."
  (multiple-value-bind (declarations forms) (extract-declarations body)
  `(with-output-to-string (,var ,string-form
                                #-(or :ecl :cmu :sbcl) :element-type
                                #-(or :ecl :cmu :sbcl) ,element-type)
     ,@declarations
    (with-html-output (,var nil :prologue ,prologue :indent ,indent)
      ,@forms))))

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; also used by LW-ADD-ONS

(defvar *hyperdoc-base-uri* "http://weitz.de/cl-who/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :cl-who
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))
