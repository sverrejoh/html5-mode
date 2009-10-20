;;; html5-mode - HTML5 editing mode
;; Author:  Sverre Johansen (sverre.johansen@gmail.com)

(defvar html5-compat-mode 'no-quirks)
(defvar html5-insertion-handler 'html5-initial-mode)

(defmacro html5-token-case (token &rest clauses)
  (cons 'cond
        (mapcar (lambda (clause)
                  (list `(and (eql ,(caar clause) (html5-token-type ,token))
                              (or (not ',(cdar clause))
                                  (member (html5-token-name ,token)
                                          ',(cdar clause))))
                         (cadr clause)))
                clauses)))

(put 'html5-token-case 'lisp-indent-function 1)

(defun html5-parse ()
  )


(defun html5-initial-mode (token)
  (html5-token-case token
    (('space-characters)
     t)

    (('comment)
     ;; TODO
     ;; Append a Comment node to the Document  object with the data
     ;; attribute set to the data given in the comment token
     t)

    (('doctype)
     ;; TODO
     t)

    (t
     ;; Raise hell, this is a parse error
     nil)))

(defun html5-before-html-mode (token)
  (cond
   (
     ;; Parse error
     nil)

    ((eql (html5-token-type token) 'comment)
     ;; TODO
     ;; Append a Comment node to the Document  object with the data
     ;; attribute set to the data given in the comment token
     t)

    ;; html start tag
    ((and (eql (html5-token-type token) 'start-tag)
          (eql (html5-token-name token) "html"))
     ;; TODO:
     ;; Create html element.
     ;; Put element on stack of open elements.

     ;; If the Document is being loaded as part of navigation of a
     ;; browsing context, then: if the newly created element has a
     ;; manifest attribute, then resolve the value of that attribute
     ;; to an absolute URL, relative to the newly created element, and
     ;; if that is successful, run the application cache selection
     ;; algorithm with the resulting absolute URL; otherwise, if there
     ;; is no such attribute or resolving it fails, run the
     ;; application cache selection algorithm with no manifest. The
     ;; algorithm must be passed the Document object.
     (setq html5-insertion-handler 'html5-before-head-mode))

    (t
     ;; TODO:
     ;; - Create an HTML Element - Put ele on stack of open
     ;;   elements.
     ;; - If the Document is being loaded as part of
     ;;   navigation of a browsing context, then: run the application
     ;;   cache selection algorithm with no manifest, passing it the
     ;;   Document object.
     (setq html5-insertion-handler 'html5-before-head-mode)
     (html5-before-head-mode token))))

(defun html5-before-head-mode (token)
  (cond
   ((eql (html5-token-name token) 'space-characters)
    t)
   ((eql (html5-token-name token) 'comment)
    ;; TODO
    ;; Append comment node to current node
    t)
   ((eql ((html5-token-name token) 'doctype))
    ;; TODO
    ;; Parse error
    t)
   ((and (eql (html5-token-type token) 'start-tag)
          (eql (html5-token-name token) "html"))
    ;; XXX Change to this mode as well?
    (html5-in-body-mode token))
   ((and (eql (html5-token-type token) 'start-tag)
          (eql (html5-token-name token) "head"))
    ;; TODO
    ;; Insert HTTML element for token
    ;; Set the head element pointer  to the newly created head element.
    (setq html5-insertion-handler 'html5-in-head-mode))
   ((and (eql (html5-token-type token) 'end-tag)
         (member (html5-token-type token)
                 (list "head" "body" "html" "br")

