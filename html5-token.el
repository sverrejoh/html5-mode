;;; html5-mode - HTML5 editing mode
;; Author:  Sverre Johansen (sverre.johansen@gmail.com)

;; Get next char from buffer
(defun html5-get-char ()
  "Returns next character from buffer"
  (forward-char)
  (char-before))

(defun html5-chars-until (chars &optional opposite regexp)
  (let ((string (buffer-substring-no-properties
                 (point)
                 (1- (search-forward-regexp
                      (or regexp
                          (and opposite (concat "[^" chars "]"))
                          (concat (concat "[" chars "]"))))))))
    (backward-char)
    string))

(defun html5-is-ascii (char)
  (or
   (and (>= char 97)
        (<= char 122))
   (and (>= char 65)
        (<= char 90))))

(defun html5-emit-current-token ()
  (when (member (html5-token-type html5-current-token)
                (list 'start-tag 'end-tag 'empty-tag))
    (when html5-lowercase-element-name
      (setf (html5-token-name html5-current-token)
            (downcase (html5-token-name html5-token-name))))
    (when (and (equal (html5-token-type html5-current-token)
                      'end-tag)
               (html5-token-data html5-current-token))
      (push (make-html5-token
             :type 'parse-error
             :data 'attributes-in-end-tag)
            html5-token-queue)))
  (push html5-current-token html5-token-queue)
  (setq html5-state 'html5-data-state))


(defconst html5-space-chars
  (list ?\t ?\n ?\r ?\u000B ?\u000C ?\s 10))

(defstruct html5-token
  "html5 token"
  type
  data
  name
  public-id
  correct
  system-id
  datavars)

;; Perform case conversions?
(defvar html5-lowercase-element-name nil)
(defvar html5-lowercase-attr-name nil)

(defun html5-tokenizer ()
  "Returns a token from the token queue if possible, else nil"
  (interactive)
  (message "html5-tokenizer")
  (save-excursion
    (goto-char (point-min))
    (let* (;; Current token being created
           (html5-current-token nil)
           (html5-token-queue nil)
           (html5-error-queue nil)

           ;; Default values
           (html5-content-model-flag 'pcdata)
           (html5-escape-flag nil)

           (html5-state 'html5-data-state)
           (token nil))
      (html5-get-token))))

(defun html5-get-token ()
  "Recursive token loop"
  (while (and html5-state
              (funcall html5-state))))

(defun html5-data-state ()
  (message "html5-data-state")
  (let ((c (html5-get-char)))
    (cond
     ;; Check for entity begin
     ((and (equal c ?&)
           (not html5-escape-flag)
           (or
            (equal html5-content-model-flag 'pcdata)
            (equal html5-content-model-flag 'rcdata)))
      (setq html5-state 'html5-entity-data-state)
      t)

     ;; Check for comment begin
     ((and (equal c ?-)
           (not html5-escape-flag)
           (or
            (equal html5-content-model-flag 'cdata)
            (equal html5-content-model-flag 'rcdata))
           (string= (buffer-substring-no-properties
                     (- (point) 4)
                     point)
                    "<!--"))
      (setq html5-escape-flag t)
      (push (make-html5-token :type 'characters
                              :data c)
            html5-token-queue)
      t)

     ;; Check for tag open
     ((and (equal c ?<)
           (or (equal html5-content-model-flag 'pcdata)
               (and (or (equal html5-content-model-flag 'cdata)
                        (equal html5-content-model-flag 'rcdata))
                    (not html5-escape-flag))))
      (setq html5-state 'html5-tag-open-state)
      t)

     ;; Check for end of comment
     ((and (equal c ?>)
           (or (equal html5-content-model-flag 'cdata)
               (equal html5-content-model-flag 'rcdata))
           html5-escape-flag
           (string= (buffer-substring-no-properties
                     (- (point) 3)
                     (point))
                    "-->"))
      (setq html5-escape-flag nil)
      (push (make-html5-token :type 'characters
                              :data c)
            html5-token-queue)
      t)

     ;; Tokenization ends.
     ((not c)
      nil)

     ;; Add space characters
     ((member c html5-space-chars)
      (push (make-html5-token :type 'space-characters
                              :data (concat
                                     (string c)
                                     (html5-chars-until html5-space-chars t)))
            html5-token-queue)
      t)

     ;; Add content data
     (t
      ;; Add chars until "&" "<" ">" or "-"
      (push (make-html5-token :type 'characters
                              :data (concat (char-to-string c)
                                            (html5-chars-until (list ?& ?<
                                                                     ?> ?-))))
              html5-token-queue)
      t))))

(defun html5-tag-open-state ()
  (message "html5-tag-open-state")
  (let ((c (html5-get-char)))
    (if (equal html5-content-model-flag 'pcdata)
        (cond
         ((equal c ?!)
          (setq html5-state 'html5-markup-declaration-open-state))

         ((equal c ?/)
          (setq html5-state 'html5-close-tag-open-state))

         ((html5-is-ascii c)
          (setq html5-current-token (make-html5-token
                                     :type 'start-tag
                                     :name (char-to-string c)
                                     :data ()))
          (setq html5-state 'html5-tag-name-state))



         ((equal c ?>)
          ;; XXX In theory it could be something besides a tag name. But
          ;; do we really care?
          (push (make-html5-token
                 :type 'parse-error
                 :data 'expected-tag-name-but-got-right-bracket)
                html5-token-queue)
          (push (make-html5-token
                 :type 'characters
                 :data "<>")
                html5-token-queue)
          (setq html5-state 'html5-data-state))

         ((equal c ??)
          (push (make-html5-token
                 :type 'parse-error
                 :data 'expected-tag-name-but-got-question-mark)
                html5-token-queue)
          (backward-char)
          (setq html5-state 'html5-bogus-comment-state))

         (t
          (nconc html5-token-queue
                 (list 'parse-error 'expected-tag-name))
          (nconc html5-token-queue
                 (list 'parse-error 'characters ?<))))

      ;; We know the content model flag is set to either RCDATA or CDATA
      ;; now because this state can never be entered with the PLAINTEXT
      ;; flag.
      (if (equal c ?/)
          (setq html5-state 'html5-close-tag-open-state)
        (push (make-html5-token :type 'characters
                                :data "<")
              html5-token-queue)
        (backward-char)
        (html5-data-state))))
  t)

(defun html5-markup-declaration-open-state ()

  (cond
   ;; Check for comment start
   ((string= (buffer-substring-no-properties (point)
                                             (+ (point) 2))
             "--")
    (forward-char 2)
    (setq html5-current-token (make-html5-token :type 'comment
                                                :data ""))
    (setq html5-state 'html5-comment-start-state))

   ;; DOCTYPE Declaration
   ((string= (buffer-substring-no-properties (point)
                                             (+ (point)
                                                (length "DOCTYPE")))
             "DOCTYPE")
    (forward-char (length "DOCTYPE"))
    (setq html5-current-token (make-html5-token :type 'doctype
                                                :name ""
                                                :public-id nil
                                                :system-id nil
                                                :correct t))
    (setq html5-state 'html5-doctype-state))

   (t
    (push (make-html5-token :type 'parse-error
                            :data 'expected-dashes-or-doctype)
          html5-token-queue)
    (setq html5-state 'html5-bogus-comment-state)))
  t)

(defun html5-doctype-state ()
  (if (member (html5-get-char) html5-space-chars)
      (setq html5-state 'html5-before-doctype-name-state)
    (progn
      (push (make-html5-token :type 'parse-error
                              :data 'need-space-after-doctype)
            html5-token-queue)
      (backward-char)
      (setq html5-state 'html5-before-doctype-name-state))))

(defun html5-before-doctype-name-state ()
  (let ((c (html5-get-char)))
    (cond
     ((member c html5-space-chars)
      (setq html5-state 'html5-before-doctype-name-state))

     ((equal c ?>)
      (push (make-html5-token
             :type 'parse-error
             :data 'expected-doctype-name-but-got-right-bracket)
            html5-token-queue)
      (setf (html5-token-correct html5-current-token) nil)
      (push html5-current-token html5-token-queue)
      (setq html5-state 'html5-data-state))

     ((equal c nil)
      (push (make-html5-token :type 'parse-error
                              :data 'expected-doctype-name-but-got-eof)
            html5-token-queue)
      (setf (html5-token-correct html5-current-token) nil)
      (push html5-current-token html5-token-queue)
      (setq html5-state 'html5-data-state))

     (t
      (setf (html5-token-name html5-current-token) (char-to-string c))
      (setf html5-state 'html5-doctype-name-state))))
  t)

(defun html5-doctype-name-state ()
  (let ((c (html5-get-char)))
    (cond
     ((member c html5-space-chars)
      (html5-after-doctype-name))

     ((equal c ?>)
      (push html5-current-token html5-token-queue)
      (setq html5-state 'html5-data-state))

     ((equal c nil)
      (setf (html5-token-correct html5-current-token) nil)
      (push html5-current-token html5-token-queue)
      (setq html5-state 'html5-data-state))
     (t
      (setf (html5-token-name html5-current-token)
            (concat (html5-token-name html5-current-token)
                    (char-to-string c))))))
  t)

(defun html5-tag-name-state ()
  (let ((c (html5-get-char)))
    (cond
     ((member c html5-space-chars)
      (setq html5-state 'html5-before-attribute-name-state))

     ((html5-is-ascii c)
      (setf (html5-token-name html5-current-token)
            (concat (html5-token-name html5-current-token)
                    (char-to-string c)
                    (html5-chars-until nil nil "[^a-zA-Z]"))))
     ((equal c ?>)
      (html5-emit-current-token))

     ((equal c nil)
      (push (make-html5-token
             :type 'parse-error
             :data 'eof-in-tag-name)
            html5-token-queue)
      (html5-emit-current-token))

     ((equal c ?/)
      (html5-process-solidus-in-tag)
      (setq html5-state 'html5-before-attribute-name-state))

     (t
      (setf (html5-token-name html5-current-token)
            (concat (html5-token-name html5-current-token)
                    c)))))
  t)

(defun html5-close-tag-open-state ()
  )

(defun html5-process-solidus-in-tag ()
  (let ((c (html5-get-char))
        (rv nil))
    (cond
     
     ((and (equal (html5-token-type current-token) 'start-tag)
           (equal c ?>))
      (setf (html5-token-type html5-current-token) 'empty-tag))

     ((equal c)
      (push (make-html5-token
             :type 'parse-error
             :data 'EOF-following-solidus)
            html5-token-queue)
      (setq html5-state 'html5-data-state)
      (html5-emit-current-token)
      (setf rv t))

     (t
      (push (make-html5-token
             :type 'parse-error
             :data 'incorrectly-placed-solidus )
            html5-token-queue)
      (backward-char))
     rv)))


      
     


    