;;; html5-mode - HTML5 editing mode
;; Author:  Sverre Johansen (sverre.johansen@gmail.com)

;; Get next char from buffer

(defvar html5-cursor 1)
(defvar html5-lineno 1)

(defconst html5-space-chars
  (list ?\t ?\n ?\r ?\u000B ?\u000C ?\s 10))

(defstruct html5-token
  "html5 token"
  type
  data
  name
  correct
  public-id
  system-id
  datavars

  pos
  length)

;; Perform case conversions?
(defvar html5-lowercase-element-name nil)
(defvar html5-lowercase-attr-name nil)
(defvar html5-current-token nil)
(defvar html5-token-queue nil)
(defvar html5-error-queue nil)

;; Default values
(defvar html5-content-model-flag 'pcdata)
(defvar html5-escape-flag nil)

(defvar html5-token-pos nil)

;; Init state
(defvar html5-state 'html5-data-state)

(defmacro html5-record-token (token)
  `(push ,token html5-token-queue))


(defsubst html5-get-char ()
  "Returns next character from buffer"
  (let ((c))
    ;; Check of eof
    (if (>= html5-cursor (point-max))
        (setq c nil)
      (setq c (char-before (incf html5-cursor)))

      ;; JS2-Mode keeps track of this, so I guess it's a good idea.
      (if (= c ?\n)
          (setq html5-line-start html5-cursor
                html5-lineno (1+ html5-lineno))))
    c))

(defun html5-eat-char (&optional count)
  (if count
      (setq html5-cursor (+ html5-cursor count))
    (incf html5-cursor)))

(defun html5-unget-char (&optional count)
  (if count
      (setq html5-cursor (- html5-cursor count))
    (decf html5-cursor)))

(defun html5-chars-until (chars &optional opposite regexp)
  (let ((start-point (point))
        (goto-char html5-cursor)
        (end-point
         (save-excursion (search-forward-regexp
                          ;; Search with regex, or make regular or opposite re.
                          (or regexp
                              (and opposite (concat "[^" chars "]"))
                              (concat (concat "[" chars "]")))
                          nil t))))
    (buffer-substring-no-properties start-point
                                    (if end-point
                                        (1- end-point)
                                      (point-max)))))


(defun html5-is-ascii (char)
  (and (characterp char)
       (or
        (and (>= char 97)
             (<= char 122))
        (and (>= char 65)
             (<= char 90)))))

(defun html5-emit-current-token ()
  (when (member (html5-token-type html5-current-token)
                (list 'start-tag 'end-tag 'empty-tag))
    (when html5-lowercase-element-name
      (setf (html5-token-name html5-current-token)
            (downcase (html5-token-name html5-current-token))))
    (when (and (equal (html5-token-type html5-current-token)
                      'end-tag)
               (html5-token-data html5-current-token))
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'attributes-in-end-tag))))
  (html5-record-token html5-current-token)
  (setq html5-state 'html5-data-state))

(defun html5-test-tokenizer ()
  (interactive)
  (save-excursion
    (setq html5-cursor 1)
    (setq html5-state 'html5-data-state)
    (let ((token (html5-get-token)))
      (while token
        (message "Token: %s" (symbol-name (html5-token-type token)))
        (setq token (html5-get-token))))))

(defun html5-add-errors ()
  (interactive)
  (save-excursion
    (setq html5-cursor 1)
    (setq html5-state 'html5-data-state)
    (let ((token (html5-get-token))
          (beg html5-cursor)
          (end (1+ html5-cursor))
          (ovl))
      (while token
        (when (equal (html5-token-type token) 'parse-error)
          (setq ovl (make-overlay beg end))
;;          (overlay-put ovl 'face face)
          (put-text-property beg end 'help-echo
                             (symbol-name 'foo))
          (setq token (html5-get-token))
          )))))

(defun html5-list-errors ()
  (interactive)
  (save-excursion
    (setq html5-cursor 1)
    (setq html5-state 'html5-data-state)
    (let ((token (html5-get-token)))
      (while token
        (when (equal (html5-token-type token)
                     'parse-error)
          (message "Error Token: %s" (symbol-name (html5-token-data token))))
        (setq token (html5-get-token))))))


(defun html5-get-token ()
  "Returns next token or nil"
  (let ((token 'no-token))
    (while (equal token 'no-token)
      (cond
       (html5-error-queue
        (setq token (car (last html5-error-queue)))
        (setq html5-token-queue (butlast html5-error-queue)))

       (html5-token-queue
        (setq token (car (last html5-token-queue)))
        (setq html5-token-queue (butlast html5-token-queue)))

       (html5-state
        (if (not (funcall html5-state))
            (setq token nil)))

       (t
        nil)))
    token))


(defun html5-data-state ()
  (let ((c (html5-get-char)))
    (cond
     ;; No data: tokenization ends.
     ((equal c nil)
      nil)

     ;; Check for entity begin
     ((and (equal c ?&)
           (not html5-escape-flag)
           (or
            (equal html5-content-model-flag 'pcdata)
            (equal html5-content-model-flag 'rcdata)))
      (setq html5-state 'html5-entity-data-state)
      t)

     ;; Check if we're inside a comment
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
      (html5-record-token (make-html5-token :type 'characters
                                            :data c))
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
      (html5-record-token (make-html5-token :type 'characters
                                            :data c))
      t)

     ;; Add space characters
     ((member c html5-space-chars)
      (html5-record-token (make-html5-token
                           :type 'space-characters
                           :data (concat
                                  (string c)
                                  (html5-chars-until html5-space-chars t))))
      t)

     ;; Add content data
     (t
      ;; Add chars until "&" "<" ">" or "-"
      (html5-record-token (make-html5-token :type 'characters
                              :data (concat (char-to-string c)
                                            (html5-chars-until (list ?& ?<
                                                                     ?> ?-)))))
      t))))

(defun html5-tag-open-state ()
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
          (html5-record-token (make-html5-token
                 :type 'parse-error
                 :data 'expected-tag-name-but-got-right-bracket))

          (html5-record-token (make-html5-token
                 :type 'characters
                 :data "<>"))

          (setq html5-state 'html5-data-state))

         ((equal c ??)
          (html5-record-token (make-html5-token
                               :type 'parse-error
                               :data 'expected-tag-name-but-got-question-mark))
          (html5-unget-char)
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
        (html5-record-token (make-html5-token :type 'characters
                                :data "<"))
        (html5-unget-char)
        (html5-data-state))))
  t)

(defun html5-markup-declaration-open-state ()
  (cond
   ;; Check for comment start
   ((string= (buffer-substring-no-properties (point)
                                             (+ (point) 2))
             "--")
    (html5-eat-char 2)
    (setq html5-current-token (make-html5-token :type 'comment
                                                :data ""))
    (setq html5-state 'html5-comment-start-state))

   ;; DOCTYPE Declaration
   ((string= (buffer-substring-no-properties (point)
                                             (+ (point)
                                                (length "DOCTYPE")))
             "DOCTYPE")
    (html5-eat-char (length "DOCTYPE"))
    (setq html5-current-token (make-html5-token :type 'doctype
                                                :name ""
                                                :public-id nil
                                                :system-id nil
                                                :correct t))
    (setq html5-state 'html5-doctype-state))

   (t
    (html5-record-token (make-html5-token :type 'parse-error
                            :data 'expected-dashes-or-doctype))
    (setq html5-state 'html5-bogus-comment-state)))
  t)

(defun html5-doctype-state ()
  (if (member (html5-get-char) html5-space-chars)
      (setq html5-state 'html5-before-doctype-name-state)
    (progn
      (html5-record-token (make-html5-token :type 'parse-error
                              :data 'need-space-after-doctype))

      (html5-unget-char)
      (setq html5-state 'html5-before-doctype-name-state)))
  t)

(defun html5-before-doctype-name-state ()
  (let ((c (html5-get-char)))
    (cond
     ((member c html5-space-chars)
      (setq html5-state 'html5-before-doctype-name-state))

     ((equal c ?>)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'expected-doctype-name-but-got-right-bracket))

      (setf (html5-token-correct html5-current-token) nil)
      (html5-current-token html5-current-token)
      (setq html5-state 'html5-data-state))

     ((equal c nil)
      (html5-record-token (make-html5-token :type 'parse-error
                              :data 'expected-doctype-name-but-got-eof))

      (setf (html5-token-correct html5-current-token) nil)
      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state))

     (t
      (setf (html5-token-name html5-current-token) (char-to-string c))
      (setf html5-state 'html5-doctype-name-state))))
  t)

(defun html5-doctype-name-state ()
  (let ((c (html5-get-char)))
    (cond
     ((member c html5-space-chars)
      (html5-after-doctype-name-state))

     ((equal c ?>)
      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state))

     ((equal c nil)
      (setf (html5-token-correct html5-current-token) nil)
      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state))
     (t
      (setf (html5-token-name html5-current-token)
            (concat (html5-token-name html5-current-token)
                    (char-to-string c))))))
  t)

(defun html5-after-doctype-name-state ()
  (let ((c (html5-get-char)))
    (cond
     ((member c html5-space-chars)
      t)

     ((equal c ?>)
      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state)
      t)

     ((equal c nil)
      (setf (html5-token-correct html5-current-token) nil)
      (html5-unget-char)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-doctype))

      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state)
      t)

     ((string= (downcase (buffer-substring-no-properties
                          (1- (point))
                          (+ (point) (length "ublic"))))
               "public")
      (html5-eat-char (length "ublic"))
      (setq html5-state 'html5-before-doctype-public-identifier)
      t)

     ((string= (downcase (buffer-substring-no-properties
                          (1- (point))
                          (+ (point) (length "ystem"))))
               "system")
      (html5-eat-char (length "ystem"))
      (setq html5-state 'html5-before-doctype-system-identifier-state)
      t)

     (t
      (message "DOCTYPE: %s" (downcase (buffer-substring-no-properties
                          (point)
                          (+ (point) (length "public")))))
      (html5-record-token (make-html5-token
               :type 'parse-error
               :data 'expected-space-or-right-bracket-in-doctype
               :datavars '(:data c)))

      (setf (html5-token-correct html5-current-token) nil)
      (setq html5-state 'html5-bogus-doctype-state)
      t))))


(defun html5-doctype-public-identifier-double-quoted-state ()
  (let ((c (html5-get-char)))
    (cond
     ((equal c ?\")
      (setq html5-state 'html5-after-doctype-public-identifier-state)
      t)

     ((equal c ?>)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'unexpected-end-of-doctype))

      (setf (html5-token-correct html5-current-token) nil)
      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state)
      t)

     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-doctype))

      (setf (html5-token-correct html5-current-token) nil)
      (setq html5-state 'html5-data-state)
      t)

     (t
      (setf (html5-token-public-id html5-current-token)
            (concat (html5-token-name html5-current-token)
                    (char-to-string c)))
      t))))

(defun html5-doctype-public-identifier-single-quoted-state ()
  (let ((c (html5-get-char)))
    (cond
     ((equal c ?')
      (setq html5-state 'html5-after-doctype-public-identifier-state)
      t)

     ((equal c ?>)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'unexpected-end-of-doctype))

      (setf (html5-token-correct html5-current-token) nil)
      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state)
      t)

     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-doctype))

      (setf (html5-token-correct html5-current-token) nil)
      (setq html5-state 'html5-data-state)
      t)

     (t
      (setf (html5-token-public-id html5-current-token)
            (concat (html5-token-name html5-current-token)
                    (char-to-string c)))
      t))))

(defun html5-after-doctype-public-identifier-state ()
  (let ((c (html5-get-char)))
    (cond

     ((member c html5-space-chars)
      t)

     ((equal c ?\")
      (setf (html5-token-system-id html5-current-token) "")
      (setq html5-state 'html5-doctype-public-identifier-double-quoted-state)
      t)

     ((equal c ?')
      (setf (html5-token-system-id html5-current-token) "")
      (setq html5-state 'html5-doctype-system-identifier-single-quoted-state)
      t)

     ((equal c ?>)
      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state)
      t)

     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-doctype))

      (setf (html5-token-correct html5-current-token) nil)
      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state)
      t)

     (t
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'unexpected-char-in-doctype))

      (setf (html5-token-correct html5-current-token) nil)
      (setq html5-state 'html5-bogus-doctype-state)
      t))))

(defun html5-doctype-system-identifier-double-quoted-state ()
  (let ((c (html5-get-char)))
    (cond
     ((equal c ?\")
      (setq html5-state 'html5-after-doctype-system-identifier-state)
      t)

     ((equal c ?>)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'unexpected-end-of-doctype))

      (setf (html5-token-correct html5-current-token) nil)
      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state)
      t)

     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-doctype))

      (setf (html5-token-correct html5-current-token) nil)
      (setq html5-state 'html5-data-state)
      t)

     (t
      (setf (html5-token-system-id html5-current-token)
            (concat (html5-token-name html5-current-token)
                    (char-to-string c)))
      t))))

(defun html5-doctype-system-identifier-single-quoted-state ()
  (let ((c (html5-get-char)))
    (cond
     ((equal c ?')
      (setq html5-state 'html5-after-doctype-system-identifier-state)
      t)

     ((equal c ?>)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'unexpected-end-of-doctype))

      (setf (html5-token-correct html5-current-token) nil)
      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state)
      t)

     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-doctype))

      (setf (html5-token-correct html5-current-token) nil)
      (setq html5-state 'html5-data-state)
      t)

     (t
      (setf (html5-token-system-id html5-current-token)
            (concat (html5-token-name html5-current-token)
                    (char-to-string c)))
      t))))

(defun html5-before-doctype-system-identifier-state ()
  (let ((c (html5-get-char)))
    (cond
     ((member c html5-space-chars)
      t)

     ((equal c ?\")
      (setf (html5-token-system-id html5-current-token) "")
      (setq html5-state 'html5-doctype-system-identifier-double-quoted-state)
      t)

     ((equal c ?')
      (setf (html5-token-system-id html5-current-token) "")
      (setq html5-state 'html5-doctype-system-identifier-single-quoted-state)
      t)

     ((equal c ?>)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'unexpected-char-in-doctype))

      (setf (html5-token-correct html5-current-token) nil)
      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state)
      t)

     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-doctype))

      (setf (html5-token-correct html5-current-token) nil)
      (setq html5-state 'html5-data-state)
      t)

     (t
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'unexpected-char-in-doctype))

      (setf (html5-token-correct html5-current-token) nil)
      (setq html5-state 'html5-bogus-doctype-state)
      t))))

(defun html5-after-doctype-system-identifier-state ()
  (let ((c (html5-get-char)))
    (cond
     ((member c html5-space-chars)
      t)

     ((equal c ?>)
      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state)
      t)

     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-doctype))

      (setf (html5-token-correct html5-current-token) nil)
      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state)
      t)

     (t
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'unexpected-char-in-doctype))

      (setf (html5-token-correct html5-current-token) nil)
      (setq html5-state 'html5-bogus-doctype-state)
      t))))

(defun html5-bogus-doctype-state ()
  (let ((c (html5-get-char)))
    (cond
     ((equal c ?>)
      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state)
      t)

     ((equal c nil)
      (html5-unget-char)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-bogus-doctype))

      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state)
      t)

     (t
      t))))

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
      (html5-record-token (make-html5-token
                           :type 'parse-error
                           :data 'eof-in-tag-name))
      (html5-emit-current-token))

     ((equal c ?/)
      (html5-process-solidus-in-tag)
      (setq html5-state 'html5-before-attribute-name-state))

     (t
      (setf (html5-token-name html5-current-token)
            (concat (html5-token-name html5-current-token)
                    (char-to-string c))))))
  t)

(defun html5-close-tag-open-state ()
  (catch 'return
    (when (member html5-content-model-flag (list 'rcdata 'cdata))
      (let* ((tag-name (and html5-current-token
                            (html5-token-name html5-current-token)))
             (tag-length (length tag-name)))

        ;; If endtag matches starttag, switch to pcdata (let tagname state handle it properly)
        (if (and tag-name
                 (equal tag-name
                        (substring-no-properties (point) (+ (point) (tag-length))))
                 (member (char-after (+ (point) length))
                         (cons html5-space-chars (list ?> ?/ ?< nil))))
            (setq html5-content-model-flag 'pcdata)

          ;; If there wasn't any matching tag, store as characters
          (html5-record-token (make-html5-token
                 :type 'characters
                 :data "</"))

          (setq html5-state 'html5-data-state)
          (throw 'return t))))

    (let ((c (html5-get-char)))
      (cond
       ((html5-is-ascii c)
        (setq html5-current-token (make-html5-token
                                   :type 'end-tag
                                   :name (char-to-string c)
                                   :data ()))
        (setq html5-state 'html5-tag-name-state))

       ((equal c ?>)
        (html5-record-token (make-html5-token
               :type 'parse-error
               :data 'expected-closing-tag-but-got-right-bracket))

        (setq html5-state 'html5-data-state))

       ((equal c nil)
        (html5-record-token (make-html5-token
               :type 'parse-error
               :data 'expected-closing-tag-but-got-eof))

        (html5-record-token (make-html5-token
               :type 'characters
               :data '"</")))


       (t
        (html5-record-token (make-html5-token
               :type 'parse-error
               :data 'expected-closing-tag-but-got-char
               :datavars '(:data c)))

        (html5-unget-char)
        (setq html5-state 'html5-bogus-comment-state))))
    (throw 'return t)))

(defun html5-before-attribute-name-state ()
  (let ((c (html5-get-char)))
    (cond
     ((member c html5-space-chars)
      (html5-chars-until html5-space-chars t))

     ((html5-is-ascii c)
      (push (list (char-to-string c)  "")
            (html5-token-data html5-current-token))
      (setq html5-state 'html5-attribute-name-state))

     ((equal c ?>)
      (html5-emit-current-token))

     ((equal c ?/)
      (html5-process-solidus-in-tag))

     ((member c (list ?' ?\" ?=))
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'invalid-character-in-attribute-name))

      (push (list (char-to-string c)  "")
            (html5-token-data html5-current-token))
      (setq html5-state 'html5-attribute-name-state))

     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'expected-attribute-name-but-got-eof))

      (html5-emit-current-token))

     (t
      (push (list (char-to-string c)  "")
            (html5-token-data html5-current-token))
      (setq html5-state 'html5-attribute-name-state))))
  t)

(defun html5-attribute-name-state ()
  (let ((c (html5-get-char))
        (leaving-this-state t)
        (emit-token nil))
    (cond
     ((equal c ?=)
      (setq html5-state 'html5-before-attribute-value-state))

     ((html5-is-ascii c)
      (setf (caar (html5-token-data html5-current-token))
            (concat (caar (html5-token-data html5-current-token))
                    (char-to-string c)
                    (html5-chars-until nil nil "[^a-zA-Z]")))
      (setq leaving-this-state nil))

     ((equal c ?>)
      ;; # XXX If we emit here the attributes are converted to a dict
      ;; # without being checked and when the code below runs we error
      ;; # because data is a dict not a list
      (setq emit-token t))

     ((member c html5-space-chars)
      (setq html5-state 'html5-after-attibute-name-state))

     ((equal c ?/)
      (unless (html5-process-solidus-in-tag)
        (setq html5-state 'html5-before-attribute-name-state)))

     ((or (equal c ?\')
          (equal c ?\"))
      (html5-record-token (make-html5-token
         :type 'parse-error
         :data 'invalid-character-in-attribute-name))

      (setf (caar (html5-token-data html5-current-token))
            (concat (caar (html5-token-data html5-current-token))
                    (char-to-string c)))
      (setq leaving-this-state nil))

     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-attribute-name))

      (setq html5-state 'html5-data-state)
      (setq emit-token t))

     (t
      (setf (caar (html5-token-data html5-current-token))
            (concat (caar (html5-token-data html5-current-token))
                    (char-to-string c)))
      (setq leaving-this-state nil)))

    (when leaving-this-state
      (when html5-lowercase-attr-name
        (setf (caar (html5-token-data html5-current-token))
              (downcase (caar (html5-token-data html5-current-token)))))

      (dolist (attr (cdr (html5-token-data html5-current-token)))
        ;; XXX sverrej 2008-12-27
        ;; Should break the loop for performance
        (when (equal (caar (html5-token-data html5-current-token))
                     (car attr))
          (html5-record-token (make-html5-token
                 :type 'parse-error
                 :data 'duplicate-attribute))))

      (when emit-token
        (html5-emit-current-token))))
  t)

(defun html5-before-attribute-value-state ()
  (let ((c (html5-get-char)))
    (cond
     ((member c html5-space-chars)
      (html5-chars-until html5-space-chars t))

     ((equal c ?\")
      (setq html5-state 'html5-attribute-value-double-quoted-state))

     ((equal c ?&)
      (setq html5-state 'html5-attribute-value-unquoted-state)
      (html5-unget-char))

     ((equal c ?')
      (setq html5-state 'html5-attribute-value-single-quoted-state))

     ((equal c ?>)
      (html5-emit-current-token))

     ((equal c ?=)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'equals-in-unquoted-attribute-value))

      (setf (html5-token-data html5-current-token)
            (concat (html5-token-data html5-current-token)
                    (char-to-string c)))
      (setq html5-state 'html5-attribute-value-unquoted-state))

     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'expected-attribute-value-but-got-eof))

      (html5-emit-current-token))

     (t
      (setf (cadar (html5-token-data html5-current-token))
            (concat (cadar (html5-token-data html5-current-token))
                    (char-to-string c)))
      (setq html5-state html5-attribute-value-unquoted-state))))
  t)


(defun html5-attribute-value-double-quoted-state ()
  (let ((c (html5-get-char)))
    (cond
     ((equal c ?\")
      (setq html5-state 'html5-after-attibute-value-state))

     ((equal c ?&)
      (html5-process-entity-in-attribute "\""))

     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-attribute-value-double-quote))

      (html5-emit-current-token))

     (t
      (setf (cadar (html5-token-data html5-current-token))
            (concat (cadar (html5-token-data html5-current-token))
                    (char-to-string c)
                    (html5-chars-until (list ?\" ?&)))))))
  t)

(defun html5-attribute-value-single-quoted-state ()
  (let ((c (html5-get-char)))
    (cond
     ((equal c ?')
      (setq html5-state 'html5-after-attibute-value-state))

     ((equal c ?&)
      (html5-process-entity-in-attribute "'"))

     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-attribute-value-single-quote))

      (html5-emit-current-token))

     (t
      (setf (cadar (html5-token-data html5-current-token))
            (concat (cadar (html5-token-data html5-current-token))
                    (char-to-string c)
                    (html5-chars-until (list ?' ?&)))))))
  t)

(defun html5-attribute-value-unquoted-state ()
  (let ((c (html5-get-char)))
    (cond
     ((member c html5-space-chars)
      (setq html5-state 'html5-before-attribute-name-state))

     ((equal c ?&)
      (html5-process-entity-in-attribute nil))

     ((equal c ?>)
      (html5-emit-current-token))

     ((member c (list ?\" ?' ?=))
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'unexpected-character-in-unquoted-attribute-value))

      (setf (cadar (html5-token-data html5-current-token))
            (concat (cadar (html5-token-data html5-current-token))
                    (char-to-string c))))
     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-attribute-value-no-quotes))

      (html5-emit-current-token))

     (t
      (setf (cadar (html5-token-data html5-current-token))
            (concat (cadar (html5-token-data html5-current-token))
                    (char-to-string c)
                    (html5-chars-until (append html5-space-chars
                                               (list ?& ?> ?< ?= ?' ?\"))))))))
  t)



(defun html5-after-attibute-value-state ()
  (let ((c (html5-get-char)))
    (cond
     ((member c html5-space-chars)
      (setq html5-state 'html5-before-attribute-name-state))

     ((equal c ?>)
      (html5-emit-current-token)
      (setq html5-state 'html5-data-state))

     ((equal c ?/)
      (if (not (html5-process-solidus-in-tag))
          (setq html5-state 'html5-before-attribute-name-state)))

     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'unexpected-EOF-after-attribute-value)))

     (t
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'unexpected-character-after-attribute-value))

      (html5-unget-char)
      (setq html5-state 'html5-before-attribute-name-state))))
  t)


(defun html5-bogus-comment-state ()
  ;; XXX html5-chars-until should check for EOF
  (html5-record-token (make-html5-token
         :type 'comment
         :data (html5-chars-until '(?>))))

  (html5-eat-char)
  (setq html5-state 'html5-data-state)
  t)

(defun html5-comment-start-state ()
  (let ((c (html5-get-char)))
    (cond
     ((equal c ?-)
      (setq html5-state 'html5-comment-start-dash-state))
     ((equal c ?>)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'incorrect-comment))

      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state))
     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-comment))

      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state))
     (t
      (setf (html5-token-data html5-current-token)
            (concat (html5-token-data html5-current-token)
                    (char-to-string c)
                    (html5-chars-until '(?-))))
      (setq html5-state 'html5-comment-state))))
  t)

(defun html5-comment-start-dash-state ()
  (let ((c (html5-get-char)))
    (cond
     ((equal c ?-)
      (setq html5-state 'html5-comment-state))
     ((equal c ?>)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'incorrect-comment))

      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state))
     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-comment))

      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state))
     (t
      (setf (html5-token-data html5-current-token)
            (concat (html5-token-data html5-current-token)
                    "-"
                    (char-to-string c)
                    (html5-chars-until '(?-))))
      (setq html5-state 'html5-comment-state))))
  t)

(defun html5-comment-state ()
  (let ((c (html5-get-char)))
    (cond
     ((equal c ?-)
      (setq html5-state 'html5-comment-end-dash-state))
     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-comment))

      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state))
     (t
      (setf (html5-token-data html5-current-token)
            (concat (html5-token-data html5-current-token)
                    (char-to-string c)
                    (html5-chars-until '(?-))))
      (setq html5-state html5-comment-state))))
  t)

(defun html5-comment-end-dash-state ()
  (let ((c (html5-get-char)))
    (cond
     ((equal c ?-)
      (setq html5-state 'html5-comment-end-state))
     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-comment-end-dash))

      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state))
     (t
      (setf (html5-token-data html5-current-token)
            (concat (html5-token-data html5-current-token)
                    "-"
                    (char-to-string c)
                    (html5-chars-until '(?-))))
      ;; Consume the next character which is either a "-" or an EOF as
      ;; well so if there's a "-" directly after the "-" we go nicely to
      ;; the "comment end state" without emitting a ParseError() there.
      (html5-eat-char))))
  t)

(defun html5-comment-end-state ()
  (let ((c (html5-get-char)))
    (cond
     ((equal c ?>)
      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state))
     ((equal c ?-)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'unexpected-dash-after-double-dash-in-comment))

      (setf (html5-token-data html5-current-token)
            (concat (html5-token-data html5-current-token)
                    (char-to-string c))))
     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-comment-double-dash))

      (html5-record-token html5-current-token)
      (setq html5-state 'html5-data-state))
     (t
      ;; XXX
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'eof-in-comment-double-dash))

      (setf (html5-token-data html5-current-token)
            (concat (html5-token-data html5-current-token)
                    "--"
                    (char-to-string c)))
      (setq html5-state 'html5-comment-state))))
  t)


(defun html5-entity-data-state ()
  (html5-record-token (make-html5-token
                       :type 'characters
                       :pos (1- (point))
                       :length 1 ;; FIXME consume entity does not return properly.
                       :data (or (html5-consume-entity)
                                 "&")))

  (setq html5-state 'html5-data-state))

(defun html5-after-attibute-name-state ()
  (let ((c (html5-get-char)))
    (cond
     ((member c html5-space-chars)
      (html5-chars-until html5-space-chars t))
     ((equal c ?=)
      (setq html5-state 'html5-before-attribute-value-state))
     ((equal c ?>)
      (html5-emit-current-token))
     ((html5-is-ascii c)
      (push (list (char-to-string c)  "")
            (html5-token-data html5-current-token))
      (setq html5-state 'html5-attribute-name-state))
     ((equal c ?/)
      (if (not (html5-process-solidus-in-tag))
          (setq html5-state 'html5-before-attribute-name-state)))

     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'expected-attribute-name-but-got-eof))

      (html5-emit-current-token))
     (t
      (push (list (char-to-string c)  "")
            (html5-token-data html5-current-token))
      (setq html5-state 'html5-attribute-name-state))))
  t)

(defun html5-process-solidus-in-tag ()
  (let ((c (html5-get-char))
        (rv nil))
    (cond
     ((and (equal (html5-token-type html5-current-token) 'start-tag)
           (equal c ?>))
      (setf (html5-token-type html5-current-token) 'empty-tag))

     ((equal c nil)
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'EOF-following-solidus))

      (setq html5-state 'html5-data-state)
      (html5-emit-current-token)
      (setf rv t))

     (t
      (html5-record-token (make-html5-token
             :type 'parse-error
             :data 'incorrectly-placed-solidus))

      (html5-unget-char)))
    rv))

(defun html5-consume-entity (&optional allowed-char from-attribute)
  ;; TODO This looks to me to be so stupidly implemented in html5lib, but
  ;; I might be missing something.
  ;;
  ;; (let ((char)
  ;;       (char-stack (list html5-get-char)))
  ;;   (cond
  ;;    ;; Ignore whitespace and other not allowed chars
  ;;    ((or (member (car char-stack)
  ;;                 (append html5-space-chars
  ;;                         (list nil ?< ?&)))
  ;;         (and allowed-char
  ;;              (equal (car char-stack)
  ;;                     allowed-char)))
  ;;     (html5-unget-char))

  ;;    ;; Number entity?
  ;;    ((equal (car char-stack) ?#)
  ;;     ;; TODO
  ;;    )

  ;;    ;; Probably named entiry
  ;;    (t
  ;;     t))))
  "&")

(defun html5-process-entity-in-attribute (allowed-char)
  (setf (cadar (html5-token-data html5-current-token))
        (concat (cadar (html5-token-data html5-current-token))
                (or (html5-consume-entity allowed-char t)
                    "&"))))
