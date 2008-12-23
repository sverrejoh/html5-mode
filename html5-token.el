;;; html5-mode - HTML5 editing mode
;; Author:  Sverre Johansen (sverre.johansen@gmail.com)

;; Get next char from buffer
(defun html5-get-char ()
  "Returns next character from buffer"
  (char-before (incf html5-cursor)))

(defun html5-chars-until (chars &optional opposite)
  (let ((char (html5-get-char))
        (data ()))
    (while (or
            (not (member (html5-get-char) chars))
            opposite))
      (push char data))
    (concat (reverse data)))

(defconst html5-space-chars
  "Characters defined as space in HTML 5"
  (list ?\t ?\u000B ?\u000C ?\s 10))

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
(defvar lowercase-element-name nil)
(defvar lowercase-attr-name nil)

(defun html5-tokenizer ()
  "Returns a token from the token queue if possible, else nil"
  (interactive)
  (message "html5-tokenizer")
  (let* ((html5-cursor (point-min))
         
         ; Current token being created
         (html5-current-token nil)         
         (html5-token-queue nil)
         (html5-error-queue nil)
         
         ; Default values
         (html5-content-model-flag 'pcdata)
         (html5-escape-flag nil)

         (token nil))
    (html5-get-token)))

(defun html5-get-token ()
  "Recursive token loop"
  (html5-data-state))

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
      (html5-entity-data-state)
      t)

     ;; Check for comment begin
     ((and (equal c ?-)
           (not html5-escape-flag)
           (or
            (equal html5-content-model-flag 'cdata)
            (equal html5-content-model-flag 'rcdata))
           (string= (buffer-substring-no-properties
                     (- html5-cursor 4)
                     html5-cursor)
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
      (html5-tag-open-state)
      t)

     ;; Check for end of comment
     ((and (equal c ?>)
           (or (equal html5-content-model-flag 'cdata)
               (equal html5-content-model-flag 'rcdata))
           html5-escape-flag
           (string= (buffer-substring-no-properties
                     (- html5-cursor 3)
                     html5-cursor)
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
                                     (html5-chars-until html5-space-chars)))
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
          (html5-markup-declaration-open-state))
         
         ((equal c ?/)
          (html5-close-tag-open-state))
         
         ((html5-is-ascii c)
          (setq html5-token-queue (list 'start-tag 'data nil)))
         
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
          (html5-data-state))
         
         ((equal c ??)
          (push (make-html5-token
                 :type 'parse-error
                 :data 'expected-tag-name-but-got-question-mark)
                html5-token-queue)
          (decf html5-cursor)
          (html5-bogus-comment-state))

         (t
          (nconc html5-token-queue
                 (list 'parse-error 'expected-tag-name))
          (nconc html5-token-queue
                 (list 'parse-error 'characters ?<))))

      ;; We know the content model flag is set to either RCDATA or CDATA
      ;; now because this state can never be entered with the PLAINTEXT
      ;; flag.
      (if (equal c ?/)
          (html5-close-tag-open-state)
        (push (make-html5-token :type 'characters
                                :data "<")
              html5-token-queue)
        (decf html5-cursor)
        (html5-data-state))))
  t)




(defun html5-markup-declaration-open-state ()
  (cond
   ;; Check for comment start
   ((string= (buffer-substring-no-properties html5-cursor
                                             (+ html5-cursor 2))
             "--")
    (setq html5-cursor (+ html5-cursor 2))
    (setq html5-current-token (make-html5-token :type 'comment
                                                :data ""))
    (html5-comment-start-state))

   ;; DOCTYPE Declaration
   ((string= (buffer-substring-no-properties html5-cursor
                                             (+ html5-cursor
                                                (length "DOCTYPE")))
             "DOCTYPE")
    (setq html5-cursor (+ html5-cursor (length "DOCTYPE")))
    (setq html5-current-token (make-html5-token :type 'doctype
                                                :name ""
                                                :public-id nil
                                                :system-id nil
                                                :correct t))
    (html5-doctype-state))
   
   (t
    (push (make-html5-token :type 'parse-error
                            :data 'expected-dashes-or-doctype)
          html5-token-queue)
    (html5-bogus-comment-state))
  t))

(defun html5-doctype-state ()
  (if (member (html5-get-char) html5-space-chars)
      (html5-before-doctype-name-state)
    (progn
      (push (make-html5-token :type 'parse-error
                              :data 'need-space-after-doctype)
            html5-token-queue)
      (setq html5-cursor (1- html5-cursor))
      (html5-before-doctype-name-state))))

(defun html5-before-doctype-name-state ()
  (let ((c (html5-get-char)))
    (cond
     ((member c html5-space-chars)
      (html5-before-doctype-name-state))
     
     ((equal c ?>)
      (push (make-html5-token
             :type 'parse-error
             :data 'expected-doctype-name-but-got-right-bracket)
            html5-token-queue)
      (setf (html5-token-correct html5-current-token) nil)
      (push html5-current-token html5-token-queue)
      (html5-data-state))
      
     ((equal c nil)
      (push (make-html5-token :type 'parse-error
                              :data 'expected-doctype-name-but-got-eof)
            html5-token-queue)
      (setf (html5-token-correct html5-current-token) nil)
      (push html5-current-token html5-token-queue)
      (html5-data-state))
     
     (t
      (setf (html5-token-name html5-current-token) (char-to-string c))
      (html5-doctype-name))))
  t)

(defun html5-doctype-name ()
  (let ((c (html5-get-char)))
    (cond
     ((member c html5-space-chars)
      (html5-after-doctype-name))

     ((equal c ?>)
      (push html5-current-token html5-token-queue)
      (html5-data-state))

     ((equal c nil)
      (setf (html5-token-correct html5-current-token) nil)
      (push html5-current-token html5-token-queue)
      (html5-data-state))      

     (t
      (setf (html5-token-name html5-current-token)
            (concat (html5-token-name html5-current-token)
                    (char-to-string c)))
      (html5-doctype-name)))))
  
;; (defun html5-comment-start-state ())

   

  
;; ;; OLDSCHOOL

;; ;; (defvar html5-message-queue nil)
;; ;; (defvar html5-space-chars
;; ;;   (list ?\t ?\n ?\u000B ?\u000C ?\s ?\r))

;; (defun html5-is-ascii (char)
;;   (or (and (> char 64)
;;            (< char 91))
;;       (and (> char 96)
;;            (< char 123))))

;; ;; (defun html5-tokenizer ()
;; ;;   (interactive)
;; ;;   (cond
;; ;;    (html5-token-queue
;; ;;     (pop html5-token-queue))
;; ;;    (html5-message-queue
;; ;;     (pop html5-message-queue))
;; ;;    (t
;; ;;     (if (funcall html5-state)
;; ;;         (html5-tokenizer)
;; ;;       (message "No more shit")))))

;; ;; (defun html5-update-last-four-chars (char)
;; ;;   "Keeps track of the last fo-r chars, used for detecting comments"
;; ;;   (if (or (equal html5-content-model-flag 'pcdata)
;; ;;           (equal html5-content-model-flag 'rcdata))
;; ;;       (progn
;; ;;         (if (equal (length html5-last-four-chars) 5)
;; ;;             (pop html5-last-four-chars))
;; ;;         (push char html5-last-four-chars))))

;; ;; (defun html5-data-state ()
;; ;;   (let ((c (html5-get-char)))
;; ;;     (html5-update-last-four-chars c)
;; ;;     (cond

;; ;;      ;; Check for entity begin
;; ;;      ((and (equal c ?&)
;; ;;            (equal html5-content-model-flag 'pcdata)
;; ;;            (equal html5-content-model-flag 'rcdata)
;; ;;            (not html5-escape-flag))
;; ;;       (setq html5-state 'html5-entity-data-state))

;; ;;      ;; Check for comment begin
;; ;;      ((and (equal c ?-)
;; ;;            (equal html5-content-model-flag 'pcdata)
;; ;;            (equal html5-content-model-flag 'rcdata)
;; ;;            (not html5-escape-flag)
;; ;;            (equal (string html5-last-four-chars)
;; ;;                   "<!--"))
;; ;;       (setq html5-escape-flag t)
;; ;;       (nconc html5-token-queue (list 'characters c)))

;; ;;      ;; Check for tag open
;; ;;      ((and (equal c ?<)
;; ;;            (or (equal html5-content-model-flag 'pcdata)
;; ;;                (and
;; ;;                 (or (equal html5-content-model-flag 'cdata)
;; ;;                     (equal html5-content-model-flag 'rcdata))
;; ;;                 (not html5-escape-flag))))
;; ;;       (setq html5-state 'html5-tag-open-state))

;; ;;      ;; Check for end of comment
;; ;;      ((and (equal c ?>)
;; ;;            (or (equal html5-content-model-flag 'cdata)
;; ;;                (equal html5-content-model-flag 'rcdata))
;; ;;            html5-escape-flag
;; ;;            (equal (string (cdr html5-last-four-chars))
;; ;;                   "-->"))
;; ;;       (setq html5-escape-flag t))

;; ;;      ;; Check for end of file
;; ;;      ((equal c 'EOF)
;; ;;       nil)

;; ;;      ;; Add space characters
;; ;;      ((member c html5-space-chars)
;; ;;       (nconc html5-token-queue (list 'space-characters
;; ;;                                      (concat (string c) (html5-chars-until
;; ;;                                                          html5-space-chars
;; ;;                                                          t)))))

;; ;;      ;; Add content data
;; ;;      (t
;; ;;       ;; Add chars until "&" "<" ">" or "-"
;; ;;       (let ((data (html5-chars-until (list ?& ?< ?> ?-))))
;; ;;         (nconc html5-token-queue (list 'characters data))
;; ;;         ;; Update last-four-chars
;; ;;         ;; TODO Should be list of chars, not string
;; ;;         (setq html5-last-four-chars (substring (concat html5-last-four-chars
;; ;;                                                        (substring data -4 0))
;; ;;                                                -4 0))))))
;; ;;   t)

;; ;; (defun html5-tag-open-state ()
;; ;;   (let ((c (html5-get-char)))
;; ;;     (if (equal html5-content-model-flag 'pcdata)
;; ;;         (cond
;; ;;          ((equal c ?!)
;; ;;           (setq html5-state 'html5-markup-declaration-open-state))
;; ;;          ((equal c ?/)
;; ;;           (setq html5-state 'html5-close-tag-open-state))
;; ;;          ((html5-is-ascii c)
;; ;;           (setq html5-token-queue (list 'start-tag 'data nil)))
;; ;;          ((equal c ?>)
;; ;;           ;; XXX In theory it could be something besides a tag name. But
;; ;;           ;; do we really care?
;; ;;           (nconc html5-token-queue
;; ;;                  (list 'parse-error 'expected-tag-name-but-got-right-bracket))
;; ;;           (nconc html5-token-queue
;; ;;                  (list 'characters "<>")))
;; ;;          ((equal c ??)
;; ;;           (nconc html5-token-queue
;; ;;                  (list 'parse-error 'expected-tag-name-but-got-question-mark))
;; ;;           (decf html5-cursor)
;; ;;           (setq html5-state 'html5-bogus-comment-state))
;; ;;          (t
;; ;;           (nconc html5-token-queue
;; ;;                  (list 'parse-error 'expected-tag-name))
;; ;;           (nconc html5-token-queue
;; ;;                  (list 'parse-error 'characters ?<))))

;; ;;       ;; We know the content model flag is set to either RCDATA or CDATA
;; ;;       ;; now because this state can never be entered with the PLAINTEXT
;; ;;       ;; flag.
;; ;;       (if (equal c ?/)
;; ;;           (setq html5-state 'html5-close-tag-open-state)
;; ;;         (nconc html5-token-queue (list 'characters 'characters ?<))
;; ;;         (decf html5-cursor)
;; ;;         (setq html5-state 'html5-data-state))))
;; ;;   t)

;; ;; (defun html5-markup-declaration-open-state ()
;; ;;   (if (equal (list (html5-get-char) (html5-get-char))
;; ;;              (list ?- ?-))
;; ;;       (progn
;; ;;         (setq html5-current-token ('comment nil))
;; ;;         (setq html5-state 'html5-comment-start-state))
;; ;;     (if (equal (html5-get-string 5) "DOCTYPE
    
;; ;; (defun html5-get-string (length)
;; ;;   (buffer-substring html5-cursor
;; ;;                     (+ html5-cursor 5)))

;; ;; (defun html5-get-char ()
;; ;;   (char-before (incf html5-cursor)))

;; ;; (defun html5-chars-until (chars &optional opposite)
;; ;;   (let ((char (html5-get-char))
;; ;;         (data ()))
;; ;;     (while (or
;; ;;             (not (member (html5-get-char) chars))
;; ;;             (and opposite
;; ;;                  (member (html5-get-char) chars)))
;; ;;       (push char data))
;; ;;     (concat (reverse data))))

;; ;; ;(provide 'html5-token)