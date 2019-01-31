;; Entry point procedure
(define (handle-input in)
  (cond ((or (number? in) (control-char? in))
         (handle-key in))
        ((char? in)
         (handle-char in)))
  (refresh-inputwin))

(define (control-char? c)
  (or (char<? c #\space)
      (char=? c #\delete)))

(define (refresh-inputwin)
  (werase inputwin)
  (waddstr* inputwin (buffer-window))
  (wmove inputwin 0 (cursor-position)))



;; Buffer for user input
;; =====================

(define input-string "")
(define cursor-pos 0)
(define extent-start 0)

(define (move-cursor #!optional (n 1))
  (let ((mvmt (case n
                ((left) 0)
                ((right) (string-length input-string))
                (else (max 0 (min (+ cursor-pos n)
                                  (string-length input-string)))))))
    (set! cursor-pos mvmt)
    (move-extent cursor-pos)))

(define (move-extent pos)
  (let ((ok-left (<= extent-start pos))
        (ok-right (> (+ extent-start (sub1 cols)) pos)))
    (cond ((not ok-left)
           (set! extent-start (max (- pos cols) 0)))
          ((not ok-right)
           (set! extent-start
             (- pos (sub1 cols)))))))

(define (buffer-window)
  (substring input-string extent-start))

(define (cursor-position)
  (- cursor-pos extent-start))

(define (buffer-insert! c)
  (set! input-string
    (string-append (substring input-string 0 cursor-pos)
                   (string c)
                   (substring input-string cursor-pos))))

(define (buffer-remove! pos)
  (unless (or (< pos 0) (>= pos (string-length input-string)))
    (set! input-string
      (string-append (substring input-string 0 pos)
                     (substring input-string (add1 pos))))))

(define (buffer-kill!)
  (set! input-string
    (substring input-string 0 cursor-pos)))

(define (handle-char c)
  (buffer-insert! c)
  (move-cursor))

(define char-set:non-white
  (char-set-complement char-set:white-space))

(define (find-word-left str pos)
  (cond ((zero? (string-length str)) 0)
        ((char-set-contains? char-set:white-space (string-ref str pos))
         (let ((skipped (string-index-right str char-set:non-white 0 pos)))
           (if skipped (find-word-left str skipped) 0)))
        (else
          (add1 (or (string-skip-right str char-set:non-white 0 pos) -1)))))

(define (find-word-right str pos)
  (cond ((zero? (string-length str)) 0)
        ((char-set-contains? char-set:white-space (string-ref str pos))
         (let ((skipped (string-index str char-set:non-white pos)))
           (if skipped (find-word-right str skipped) (sub1 (string-length str)))))
        (else
          (sub1 (or (string-skip str char-set:non-white pos) (string-length str))))))

;; Key bindings
;; ============

(define (handle-key k)
  (and-let* ((proc (alist-ref k keys equal?))) (proc)))

(define keys '())
(define-syntax define-key
  (syntax-rules ()
    ((_ (k ...) . body) (let ((proc (lambda () . body)))
                           (push! (cons k proc) keys)
                           ...))
    ((_ k . body) (define-key (k) . body))))

(define-key (KEY_BACKSPACE #\delete)
  (buffer-remove! (sub1 cursor-pos))
  (move-cursor -1))

(define-key (KEY_LEFT #\x02) ;; C-b
  (move-cursor -1))

(define-key (KEY_RIGHT #\x06) ;; C-f
  (move-cursor 1))

(define-key (KEY_HOME #\x01) ;; C-a
  (move-cursor 'left))

(define-key (KEY_DC #\x04) ;; C-d
  (buffer-remove! cursor-pos))

(define-key (KEY_END #\x05) ;; C-e
  (move-cursor 'right))

(define-key #\tab
  (unless (or (special-window? *current-window*) (equal? input-string ""))
    (let* ((members-names (ipc-query 'room-members (current-room)))
           (prefix (substring input-string 0 cursor-pos))
           (candidate (find (completion-candidate-checker prefix) members-names)))
      (when candidate
        (when (string-suffix? " (IRC)" candidate) ;; Ugh, kill me now
          (set! candidate (substring candidate 0 (- (string-length candidate) 6))))
        (set! input-string (string-append candidate ": "))
        (move-cursor 'right))
      )))

;; FIXME This is not really right
;;       It should normalize/decompose the unicode strings and remove non-spacing marks
;;       https://www.unicode.org/reports/tr15/
(define (completion-candidate-checker prefix)
  (lambda (str)
    (or (string-prefix? prefix str)
        (string-prefix? prefix (string-filter char-set:latin str)))))

(define-key #\x0B ;; C-k
  (buffer-kill!))

(define-key #\x0C ;; C-l
  (refresh-statuswin)
  (refresh-inputwin)
  (refresh-current-window))

(define-key #\newline
  (cond ((or (string=? "" input-string)
             (string-every char-set:white-space input-string))
         (cond ((room-window? *current-window*)
                (ipc-send 'mark-last-message-as-read (current-room)))
               ((special-window? *current-window*)
                (put! *current-window* 'notifications 0)
                (refresh-statuswin))))
        ((char=? (string-ref input-string 0) #\/)
         (handle-command input-string))
        ((room-window? *current-window*)
         (ipc-send 'message:text (current-room) input-string)))
  (set! input-string "")
  (move-cursor 'left))

(define-key #\escape
  (wtimeout inputwin 0)
  (let ((next (wget_wch inputwin)))
    (wtimeout inputwin -1)
    (if next
        ;; ESC+key, usually used for Alt+key
        (handle-key (vector #\escape next))
        ;; Any code for the ESC key alone here:
        (void))))

(define (move-left-word)
  (define (left)
    (find-word-left input-string (min (sub1 (string-length input-string))
                                      cursor-pos)))
  (let ((new-pos (left)))
    (if (and (= new-pos cursor-pos) (not (zero? new-pos)))
        (begin (move-cursor -1)
               (move-cursor (- (left) cursor-pos)))
        (move-cursor (- new-pos cursor-pos)))))

(define-key #(#\escape #\b)
  (move-left-word))

(define-key #(#\escape #\f)
  (define (right)
    (find-word-right input-string (min (sub1 (string-length input-string))
                                       cursor-pos)))
  (let ((new-pos (right)))
    (if (and (= new-pos cursor-pos) (not (= new-pos (string-length input-string))))
        (begin (move-cursor)
               (move-cursor (- (right) cursor-pos)))
        (move-cursor (add1 (- new-pos cursor-pos))))))

(define-key #(#\escape #\d)
  (unless (= cursor-pos (string-length input-string))
    (set! input-string
      (string-append
        (substring input-string 0 cursor-pos)
        (substring input-string (add1 (find-word-right input-string cursor-pos)))))))

(define-key #\x17 ;; C-w
  (unless (= cursor-pos 0)
    (let ((word-left-pos (find-word-left input-string (sub1 cursor-pos))))
      (set! input-string
        (string-append
          (substring input-string 0 word-left-pos)
          (substring input-string cursor-pos)))
      (move-cursor (- word-left-pos cursor-pos)))))

(define-key #\x0e ;; C-n
  (switch-to-adjacent (append *special-windows* *room-windows*)))

(define-key #\x10 ;; C-p
  (switch-to-adjacent (reverse (append *special-windows* *room-windows*))))

(define-key #(#\escape #\n)
  (switch-to-adjacent (cons *current-window* (weighted-windows-list))))

(define-key #(#\escape #\p)
  (switch-to-adjacent (cons *current-window* (reverse (weighted-windows-list)))))

(define (switch-to-adjacent windows)
  (let ((current-position (memv *current-window* windows)))
    (assert current-position)
    (switch-window
      (if (null? (cdr current-position))
          (car windows)
          (cadr current-position)))))

(define (weighted-windows-list)
  (define (merge a b)
    (append a (fold-right (lambda (o l) (if (memv o a) l (cons o l)))
                          '() b)))
  (let* ((all-windows (append *special-windows* *room-windows*))
         (before-current after-current
          (break (lambda (w) (eqv? w *current-window*))
                 all-windows))
         (ordered (append (if (pair? after-current) (cdr after-current) '())
                          before-current))
         (highlighted (filter window-has-highlight? ordered))
         (notified (filter window-has-notification? ordered)))
    (merge highlighted notified)))


;; History down
(define-key KEY_NPAGE
  (let* ((current-offset (room-offset (current-room)))
         (new-offset (sub1 current-offset)))
    (if (= new-offset 0)
        (begin
          (room-offset-delete! (current-room))
          (ipc-send 'subscribe (current-room)))
        (room-offset-set! (current-room) (max 0 new-offset)))
    (refresh-current-window)))

;; History up
(define-key KEY_PPAGE
  (let* ((current-offset (room-offset (current-room)))
         (new-offset (add1 current-offset)))
    (room-offset-set! (current-room) new-offset)
    (ipc-send 'unsubscribe (current-room))
    (refresh-current-window)))


;; Commands
;; ========

(define (handle-command str)
  (let* ((cmdline (string-split (string-drop str 1) " "))
         (cmd (string->symbol (car cmdline)))
         (args-index (string-index str #\space))
         (joined-args (substring str (if args-index
                                         (add1 args-index)
                                         (string-length str))))
         (args (cdr cmdline))
         (proc (alist-ref cmd *commands*)))
    (if proc
        (proc joined-args args)
        (special-window-write 'ensemble "Unknown command: ~a" cmd))))

(define *commands* '())
(define-syntax define-command
  (syntax-rules ()
    ((_ (sym ...) joined args . body) (let ((proc (lambda (joined args) . body)))
                                        (push! (cons 'sym proc) *commands*)
                                ...))
    ((_ sym joined args . body) (define-command (sym) joined args . body))))

(define-command (window w r) joined args
  (switch-window (string->symbol (string-downcase joined))))

(define-command rename joined args
  (let ((target (string->symbol joined)))
    (cond ((null? args)
           (special-window-write 'ensemble
                                 "usage: /rename NEW-NAME"))
          ((room-window? target)
           (special-window-write 'ensemble
                                 "rename: the '~a' window already exists"
                                 target))
           (else
             (rename-window *current-window* target)))))

(define-command say joined args
  (when (room-window? *current-window*)
    (ipc-send 'message:text (current-room) joined)))

(define-command me joined args
  (ipc-send 'message:emote (current-room) joined))

(define-command list joined args
  (special-window-write 'ensemble "beginning of list")
  (for-each
    (lambda (window)
      (special-window-write
        'ensemble "~a: ~a"
         window (ipc-query 'room-display-name (window-room window))))
    *room-windows*)
  (special-window-write 'ensemble "end of list"))

(define-command (find f) joined args
  (let ((matching-rooms (ipc-query 'find-room joined)))
    (cond ((null? matching-rooms))
          ((null? (cdr matching-rooms))
           (switch-window (window-for-room (car matching-rooms))))
          (else
            (switch-window 'ensemble)
            (special-window-write 'ensemble "Rooms matching '~a'" joined)
            (for-each
              (lambda (room-id)
                (special-window-write
                  'ensemble "~a: ~a"
                  (window-for-room room-id) (ipc-query 'room-display-name room-id)))
              matching-rooms)
            (special-window-write 'ensemble "end of /find results")))))

(define-command login joined args
  (let ((server username password (run-login-prompt)))
    (ipc-send 'login server username password)))

(define-command join joined args
  (ipc-send 'join-room joined))

(define-command leave joined args
  (if (null? args)
      (ipc-send 'leave-room (window-room *current-window*))
      (ipc-send 'leave-room joined)))

(define-command (exit quit) joined args
  (exit))
