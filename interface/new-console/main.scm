(import
  (chicken format)
  (chicken io)
  (chicken port)
  (chicken process-context)
  (ensemble libs bindings)
  (ensemble libs history)
  srfi-1
  srfi-71
  ioctl
  stty
  utf8)


;; Unicode madness
;; ===============

(define (string-width s)
  (fold (lambda (c sum) (+ sum (char-width c)))
        0
        (string->list s)))

(define (string-height str max-width)
  (let ((q r (quotient&remainder (string-width str) max-width)))
    (+ q (if (zero? r) 0 1))))

(define (string-chop/width str max-width)
  (let lp ((start 0)
           (end 0)
           (cur-width 0))
    (if (or (= end (string-length str))
            (< max-width 2))
        (list (substring str start end))
        (let* ((char (string-ref str end))
               (cw (char-width char))
               (next-width (+ cur-width cw)))
          (if (or (> next-width max-width)
                  (> cw max-width))
              (cons (substring str start end)
                    (lp end end 0))
              (lp start (add1 end) next-width))))))

(define (sanitize-string str)
  (list->string
    (map sanitize-char (string->list str))))

(define (sanitize-char chr)
  (cond ;; whitelist
        ((memv chr '(#\space))
         chr)
        ;; delete has a special encoding
        ((char=? chr #\delete)
         #\x2421)
        ;; C0 control characters
        ((and (char>=? chr #\null)
              (char<=? chr #\space))
         (integer->char (+ #x2400 (char->integer chr))))
        ;; C1 control characters
        ((and (char>=? chr #\x80)
              (char<=? chr #\x9F))
         ;; SYMBOL FOR SUBSTITUTE FORM TWO
         #\x2426)
        ;; every other character passes through
        (#t chr)))


;; Terminal escape sequences
;; =========================

(define (csi-sequence str . args)
  (sprintf "\x1b[~?" str args))

;; TODO 8bit / escape-less control
#;(define (csi-sequence str . args)
  (sprintf "\x9B~?" str args))

(define ((dip-switch what) bool)
  (csi-sequence "?~a~a" what (if bool #\h #\l)))

(define (gfx-sequence n)
  (csi-sequence "~am" n))

(define cursor-visibility (dip-switch 25))
(define meta-mode (dip-switch 1034))
(define tui-mode (dip-switch 1049))
(define bracketed-paste-mode (dip-switch 2004))

(define cursor-to-end 0)
(define cursor-to-beginning 1)
(define entirety 2)

(define (cursor-position x y) (csi-sequence "~a;~aH" y x))
(define (erase-in-display part) (csi-sequence "~aJ" part))
(define (erase-in-line part) (csi-sequence "~aK" part))
(define (scroll-up n) (csi-sequence "~aS" n))
(define (scroll-down n) (csi-sequence "~aT" n))

(define (gfx-reset) (gfx-sequence 0))
(define (gfx-bold) (gfx-sequence 1))
(define (gfx-normal) (gfx-sequence 22))
(define (gfx-reverse) (gfx-sequence 7))

(define (gfx-fg n) (csi-sequence "38;5;~am" n))
(define (gfx-bg n) (csi-sequence "48;5;~am" n))
(define (gfx-fg-default) (gfx-sequence 39))
(define (gfx-bg-default) (gfx-sequence 49))

(define (bell) #\alarm)


;; Terminal setup and I/O
;; ======================

(define *terminal-output-port* (current-output-port))
(define *terminal-input-port* (current-input-port))

(define *original-terminal-attributes* #f)

(define *terminal-width* 1)
(define *terminal-height* 1)
(define *interface-height* 3)

(define (output-to-terminal . args)
  (define (display-list l)
    (for-each
      (lambda (o)
        (assert (or (char? o) (string? o) (pair? o) (null? o)))
        (if (or (pair? o) (null? o))
            (display-list o)
            (display o *terminal-output-port*)))
      l))
  (display-list args)
  (flush-output *terminal-output-port*))

(define (resize-terminal)
  (let ((size (ioctl-winsize *terminal-output-port*)))
    (set! *terminal-height* (max 1 (car size)))
    (set! *terminal-width* (max 1 (cadr size)))))

(define (cleanup-terminal)
  (output-to-terminal (erase-in-display entirety)
                      (tui-mode #f)
                      (cursor-visibility #t)))

(define (restore-terminal-attributes)
  (when *original-terminal-attributes*
    (set-terminal-attributes! *terminal-output-port*
                              TCSANOW
                              *original-terminal-attributes*)
    (free-term-attrs *original-terminal-attributes*)
    (set! *original-terminal-attributes* #f)))

(define (open-terminal)
  (and-let* ((tty-filename (get-environment-variable "ENSEMBLE_TTY"))
             (outport (open-output-file tty-filename))
             (inport (open-input-file tty-filename))
             (attr (get-terminal-attributes outport)))
       (set! *original-terminal-attributes* attr)
       (set! *terminal-output-port* outport)
       (set! *terminal-input-port* inport)
       (set-buffering-mode! *terminal-output-port* #:full)
       (set-buffering-mode! *terminal-input-port* #:none)))

(define (setup-terminal)
  (open-terminal)
  (on-exit (lambda ()
             (cleanup-terminal)
             (restore-terminal-attributes)))
  (output-to-terminal (tui-mode #t)
                      (erase-in-display entirety)
                      (cursor-visibility #f)
                      (cursor-position 1 1))
  (resize-terminal)
  (stty *terminal-output-port* '(not echo icanon)))


;; Interface rendering
;; ===================

(define (titlebar-render text)
  (let* ((len (string-width text))
         (rest (- *terminal-width* len)))
    (list (cursor-position 0 0)
          (gfx-bg 7)
          (gfx-fg 0)
          (substring text 0 (min len *terminal-width*))
          (make-string (max rest 0) #\space)
          (gfx-reset))))

(define (statusbar-render)
  (list (cursor-position 0 (sub1 *terminal-height*))
        (gfx-bg 7)
        (gfx-fg 0)
        "[----] | "
        (gfx-fg 8)
        "ensemble"
        #\space
        (gfx-fg 0)
        "[pat]"
        #\space
        (gfx-fg 4)
        "chi(1:4)"
        #\space
        (gfx-fg 0)
        "fed(:25)"
        (gfx-normal)
        (make-string (max (- *terminal-width* 15) 0) #\space)
        (gfx-reset)))

(define (inputbar-render)
  (list (cursor-position 1 *terminal-height*)
        (erase-in-line entirety)))

(define (test-render)
  (list (titlebar-render "Pattes de Rennes et des environs … et les copains aussi")
        (statusbar-render)
        (inputbar-render)))

(define (print-message msg)
  (output-to-terminal (scroll-up 1)
                      (cursor-position 1 (- *terminal-height* 2))
                      (erase-in-line entirety)
                      msg
                      (test-render)))


;; History management
;; ==================

;; history is a doubly-linked list of messages
(define *history* '())
(define *focus* #f)

(define (new-history-message! msg)
  (set! *history*
    (history-insert-after! msg *history*)))

(define (node-n-lines-above n node start)
  (let lp ((node node)
           (n (- n start)))
    (if (< n 0)
        node
        (lp (history-previous node)
            (- n (message-height (history-data node)))))))

(define (node-n-lines-above n node start)
  (if (< (- n start) 0)
      (values node (- start n))
      (let* ((new-node (history-previous node)))
        (if (null? new-node)
            (values '() 0)
            (node-n-lines-above (- n start)
                                new-node
                                (message-height (history-data new-node)))))))

(define (topmost-node start-node)
  (let ((prev (history-previous start-node)))
    (if (null? prev) start-node (topmost-node prev))))

;; Messages
;; --------
(define-record message id text split-width split)

(define (message #!key (id (gensym)) (type 'message) text)
  (assert text)
  (ensure-message-split! (make-message id (sanitize-string text) -1 '())))

(define (ensure-message-split! msg)
  (when (not (= (message-split-width msg) *terminal-width*))
    (message-split-width-set! msg *terminal-width*)
    (message-split-set! msg (string-chop/width (message-text msg) *terminal-width*)))
  msg)

(define (message-height msg)
  (length (message-split (ensure-message-split! msg))))

(define (message-line-ref msg line)
  (ensure-message-split! msg)
  (assert (<= 1 line (message-height msg)))
  (list-ref (message-split msg) (sub1 line)))

;; Focus point
;; -----------
(define-record focus node line)

(define (current-focus)
  (or *focus*
      (make-focus *history* (message-height (history-data *history*)))))

(define (focus-up)
  (let* ((focus (current-focus))
         (new-line (sub1 (focus-line focus))))
    (set! *focus*
      (if (>= new-line 1)
          (make-focus (focus-node focus) new-line)
          (let ((new-node (history-previous (focus-node focus))))
            (if (null? new-node)
                focus
                (make-focus new-node (message-height (history-data new-node)))))))))

(define (focus-down)
  (when *focus*
    (set! *focus*
      (let* ((node (focus-node *focus*))
             (msg (history-data node))
             (line (focus-line *focus*)))
        (if (= line (message-height msg))
            (let ((new-node (history-next node)))
              (if (null? new-node)
                  #f
                  (make-focus new-node 1)))
            (make-focus node (+ line 1)))))))

(define (focused-line)
  (let ((focus (current-focus)))
    (message-line-ref (history-data (focus-node focus))
                      (focus-line focus))))


#|
;; IPC Implementation
;; ==================

(import (chicken syntax) (chicken type) srfi-69)

(define *current-room*)

(begin-for-syntax
  (define *ipc-receiver* 'frontend)
  (define *ipc-send-procedure* 'ipc-send))
(define *ipc-hash-table* (make-hash-table))
(include "ipc.scm")
|#

;; Testing
;; =======

(define (load-test-history!)
  (with-input-from-file "test-data"
    (lambda () (port-for-each (lambda (m)
                                (new-history-message! (message text: m)))
                              read-line))))

(define (load-additional-test-history!)
  (new-history-message! (message text: "[19/05 12:16] <Hiro Lynx> Trop bien :3"))
  (new-history-message! (message text: "[19/05 20:19] <Tifur> <3 :3"))
  (new-history-message! (message text: "[19/05 21:12] <Youbi> C’est cool quand deux films français sont trouvables sur tpb, et pas sur un tracker français only :3"))
  (new-history-message! (message text: "[21/05 12:33] <Kͦaͭsͭiͤl> 人人生来自由，在尊严和权利上一律平等。他们有理性和良心，请以手足关系的精神相对待。"))
  (new-history-message! (message text: "[22/05 13:09] <Kooda> Ceci est un petit message\nAvec des retours à la ligne ! Dingue !\n\n")))


(define (page-up)
  (let* ((focus (current-focus))
         (hist line (node-n-lines-above (- *terminal-height* *interface-height*)
                                        (focus-node focus)
                                        (focus-line focus))))
    (if (null? hist)
        '()
        (let* ((msg (history-data hist))
               (height (message-height msg)))
          (focus-up)
          (list (scroll-down 1)
                (cursor-position 1 2)
                (erase-in-line entirety)
                (map (lambda (i) (message-line-ref msg i))
                     (iota (- height line -1) line))
                (test-render))))))

(define (page-down)
  (focus-down)
  (if *focus*
      (let ((msg (history-data (focus-node *focus*)))
            (line (focus-line *focus*)))
        (list (scroll-up 1)
              (cursor-position 1 (- *terminal-height* 1 line))
              (erase-in-display cursor-to-end)
              (map (lambda (i) (message-line-ref msg i))
                   (iota line 1))
              (test-render)))
      '()))

(define (input-loop)
  (let ((char (read-char *terminal-input-port*)))
    (case char
      ((#\a) (output-to-terminal (page-up)) (input-loop))
      ((#\u) (output-to-terminal (page-down)) (input-loop))
      ((#\i) (output-to-terminal (handle-resize)) (input-loop)))))

(define (handle-resize)
  (resize-terminal)
  (let* ((focus (current-focus))
         (end (focus-node focus))
         (end-msg (history-data end))
         (end-line (focus-line focus))
         (start (node-n-lines-above *terminal-height* end end-line))
         (start (if (null? start) (topmost-node end) start)))
    (list (cursor-position 1 1)
          (erase-in-display entirety)
          (let lp ((hist start))
            (if (eqv? hist end)
                '()
                (cons* (message-text (history-data hist))
                       #\newline
                       (lp (history-next hist)))))
          (map (lambda (i) (message-line-ref end-msg i))
               (iota (min end-line (message-height end-msg)) 1))
          (scroll-up 2)
          (test-render))))

(setup-terminal)

(set! *history* '())
(set! *focus* #f)
(load-test-history!)
(load-additional-test-history!)