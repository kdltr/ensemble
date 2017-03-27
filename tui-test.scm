(use posix stty ansi-escape-sequences utf8 utf8-srfi-13)

(load "tui-lib")

(define tty (get-environment-variable "TTY"))

(unless tty (error "please define the TTY environment variable"))

(define termin (open-input-file tty))
(define termout (open-output-file tty))

(unless (terminal-port? termout)
  (error "specified file is not a terminal" tty))

(define-values (rows columns) (terminal-size termout))

(set-buffering-mode! termin #:none)
(set-buffering-mode! termout #:full 4096)

(load "tui/input")

;; WHOLE TERMINAL
(define buffer (make-grid columns rows))

;; TITLE
(define title-bar (make-grid columns 1))
(fill! title-bar #\space bg: 'magenta)
(gprint! title-bar 0 0 "title bar" bg: 'magenta)

;; STATUS
(define status-bar (make-grid columns 1))
(fill! status-bar #\space bg: 'red)
(gprint! status-bar 0 0 "status bar" bg: 'red)

(define messages-height (- (grid-height buffer)
                           (grid-height title-bar)
                           (grid-height status-bar)
                           (grid-height input-bar)))

;; MEMBERS
(define members-frame (make-grid 13 messages-height))
(fill! members-frame #\space bg: 'blue)
(gprint! members-frame 0 0 "members frame" bg: 'blue)

;; MESSAGES
(define messages-width (- (grid-width buffer) (grid-width members-frame)))

(define messages-frame (make-grid messages-width messages-height))
(fill! messages-frame #\space bg: 'white)
(gprint! messages-frame 0 0 "messages frame" fg: 'black bg: 'white)


(define (blit-all!)
  (blit! buffer 0 0 title-bar)
  (blit! buffer 0 1 messages-frame)
  (blit! buffer (- columns (grid-width members-frame)) 1 members-frame)
  (blit! buffer 0 (- rows 2) status-bar)
  (blit! buffer 0 (sub1 rows) input-bar))

(define (refresh!)
  (blit-all!)
  (with-output-to-port termout
    (lambda ()
      (display (hide-cursor))
      (draw-grid! buffer)
      (display (cursor-position rows (add1 cursor-pos)))
      (display (show-cursor))
      (flush-output))))

(define (main-loop)
  (refresh!)
  (advance-input!)
  (main-loop))


;; Message in message-frame
#|
(define nick "Kooda")
(define msg (make-string 120 #\m))

(define nick1 (make-grid (add1 (string-length nick)) 2))
(fill! nick1 '() 'black 'cyan #\space)
(gprint! nick1 '() 'black 'cyan (string-append nick "|"))
(grid-put-char! nick1 (add1 (string-length nick)) 2 '() 'black 'cyan #\|)

(define content-length (- (grid-width messages-frame) (grid-width nick1)))
(define content1 (make-grid content-length (add1 (quotient (string-length msg) content-length))))
(fill! content1 '() 'black 'magenta #\space)
(gprint! content1 '() 'black 'magenta msg)

(define message1 (make-grid (grid-width messages-frame) (grid-height content1)))
(blit! message1 1 1 nick1)
(blit! message1 (add1 (grid-width nick1)) 1 content1)

(blit! messages-frame 1 1 message1)
|#