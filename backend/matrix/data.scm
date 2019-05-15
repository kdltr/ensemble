
(define-type alist (list-of (pair symbol *)))

(define (alist? o)
  (and (list? o)
       (every (lambda (e) (and (pair? e) (symbol? (car e)))) o)))

;; Event types
;; ===========

(define-type event alist)

(: event? (* -> boolean))
(define (event? o)
  (and (alist? o)
       (alist-ref 'event_id o)
       (alist-ref 'type o)
       (alist? (alist-ref 'content o))
       (alist-ref 'formated o)))

(: timeline-event? (* -> boolean))
(define (timeline-event? o)
  (and (alist? o)
       (string? (alist-ref 'event_id o))
       (string? (alist-ref 'formated o))))

(: hole-event? (list -> boolean))
(define (hole-event? o)
  (and (alist? o)
       (equal? (alist-ref 'type o)
               "com.upyum.ensemble.hole")))

(: checkpoint-event? (list -> boolean))
(define (checkpoint-event? o)
  (and (alist? o)
       (equal? (alist-ref 'type o)
               "com.upyum.ensemble.checkpoint")))

(: make-event (string string list list -> event))
(define (make-event id type content context)
  (let* ((evt `((event_id . ,id)
                (type . ,type)
                (content . ,content)))
         (formated (print-event evt context))
         (evt (cons (cons 'formated formated)
                    evt)))
    (assert (event? evt))
    evt))

(: make-hole-event (string list -> event))
(define (make-hole-event from state)
  (make-event (sprintf "$hole-~A" from)
              "com.upyum.ensemble.hole"
              `((from . ,from)
                (state . ,state))
              '()))

(: make-checkpoint-event (string -> event))
(define (make-checkpoint-event next-batch)
  (make-event (sprintf "$checkpoint-~A" next-batch)
              "com.upyum.ensemble.checkpoint"
              `((to . ,next-batch))
              '()))

;; Timeline management
;; ===================

(define-type timeline (list-of event))

(: timeline-cons (event timeline -> timeline))
(define (timeline-cons event timeline)
  (assert (timeline-event? event))
  (assert (or (not (hole-event? event))
              (null? timeline)
              (checkpoint-event? (car timeline))))
  (cons event timeline))

(: timeline-split (timeline event -> list list))
(define (timeline-split timeline event)
  (let loop ((before timeline)
             (after '()))
    (if (equal? event (car before))
        (values (cdr before) (reverse after))
        (loop (cdr before) (cons (car before) after)))))

(: timeline-append (list #!rest list -> timeline))
(define (timeline-append l1 #!rest rest)
  (define (tl-join l1 l2)
    (fold-right timeline-cons l2 l1))
  (if (null? rest)
      l1
      (apply timeline-append (tl-join l1 (car rest)) (cdr rest))))
