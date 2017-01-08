#lang racket

(define basic-element%
  (class object%
    (super-new)
    
    (field [input-table (make-hash null)]
           [output-table (make-hash null)]
           [transistor-count 0])
    
    (define/public (get-input interface)
      (hash-ref input-table interface))
    
    (define/public (get-output interface)
      (hash-ref output-table interface))
    
    (define/public (set-input! interface value)
      (hash-set! input-table interface value))
    
    (define/public (set-output! interface value)
      (hash-set! output-table interface value))
    
    (define/public (get-transistor-count)
      transistor-count)
    
    (define/public (set-transistor-count! value)
      (set! transistor-count value))
    
    (define/public (process) void)
    ))

(define connector%
  (class object%
    (super-new)
    (init-field from-neighbor from-neighbor-interface
                to-neighbor to-neighbor-interface)
    
    (define/public (process)
      (send to-neighbor set-input! to-neighbor-interface
            (send from-neighbor get-output from-neighbor-interface)))
    ))

(define relais%
  (class basic-element%
    (super-new)
    
    (send this set-transistor-count! 1)
    
    (send this set-input! 'control 0)
    (send this set-input! 'in 1)
    (send this set-output! 'Q 0)
    (send this set-output! 'Q-bar 1)
    
    (define/override (process)
      (if (= 0 (send this get-input 'in))
          (begin (send this set-output! 'Q 0)
                 (send this set-output! 'Q-bar 0))
          (if (= 0 (send this get-input 'control))
              (begin (send this set-output! 'Q 0)
                     (send this set-output! 'Q-bar 1))
              (begin (send this set-output! 'Q 1)
                     (send this set-output! 'Q-bar 0)))))  
    ))


(define inverter%
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define relais (make-object relais%))
    (send this set-input! 'in 0)
    (send this set-output! 'out 1)
    (send this set-transistor-count! (send relais get-transistor-count))
    
    (define/override (set-input! interface value)
      (hash-set! input-table interface value)
      (send relais set-input! 'control value)) 
    
    (define/override (process)
      (send relais process)
      (send this set-output! 'out (send relais get-output 'Q-bar)))
    ))

(define and-gate%
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define relais1 (make-object relais%))
    (define relais2 (make-object relais%))
    (define connector (make-object connector% relais1 'Q relais2 'in))
    
    (send this set-input! 'A-in 0)
    (send this set-input! 'B-in 0)
    (send this set-output! 'out 0)
    (send this set-transistor-count! (+ (send relais1 get-transistor-count)
                                        (send relais2 get-transistor-count)))
    
    (define/override (set-input! interface value)
      (hash-set! input-table interface value)
      (cond ((eq? interface 'A-in)
             (send relais1 set-input! 'control value))
            ((eq? interface 'B-in)
             (send relais2 set-input! 'control value))
            (else (eprintf "Unknown Interface Type")))) 
    
    (define/override (process)
      (send relais1 process)
      (send connector process)
      (send relais2 process)
      (send this set-output! 'out (send relais2 get-output 'Q)))    
    ))

(define or-gate%
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define relais1 (make-object relais%))
    (define relais2 (make-object relais%))
    
    (send this set-input! 'A-in 0)
    (send this set-input! 'B-in 0)
    (send this set-output! 'out 0)
    (send this set-transistor-count! (+ (get-field transistor-count relais1)
                                        (get-field transistor-count relais2)))
    
    (define/override (set-input! interface value)
      (hash-set! input-table interface value)
      (cond ((eq? interface 'A-in)
             (send relais1 set-input! 'control value))
            ((eq? interface 'B-in)
             (send relais2 set-input! 'control value))
            (else (eprintf "set!-interface --- unknown interface name ~a\n" interface))))
    
    (define/override (process)
      (send relais1 process)
      (send relais2 process)
      (send this set-output! 'out
            (max (send relais1 get-output 'Q)
                 (send relais2 get-output 'Q))))
    ))

(define nand-gate%
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define relais1 (make-object relais%))
    (define relais2 (make-object relais%))
    
    (send this set-input! 'A-in 0)
    (send this set-input! 'B-in 0)
    (send this set-output! 'out 1)
    (send this set-transistor-count! (+ (get-field transistor-count relais1)
                                        (get-field transistor-count relais2)))
    
    (define/override (set-input! interface value)
      (hash-set! input-table interface value)
      (cond ((eq? interface 'A-in)
             (send relais1 set-input! 'control value))
            ((eq? interface 'B-in)
             (send relais2 set-input! 'control value))
            (else (eprintf "set!-interface --- unknown interface name ~a\n" interface))))
    
    (define/override (process)
      (send relais1 process)
      (send relais2 process)
      (send this set-output! 'out
            (max (send relais1 get-output 'Q-bar)
                 (send relais2 get-output 'Q-bar))))
    ))

(define nor-gate%
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define relais1 (make-object relais%))
    (define relais2 (make-object relais%))
    (define connector (make-object connector% relais1 'Q-bar relais2 'in))
    
    (send this set-input! 'A-in 0)
    (send this set-input! 'B-in 0)
    (send this set-output! 'out 0)
    (send this set-transistor-count! (+ (send relais1 get-transistor-count)
                                        (send relais2 get-transistor-count)))
    
    (define/override (set-input! interface value)
      (hash-set! input-table interface value)
      (cond ((eq? interface 'A-in)
             (send relais1 set-input! 'control value))
            ((eq? interface 'B-in)
             (send relais2 set-input! 'control value))
            (else (eprintf "Unknown Interface Type")))) 
    
    (define/override (process)
      (send relais1 process)
      (send connector process)
      (send relais2 process)
      (send this set-output! 'out (send relais2 get-output 'Q-bar))) 
    ))

(define xor-gate%
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define or-gate (new or-gate%))
    (define nand-gate (new nand-gate%))
    (define and-gate (new and-gate%))
    (define connector-1
      (make-object connector% or-gate 'out and-gate 'A-in))
    (define connector-2
      (make-object connector% nand-gate 'out and-gate 'B-in))
    
    (send this set-input! 'A-in 0)
    (send this set-input! 'B-in 0)
    (send this set-output! 'out 1)
    (send this set-transistor-count! (+ (get-field transistor-count or-gate)
                                        (get-field transistor-count nand-gate)
                                        (get-field transistor-count and-gate)))
    
    (define/override (set-input! interface value)
      (hash-set! input-table interface value)
      (cond ((eq? interface 'A-in)
             (send or-gate set-input! 'A-in value)
             (send nand-gate set-input! 'A-in value))
            ((eq? interface 'B-in)
             (send or-gate set-input! 'B-in value)
             (send nand-gate set-input! 'B-in value))
            (else (eprintf "set!-interface --- unknown interface name ~a\n" interface))))
    
    (define/override (process)
      (send or-gate process)
      (send nand-gate process)
      (send connector-1 process)
      (send connector-2 process)
      (send and-gate process)
      (send this set-output! 'out (send and-gate get-output 'out)))
    ))

(define half-adder%
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define xor-gate (new xor-gate%))
    (define and-gate (new and-gate%))
    
    (send this set-input! 'A-in 0)
    (send this set-input! 'B-in 0)
    (send this set-output! 'Sum-out 0)
    (send this set-output! 'Carry-out 0)
    (send this set-transistor-count! (+ (get-field transistor-count xor-gate)
                                        (get-field transistor-count and-gate)))
    
    (define/override (set-input! interface value)
      (hash-set! input-table interface value)
      (cond ((eq? interface 'A-in)
             (send xor-gate set-input! 'A-in value)
             (send and-gate set-input! 'A-in value))
            ((eq? interface 'B-in)
             (send xor-gate set-input! 'B-in value)
             (send and-gate set-input! 'B-in value))
            (else (eprintf "set-input! --- unknown interface name ~a\n" interface))))
    
    (define/override (process)
      (send xor-gate process)
      (send and-gate process)
      (send this set-output! 'Sum-out (send xor-gate get-output 'out))
      (send this set-output! 'Carry-out (send and-gate get-output 'out)))
    ))


(define full-adder%
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define half-adder-1 (new half-adder%))
    (define half-adder-2 (new half-adder%))
    (define or-gate (new or-gate%))
    (define connector-1
      (make-object connector% half-adder-1 'Carry-out or-gate 'B-in))
    (define connector-2
      (make-object connector% half-adder-1 'Sum-out half-adder-2 'B-in))
    (define connector-3
      (make-object connector% half-adder-2 'Carry-out or-gate 'A-in))
    
    (send this set-input! 'Carry-in 0)
    (send this set-input! 'A-in 0)
    (send this set-input! 'B-in 0)
    (send this set-output! 'Sum-out 0)
    (send this set-output! 'Carry-out 0)
    (send this set-transistor-count! (+ (get-field transistor-count or-gate)
                                        (get-field transistor-count half-adder-1)
                                        (get-field transistor-count half-adder-2)))
    
    (define/override (set-input! interface value)
      (hash-set! input-table interface value)
      (cond ((eq? interface 'Carry-in)
             (send half-adder-2 set-input! 'A-in value))
            ((eq? interface 'A-in)
             (send half-adder-1 set-input! 'A-in value))
            ((eq? interface 'B-in)
             (send half-adder-1 set-input! 'B-in value))
            (else (eprintf "set-input! --- unknown interface name ~a\n" interface))))
    
    (define/override (process)
      (send half-adder-1 process)
      (send connector-1 process)
      (send connector-2 process)
      (send half-adder-2 process)
      (send connector-3 process)
      (send or-gate process)
      (send this set-output! 'Sum-out (send half-adder-2 get-output 'Sum-out))
      (send this set-output! 'Carry-out (send or-gate get-output 'out)))
    ))

(define 8-bit-adder%
  (class object%
    (super-new)
    (init-field [Carry-in 0]
                [A-in '(0 0 0 0 0 0 0 0)] ; Caution: least-significant bit is on the *left*
                [B-in '(0 0 0 0 0 0 0 0)] ; To have correspondence with list-ref
                [Sum-out '(0 0 0 0 0 0 0 0)]
                [Carry-out 0])
    
    (define BANKSIZE 8)
    
    (define/public (get-interface interface)
      (cond ((eq? interface 'Sum-out) Sum-out)
            ((eq? interface 'Carry-out) Carry-out)
            (else (error "get-interface --- unknown interface name"))))
    
    (define/public (set!-interface interface value)
      (cond ((eq? interface 'Carry-in)
             (set! Carry-in value)
             (send (list-ref full-adder-bank 0) set!-interface 'Carry-in value))
            ((eq? interface 'A-in)
             (set! A-in value)
             (for ([i BANKSIZE])
               (send (list-ref full-adder-bank i) set!-interface 'A-in (list-ref A-in i))))
            ((eq? interface 'B-in)
             (set! B-in value)
             (for ([i BANKSIZE])
               (send (list-ref full-adder-bank i) set!-interface 'B-in (list-ref B-in i))))
            (else (eprintf "set!-interface --- unknown interface name ~a\n" interface))))
    
    (define/public (set-input-fields! Carry-in-value A-in-value B-in-value)
      (set!-interface 'Carry-in Carry-in-value)
      (set!-interface 'A-in (reverse A-in-value))
      (set!-interface 'B-in (reverse B-in-value)))
    
    (define/public (process)
      (for ([i (sub1 BANKSIZE)])
        (send (list-ref full-adder-bank i) process)
        (send (list-ref connector-list i) process)
        )
      (send (list-ref full-adder-bank (sub1 BANKSIZE)) process)
      (set! Sum-out
            (for/list ([i BANKSIZE])
              (send (list-ref full-adder-bank i) get-interface 'Sum-out)
              ))
      (set! Carry-out (send (list-ref full-adder-bank (sub1 BANKSIZE)) get-interface 'Carry-out))
      )
    
    (define/public (get-all-fields)
      (list Carry-in (reverse A-in) (reverse B-in) Carry-out (reverse Sum-out)))
    
    (define/public (get-output-fields)
      (list Carry-out (reverse Sum-out)))
    
    (define full-adder-bank
      (for/list ([i BANKSIZE])
        (new full-adder%)))
    
    (define connector-list
      (for/list ([i (sub1 BANKSIZE)])
        (make-object connector%
          (list-ref full-adder-bank i) 'Carry-out
          (list-ref full-adder-bank (add1 i)) 'Carry-in)))
    
    (field [transistor-count
            (foldl
             +
             0
             (map (lambda (adder)
                    (get-field transistor-count adder))
                  full-adder-bank)
             )])
    ))

(define R-S-flip-flop%
  (class object%
    (super-new)
    (init-field [R-in 0] [S-in 0] [Q 0] [Q-bar 1])
    
    (define/public (get-interface interface)
      (cond ((eq? interface 'Q) Q)
            ((eq? interface 'Q-bar) Q-bar)
            (else (error "get-interface --- unknown interface name"))))
    
    (define/public (set!-interface interface value)
      (cond ((eq? interface 'R-in)
             (set! R-in value)
             (send nor-gate-1 set!-interface 'A-in value))
            ((eq? interface 'S-in)
             (set! S-in value)
             (send nor-gate-2 set!-interface 'B-in value))
            (else (eprintf "set!-interface --- unknown interface name ~a\n" interface))))
    
    (define/public (set-input-fields! R-in-value S-in-value)
      (set!-interface 'R-in R-in-value)
      (set!-interface 'S-in S-in-value))
    
    (define/public (process)
      (send nor-gate-1 process) (send connector-1 process)
      (send nor-gate-2 process) (send connector-2 process)
      (send nor-gate-1 process) (send connector-1 process)
      (set! Q (send nor-gate-1 get-interface 'out))
      (set! Q-bar (send nor-gate-2 get-interface 'out)))
    
    (define/public (get-all-fields)
      (list R-in S-in Q Q-bar))
    
    (define/public (get-output-fields)
      (list Q Q-bar))
    
    (define nor-gate-1 (new nor-gate%))
    (define nor-gate-2 (new nor-gate%))
    (define connector-1
      (make-object connector% nor-gate-1 'out nor-gate-2 'A-in))
    (define connector-2
      (make-object connector% nor-gate-2 'out nor-gate-1 'B-in))
    
    (field [transistor-count (+ (get-field transistor-count nor-gate-1)
                                (get-field transistor-count nor-gate-2))])
    ))

(define level-triggered-D-type-latch%
  (class object%
    (super-new)
    (init-field [Data-in 0] [Clock-in 0] [Q 0] [Q-bar 1])
    
    (define/public (get-interface interface)
      (cond ((eq? interface 'Q) Q)
            ((eq? interface 'Q-bar) Q-bar)
            (else (error "get-interface --- unknown interface name"))))
    
    (define/public (set!-interface interface value)
      (cond ((eq? interface 'Data-in)
             (set! Data-in value)
             (send inverter set!-interface 'in value)
             (send and-gate-2 set!-interface 'B-in value))
            ((eq? interface 'Clock-in)
             (set! Clock-in value)
             (send and-gate-1 set!-interface 'B-in value)
             (send and-gate-2 set!-interface 'A-in value))
            (else (eprintf "set!-interface --- unknown interface name ~a\n" interface))))
    
    (define/public (set-input-fields! Data-in-value Clock-in-value)
      (set!-interface 'Data-in Data-in-value)
      (set!-interface 'Clock-in Clock-in-value))
    
    (define/public (process)
      (send inverter process) (send connector-1 process)
      (send and-gate-1 process) (send connector-2 process)
      (send and-gate-2 process) (send connector-3 process)
      (send R-S-flip-flop process) 
      (set! Q (send R-S-flip-flop get-interface 'Q))
      (set! Q-bar (send R-S-flip-flop get-interface 'Q-bar)))
    
    (define/public (get-all-fields)
      (list Data-in Clock-in Q Q-bar))
    
    (define/public (get-output-fields)
      (list Q Q-bar))
    
    (define and-gate-1 (new and-gate%))
    (define and-gate-2 (new and-gate%))
    (define inverter (new inverter%))
    (define R-S-flip-flop (new R-S-flip-flop%))
    (define connector-1
      (make-object connector% inverter 'out and-gate-1 'A-in))
    (define connector-2
      (make-object connector% and-gate-1 'out R-S-flip-flop 'R-in))
    (define connector-3
      (make-object connector% and-gate-2 'out R-S-flip-flop 'S-in))
    
    (field [transistor-count (+ (get-field transistor-count and-gate-1)
                                (get-field transistor-count and-gate-2)
                                (get-field transistor-count inverter)
                                (get-field transistor-count R-S-flip-flop))])
    
    ))


(module+ test
  (require rackunit)
  
  (define (test-input-output object
                             input-table
                             output-table
                             test-values
                             transistor-count)
    (for-each
     (lambda (input-and-output-values-list)
       (let ([input-values (car input-and-output-values-list)]
             [predicted-output-values (cadr input-and-output-values-list)])
         (for-each
          (lambda (input-interface input-value)
            (send object set-input! input-interface input-value))
          input-table input-values)                
         (send object process)
         (let ([calculated-output-values
                (map (lambda (output-interface)
                       (send object get-output output-interface))
                     output-table)])
           (check-equal? predicted-output-values calculated-output-values)
           )))
     test-values)
    (check-eq? (send object get-transistor-count) transistor-count)
    )
  
  (test-case
   "relais"  
   (test-input-output (new relais%) 
                      '(control in) '(Q Q-bar)
                      '(((0 1) (0 1))
                        ((1 1) (1 0))
                        ((1 0) (0 0))
                        ((0 0) (0 0)))
                      1)
   )
  
  (test-case
   "inverter"
   (test-input-output (new inverter%)
                      '(in) '(out)
                      '(((0) (1))
                        ((1) (0)))
                      1)
   )
  
  (test-case
   "and-gate"
   (test-input-output (new and-gate%)
                      '(A-in B-in) '(out)
                      '(((0 0) (0))
                        ((0 1) (0))
                        ((1 0) (0))
                        ((1 1) (1)))
                      2)
   )
  
  (test-case
   "or-gate"
   (test-input-output (new or-gate%)
                      '(A-in B-in) '(out)
                      '(((0 0) (0))
                        ((0 1) (1))
                        ((1 0) (1))
                        ((1 1) (1)))
                      2)
   )
  
  (test-case
   "nand-gate"
   (test-input-output (new nand-gate%)
                      '(A-in B-in) '(out)
                      '(((0 0) (1))
                        ((0 1) (1))
                        ((1 0) (1))
                        ((1 1) (0)))
                      2)
   )
  
  (test-case
   "nor-gate"
   (test-input-output (new nor-gate%)
                      '(A-in B-in) '(out)
                      '(((0 0) (1))
                        ((0 1) (0))
                        ((1 0) (0))
                        ((1 1) (0)))
                      2)
   )
  
  (test-case
   "xor-gate"
   (test-input-output (new xor-gate%)
                      '(A-in B-in) '(out)
                      '(((0 0) (0))
                        ((0 1) (1))
                        ((1 0) (1))
                        ((1 1) (0)))
                      6)
   )
  
  (test-case
   "half-adder"
   (test-input-output (new half-adder%)
                      '(A-in B-in) '(Carry-out Sum-out)
                      '(((0 0) (0 0))
                        ((0 1) (0 1))
                        ((1 0) (0 1))
                        ((1 1) (1 0)))
                      8)
   )
  
  (test-case
   "full-adder"
   (test-input-output (new full-adder%)
                      '(Carry-in A-in B-in) '(Carry-out Sum-out)
                      '(((0 0 0) (0 0))
                        ((0 0 1) (0 1))
                        ((0 1 0) (0 1))
                        ((0 1 1) (1 0))
                        ((1 0 0) (0 1))
                        ((1 0 1) (1 0))
                        ((1 1 0) (1 0))
                        ((1 1 1) (1 1)))
                      18)
   )
  
  ;    (test-basecase-old '8-bit-adder (new 8-bit-adder%)
  ;                   '(((0 (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0)) (0 (0 0 0 0 0 0 0 0)))
  ;                     ((0 (0 0 0 0 1 0 0 1) (0 0 0 0 1 0 0 1)) (0 (0 0 0 1 0 0 1 0)))
  ;                     ((1 (0 1 1 0 1 0 0 1) (0 1 0 1 1 0 1 1)) (0 (1 1 0 0 0 1 0 1)))
  ;                     ((1 (1 1 1 1 1 1 1 1) (1 1 1 1 1 1 1 1)) (1 (1 1 1 1 1 1 1 1)))
  ;                     ))
  ;    
  ;    (test-basecase-old 'R-S-flip-flop (new R-S-flip-flop%)
  ;                   '(((0 0) (1 0))
  ;                     ((0 1) (1 0))
  ;                     ((1 0) (0 1))
  ;                     ((0 0) (0 1)) ; different!
  ;                     ((0 1) (1 0))
  ;                     ))
  ;    
  ;    (test-basecase-old 'level-triggered-D-type-latch (new level-triggered-D-type-latch%)
  ;                   '(((0 1) (0 1))
  ;                     ((0 0) (0 1))
  ;                     ((1 0) (0 1))
  ;                     ((1 1) (1 0))
  ;                     ((0 0) (1 0)) ; different!
  ;                     ((1 0) (1 0)) ; different!
  ;                     ))
  
  
)