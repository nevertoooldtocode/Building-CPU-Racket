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
    
    (define/public (initialize-input! interface-list value-list)
      (for-each
       (lambda (interface value) (send this set-input! interface value))
       interface-list value-list))
    
    (define/public (initialize-output! interface-list value-list)
      (for-each
       (lambda (interface value) (send this set-output! interface value))
       interface-list value-list))
    
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

(define relay%
  (class basic-element%
    (super-new)    
    
    (send this initialize-input! '(control in) '(0 1))
    (send this initialize-output! '(Q Q-bar) '(0 1))
    (send this set-transistor-count! 1)
    
    (define/override (process)
      (cond ((= 0 (send this get-input 'in))
             (send this set-output! 'Q 0)
             (send this set-output! 'Q-bar 0))
            ((= 1 (send this get-input 'in))
             (cond ((= 0 (send this get-input 'control))
                    (send this set-output! 'Q 0)
                    (send this set-output! 'Q-bar 1))
                   ((= 1 (send this get-input 'control))
                    (send this set-output! 'Q 1)
                    (send this set-output! 'Q-bar 0))
                   (else (eprintf "incorrect input value --- relay%"))))
            (else (eprintf "incorrect input value --- relay%"))))
    ))

(define inverter%
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define relay (make-object relay%))
    
    (send this initialize-input! '(in) '(0))
    (send this initialize-output! '(out) '(1))
    (send this set-transistor-count! (send relay get-transistor-count))
    
    (define/override (set-input! interface value)
      (hash-set! input-table interface value)
      (send relay set-input! 'control value)) 
    
    (define/override (process)
      (send relay process)
      (send this set-output! 'out (send relay get-output 'Q-bar)))
    ))

(define and-gate%
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define relay1 (make-object relay%))
    (define relay2 (make-object relay%))
    (define connector (make-object connector% relay1 'Q relay2 'in))
    
    (send this initialize-input! '(A-in B-in) '(0 0))
    (send this initialize-output! '(out) '(0))
    (send this set-transistor-count! (+ (send relay1 get-transistor-count)
                                        (send relay2 get-transistor-count)))
    
    (define/override (set-input! interface value)
      (hash-set! input-table interface value)
      (cond ((eq? interface 'A-in)
             (send relay1 set-input! 'control value))
            ((eq? interface 'B-in)
             (send relay2 set-input! 'control value))
            (else (eprintf "Unknown Interface Type")))) 
    
    (define/override (process)
      (send relay1 process)
      (send connector process)
      (send relay2 process)
      (send this set-output! 'out (send relay2 get-output 'Q)))    
    ))

(define or-gate%
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define relay1 (make-object relay%))
    (define relay2 (make-object relay%))
    
    (send this initialize-input! '(A-in B-in) '(0 0))
    (send this initialize-output! '(out) '(0))
    (send this set-transistor-count! (+ (get-field transistor-count relay1)
                                        (get-field transistor-count relay2)))
    
    (define/override (set-input! interface value)
      (hash-set! input-table interface value)
      (cond ((eq? interface 'A-in)
             (send relay1 set-input! 'control value))
            ((eq? interface 'B-in)
             (send relay2 set-input! 'control value))
            (else (eprintf "set!-interface --- unknown interface name ~a\n" interface))))
    
    (define/override (process)
      (send relay1 process)
      (send relay2 process)
      (send this set-output! 'out
            (max (send relay1 get-output 'Q)
                 (send relay2 get-output 'Q))))
    ))

(define nand-gate%
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define relay1 (make-object relay%))
    (define relay2 (make-object relay%))
    
    (send this initialize-input! '(A-in B-in) '(0 0))
    (send this initialize-output! '(out) '(1))
    (send this set-transistor-count! (+ (get-field transistor-count relay1)
                                        (get-field transistor-count relay2)))
    
    (define/override (set-input! interface value)
      (hash-set! input-table interface value)
      (cond ((eq? interface 'A-in)
             (send relay1 set-input! 'control value))
            ((eq? interface 'B-in)
             (send relay2 set-input! 'control value))
            (else (eprintf "set!-interface --- unknown interface name ~a\n" interface))))
    
    (define/override (process)
      (send relay1 process)
      (send relay2 process)
      (send this set-output! 'out
            (max (send relay1 get-output 'Q-bar)
                 (send relay2 get-output 'Q-bar))))
    ))

(define nor-gate%
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define relay1 (make-object relay%))
    (define relay2 (make-object relay%))
    (define connector (make-object connector% relay1 'Q-bar relay2 'in))
    
    (send this initialize-input! '(A-in B-in) '(0 0))
    (send this initialize-output! '(out) '(1))
    (send this set-transistor-count! (+ (send relay1 get-transistor-count)
                                        (send relay2 get-transistor-count)))
    
    (define/override (set-input! interface value)
      (hash-set! input-table interface value)
      (cond ((eq? interface 'A-in)
             (send relay1 set-input! 'control value))
            ((eq? interface 'B-in)
             (send relay2 set-input! 'control value))
            (else (eprintf "Unknown Interface Type")))) 
    
    (define/override (process)
      (send relay1 process)
      (send connector process)
      (send relay2 process)
      (send this set-output! 'out (send relay2 get-output 'Q-bar))) 
    ))

(define xor-gate%
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define or-gate (new or-gate%))
    (define nand-gate (new nand-gate%))
    (define and-gate (new and-gate%))
    (define connector-1 (make-object connector% or-gate 'out and-gate 'A-in))
    (define connector-2 (make-object connector% nand-gate 'out and-gate 'B-in))
    
    (send this initialize-input! '(A-in B-in) '(0 0))
    (send this initialize-output! '(out) '(1))
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
    
    (send this initialize-input! '(A-in B-in) '(0 0))
    (send this initialize-output! '(Sum-out Carry-out) '(0 0))
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
    (define connector-1 (make-object connector% half-adder-1 'Carry-out or-gate 'B-in))
    (define connector-2 (make-object connector% half-adder-1 'Sum-out half-adder-2 'B-in))
    (define connector-3 (make-object connector% half-adder-2 'Carry-out or-gate 'A-in))
    
    (send this initialize-input! '(Carry-in A-in B-in) '(0 0 0))
    (send this initialize-output! '(Sum-out Carry-out) '(0 0))
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
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define BANKSIZE 8)
    
    (define full-adder-bank ; 8 adders, numbered 0-7. 7 has the least significant bit
      (for/list ([i BANKSIZE])
        (new full-adder%)))
    
    (define connector-list ; 7 connectors
      (for/list ([i (sub1 BANKSIZE)])
        (make-object connector%
          (list-ref full-adder-bank (add1 i)) 'Carry-out
          (list-ref full-adder-bank i) 'Carry-in)))
    
    (send this initialize-input! '(Carry-in A-in B-in) '(0 (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0)))
    (send this initialize-output! '(Sum-out Carry-out) '((0 0 0 0 0 0 0 0) 0))
    (send this set-transistor-count!
          (foldl
           +
           0
           (map (lambda (adder)
                  (get-field transistor-count adder))
                full-adder-bank)))         
    
    (define/override (set-input! interface value)
      (hash-set! input-table interface value)
      (cond ((eq? interface 'Carry-in)
             (send (list-ref full-adder-bank (sub1 BANKSIZE)) set-input! 'Carry-in value))
            ((eq? interface 'A-in)
             (for ([i BANKSIZE])
               (send (list-ref full-adder-bank i) set-input! 'A-in (list-ref value i))))
            ((eq? interface 'B-in)
             (for ([i BANKSIZE])
               (send (list-ref full-adder-bank i) set-input! 'B-in (list-ref value i))))
            (else (eprintf "set!-interface --- unknown interface name ~a\n" interface))))
    
    (define/override (process)
      (for ([i (reverse (range 1 BANKSIZE))])
        (send (list-ref full-adder-bank i) process)
        (send (list-ref connector-list (sub1 i)) process)
        )
      (send (list-ref full-adder-bank 0) process)
      (send this set-output! 'Sum-out
            (for/list ([i BANKSIZE])
              (send (list-ref full-adder-bank i) get-output 'Sum-out)))
      (send this set-output! 'Carry-out (send (list-ref full-adder-bank 0)
                                              get-output 'Carry-out)))        
    ))

(define R-S-flip-flop%
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define nor-gate-1 (new nor-gate%))
    (define nor-gate-2 (new nor-gate%))
    (define connector-1 (make-object connector% nor-gate-1 'out nor-gate-2 'A-in))
    (define connector-2 (make-object connector% nor-gate-2 'out nor-gate-1 'B-in))
    
    (send this initialize-input! '(R-in S-in) '(0 0))
    (send this initialize-output! '(Q Q-bar) '(0 1))
    (send this set-transistor-count! (+ (get-field transistor-count nor-gate-1)
                                        (get-field transistor-count nor-gate-2)))
    
    (define/override (set-input! interface value)
      (hash-set! input-table interface value)
      (cond ((eq? interface 'R-in)
             (send nor-gate-1 set-input! 'A-in value))
            ((eq? interface 'S-in)
             (send nor-gate-2 set-input! 'B-in value))
            (else (eprintf "set!-interface --- unknown interface name ~a\n" interface))))
    
    (define/override (process)
      (send nor-gate-1 process) (send connector-1 process)
      (send nor-gate-2 process) (send connector-2 process)
      (send nor-gate-1 process) (send connector-1 process)
      (send this set-output! 'Q (send nor-gate-1 get-output 'out))
      (send this set-output! 'Q-bar (send nor-gate-2 get-output 'out)))
    ))

(define level-triggered-D-type-latch%
  (class basic-element%
    (super-new)
    
    (inherit-field input-table)
    
    (define and-gate-1 (new and-gate%))
    (define and-gate-2 (new and-gate%))
    (define inverter (new inverter%))
    (define R-S-flip-flop (new R-S-flip-flop%))
    (define connector-1 (make-object connector% inverter 'out and-gate-1 'A-in))
    (define connector-2 (make-object connector% and-gate-1 'out R-S-flip-flop 'R-in))
    (define connector-3 (make-object connector% and-gate-2 'out R-S-flip-flop 'S-in))
    
    (send this initialize-input! '(Data-in Clock-in) '(0 0))
    (send this initialize-output! '(Q Q-bar) '(0 1))
    (send this set-transistor-count! (+ (get-field transistor-count and-gate-1)
                                        (get-field transistor-count and-gate-2)
                                        (get-field transistor-count inverter)
                                        (get-field transistor-count R-S-flip-flop)))
    
    (define/override (set-input! interface value)
      (hash-set! input-table interface value)
      (cond ((eq? interface 'Data-in)
             (send inverter set-input! 'in value)
             (send and-gate-2 set-input! 'B-in value))
            ((eq? interface 'Clock-in)
             (send and-gate-1 set-input! 'B-in value)
             (send and-gate-2 set-input! 'A-in value))
            (else (eprintf "set!-interface --- unknown interface name ~a\n" interface))))
    
    (define/override (process)
      (send inverter process) (send connector-1 process)
      (send and-gate-1 process) (send connector-2 process)
      (send and-gate-2 process) (send connector-3 process)
      (send R-S-flip-flop process) 
      (send this set-output! 'Q (send R-S-flip-flop get-output 'Q))
      (send this set-output! 'Q-bar (send R-S-flip-flop get-output 'Q-bar)))    
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
   "relay"  
   (test-input-output (new relay%) 
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
  
  (test-case
   "8-bit adder"
   (test-input-output (new 8-bit-adder%)
                      '(Carry-in A-in B-in) '(Carry-out Sum-out)
                      '(((0 (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0)) (0 (0 0 0 0 0 0 0 0)))
                        ((0 (0 0 0 0 1 0 0 1) (0 0 0 0 1 0 0 1)) (0 (0 0 0 1 0 0 1 0)))
                        ((1 (0 1 1 0 1 0 0 1) (0 1 0 1 1 0 1 1)) (0 (1 1 0 0 0 1 0 1)))
                        ((1 (1 1 1 1 1 1 1 1) (1 1 1 1 1 1 1 1)) (1 (1 1 1 1 1 1 1 1))))
                      144)
   )
  
  (test-case
   "R-S-flip-flop"
   (test-input-output (new R-S-flip-flop%)
                      '(R-in S-in) '(Q Q-bar)
                      '(((0 0) (1 0))
                        ((0 1) (1 0))
                        ((1 0) (0 1))
                        ((0 0) (0 1)) ; different!
                        ((0 1) (1 0)))
                      4)
   )
  
  (test-case
   "level-triggered-D-type-latch"
   (test-input-output (new level-triggered-D-type-latch%)
                      '(Data-in Clock-in) '(Q Q-bar)
                      '(((0 1) (0 1))
                        ((0 0) (0 1))
                        ((1 0) (0 1))
                        ((1 1) (1 0))
                        ((0 0) (1 0)) ; different!
                        ((1 0) (1 0))) ; different!
                      9)
   )
  
  )