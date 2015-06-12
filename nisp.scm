; NES Instruction Set Processor (NISP)
; Authors: Mike
;
; Usage: Define romfile to an INES 1.0 rom file and run nisp.scm in mit-scheme
;
; Sources:
;     http://wiki.nesdev.com/w/index.php/INES
;     http://fms.komkon.org/EMUL8/NES.html#LABM
;     http://www.6502.org/tutorials/6502opcodes.html
;     http://www.thealmightyguru.com/Games/Hacking/Wiki/index.php?title=6502_Opcodes
;

(define romfile "smb.nes") ; Super Mario Bros. (Japan, USA).nes
(define prgrom-size -1)
(define chrrom-size -1)

(define stack-pointer 0)

(define main-program #f)
(define main-memory #f)

(define flag-interrupt #f)
(define flag-decimal #f)

(define register-accumulator #f)
(define register-x #f)

(define check-magic-header
  (lambda (char num)
    (cond ((and (= num 0) (eq? char (integer->char #x4e))) #t) ; N
	  ((and (= num 1) (eq? char (integer->char #x45))) #t) ; E
	  ((and (= num 2) (eq? char (integer->char #x53))) #t) ; S
	  ((and (= num 3) (eq? char (integer->char #x1a))) #t) ; MS-DOS EOF
	  (else #f))))

(define init-program
  (lambda (rom-port)
    (set! main-program (make-initialized-vector prgrom-size (lambda (idx) (char->integer (read-char rom-port)))))
    ))
			   
(define init-memory
  (lambda (bytes)
    (set! main-memory (make-vector bytes 0))
    ))

(define parse-header
  (lambda (rom-port)
    ; Assuming first 4 bytes of 16 have been consumed
    (begin
      (set! prgrom-size (* (char->integer (read-char rom-port)) 16384))
      (set! chrrom-size (* (char->integer (read-char rom-port)) 8192))
      (display (string "PRG ROM size " prgrom-size " bytes.\n")) 
      (display (string "CHR ROM size " chrrom-size " bytes.\n"))
      ; Unused header bytes 7-16
      (read-char rom-port)
      (read-char rom-port)
      (read-char rom-port)
      (read-char rom-port)
      (read-char rom-port)
      (read-char rom-port)
      (read-char rom-port)
      (read-char rom-port)
      (read-char rom-port)
      (read-char rom-port)
      )
    ))

(define run-program
  (lambda ()
    (consume-instructions 0)
    ))

(define eq-ch?
  (lambda (ch hex)
    (eq? ch (integer->char hex))))

(define display1
  (lambda ()
    (display (vector-ref main-program (+ stack-pointer 1)))
    (newline)
    ))

(define display2
  (lambda ()
    (display (vector-ref main-program (+ stack-pointer 1)))
    (display " ")
    (display (vector-ref main-program (+ stack-pointer 2)))
    (newline)
    ))

(define get-memory-address
  (lambda ()
    (begin
      (define highByte (arithmetic-shift (vector-ref main-program (+ stack-pointer 1)) 8))
      (define lowByte (vector-ref main-program (+ stack-pointer 2)))
      (newline)
      (display highByte)
      (newline)
      (display lowByte)
      (newline)
      (display (+ highByte lowByte))
      (newline)
      )
    (+ highByte lowByte)
    ))

(define inc-sp
  (lambda (num)
    (set! stack-pointer (+ stack-pointer num))
    ))

(define consume-instructions
  (lambda (cntr)
    (begin
      (define inst (vector-ref main-program stack-pointer))
      (cond
					; Flag (Processor Status) Instructions
       ((eq? inst #x78) (begin (display "SEI Set Interrupt\n") (set! flag-interrupt #t)(inc-sp 1)))
       ((eq? inst #xd8) (begin (display "CLD Clear Decimal\n") (set! flag-decimal #f)(inc-sp 1)))
       ((eq? inst #x9a) (begin (display "Transfer X to Stack ptr\n")(inc-sp 1)))
       ((eq? inst #x18) (begin (display "Clear Carry\n")(inc-sp 1)))
       ((eq? inst #x38) (begin (display "Set Carry\n")(inc-sp 1)))
       
					; Branch Instructions
       ((eq? inst #x10) (begin (display "Branch on Plus ") (display1)(inc-sp 2)))
       ((eq? inst #xb0) (begin (display "Branch on Carry Set ") (display1)(inc-sp 2)))
       ((eq? inst #xd0) (begin (display "Branch on Not Equal ") (display1)(inc-sp 2)))
       ((eq? inst #xf0) (begin (display "Branch on Equal ") (display1)(inc-sp 2)))
       
					; ROR (ROtate Right)
       ((eq? inst #x7e) (begin (display "Rotate right Absolute,X ") (display2)(inc-sp 3)))

					; LDA (LoaD Accumulator)
       ((eq? inst #xa9) (begin (display "Load Accumulator Immediate ") (set! register-accumulator (vector-ref main-program (+ stack-pointer 1))) (display register-accumulator)(newline)(inc-sp 2)))
       ((eq? inst #xa5) (begin (display "Load Accumulator Zero Page ") (display1)))
       ((eq? inst #xb5) (begin (display "Load Accumulator Zero Page,X ") (display1)))
       ((eq? inst #xad) (begin (display "Load Accumulator Absolute ") (display2)))
       ((eq? inst #xbd) (begin (display "Load Accumulator Absolute,X ") (display2)))
       
					; STA (STore Accumulator)
       ((eq? inst #x8d) (begin (display "Store accumulator Absolute ")  (vector-set! main-memory (get-memory-address) register-accumulator)(inc-sp 3)))
       ((eq? inst #x85) (begin (display "Store accumulator Zero Page ") (display1)))
       ((eq? inst #x9d) (begin (display "Store accumulator Absolute,X ") (display2)))
       
					; LDX (LoaD X register)
       ((eq? inst #xa2) (begin (display "Load X register Immediate ") (set! register-x (vector-ref main-program (+ stack-pointer 1))) (display1)(inc-sp 2)))
       ((eq? inst #xbe) (begin (display "Load X register Absolute,Y ") (display2)))
       ((eq? inst #xae) (begin (display "Load X register Absolute ") (display2)))

					; RTS (ReTurn from Subroutine)
       ((eq? inst #x60) (begin (display "Return from subroutine Implied\n")))

					; LDY (LoaD Y register)
       ((eq? inst #xa0) (begin (display "Load Y register Immediate ") (display1)))
       ((eq? inst #xac) (begin (display "Load Y register Absolute ") (display2)))
       
					; CMP (CoMPare accumulator)
       ((eq? inst #xc9) (begin (display "Compare accumulator ") (display1)))
       
					; CPX (ComPare X register)
       ((eq? inst #xec) (begin (display "Compare X register Absolute ") (display2)))
       ((eq? inst #xe0) (begin (display "Compare X register Immediate ") (display1)))
       
					; JMP (JuMP)
       ((eq? inst #x4c) (begin (display "Jump Absolute ") (display2)))
       
					; AND (bitwise AND with accumulator)
       ((eq? inst #x29) (begin (display "Bitwise AND with accumulator Immediate ") (display1)))
       
					; ORA (bitwise OR with Accumulator)
       ((eq? inst #x09) (begin (display "Bitwise OR with accumulator Immediate ") (display1)))
       ((eq? inst #x01) (begin (display "Bitwise OR with accumulator Indirect,X ") (display1)))
       
					; RTI (ReTurn from Interrupt)
       ((eq? inst #x40) (begin (display "Return from interrupt Implied\n")))

					; EOR (bitwise Exclusive OR)
       ((eq? inst #x41) (begin (display "Bitwise exclusive OR Indirect,X ") (display1)))
       ((eq? inst #x49) (begin (display "Bitwise exclusive OR Immediate ") (display1)))
       ((eq? inst #x45) (begin (display "Bitwise exclusive OR Zero Page ") (display1)))
       
					; INC (INCrement memory)
       ((eq? inst #xee) (begin (display "Increment memory Absolute ") (display2)))
       ((eq? inst #xe6) (begin (display "Increment memory Zero Page ") (display1)))
       
					; Register Instructions
       ((eq? inst #xca) (begin (display "Decrement X\n")))
       ((eq? inst #xc8) (begin (display "Increment Y\n")))
       ((eq? inst #xa8) (begin (display "Transfer A to Y\n")))
       ((eq? inst #x88) (begin (display "Decrement Y\n")))
       ((eq? inst #xe8) (begin (display "Increment X\n")))

					; Stack Instructions
       ((eq? inst #x68) (begin (display "Pull accumulator\n")))
       ((eq? inst #x48) (begin (display "Push accumulator\n")))
       
					; JSR (Jump to SubRoutine)
       ((eq? inst #x20) (begin (display "Jump to SubRoutine Absolute ") (display2)))

					; DEC (DECrement memory)
       ((eq? inst #xde) (begin (display "Decrement memory Absolute,X ") (display2)))
       ((eq? inst #xce) (begin (display "Decrement memory Absolute ") (display2)))

					; STY (STore Y register)
       ((eq? inst #x8c) (begin (display "Store Y register Absolute ") (display2)))
       ((eq? inst #x84) (begin (display "Store Y register Zero Page ") (display1)))
       
					; LSR (Logical Shift Right)
       ((eq? inst #x4a) (begin (display "Logical shift right Accumulator\n")))

       (else (display (string "*** Unknown instruction: " inst "\n")))
       )
      (if (< cntr 10)
	  (consume-instructions (+ cntr 1))
	  )
      )
    ))

(define run
  (lambda () 
    (begin
      (newline)
      (define rom-port (open-binary-input-file romfile))
      ; Init memory to all zeros 1kB
      (init-memory 1024)
      ; Check magic header for #\NES<EOF>
      (if (and (check-magic-header (read-char rom-port) 0) 
	       (check-magic-header (read-char rom-port) 1) 
	       (check-magic-header (read-char rom-port) 2) 
	       (check-magic-header (read-char rom-port) 3))
	  (if (parse-header rom-port)
	      (begin
		(init-program rom-port)
		(set! stack-pointer 0)
		(run-program)
		)
	      (display "Parse header failed."))
	  (display "ROM check failed magic.\n")
	  )
      )
    ))

(run)