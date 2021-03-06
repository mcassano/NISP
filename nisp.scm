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
;     http://homepage.ntlworld.com/cyborgsystems/CS_Main/6502/6502.htm
;     https://en.wikibooks.org/wiki/NES_Programming/Initializing_the_NES
;     http://nintendoage.com/forum/messageview.cfm?catid=22&threadid=7155
;

(define hexIndex (vector "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))

(define romfile "smb.nes") ; Super Mario Bros. (Japan, USA).nes
(define prgrom-size #f)
(define chrrom-size #f)

(define main-chrrom #f)

(define main-cpuram #f)
(define main-ppuram #f)

(define interrupt-reset #f)

(define flag-interrupt #f)
(define flag-decimal #f)

(define reg-accumulator #f)
(define reg-x #f)
(define reg-y #f)
(define reg-pc #f)
(define reg-sp #f)

(define check-magic-header
  (lambda (char num)
    (cond ((and (= num 0) (eq? char (integer->char #x4e))) #t) ; N
	  ((and (= num 1) (eq? char (integer->char #x45))) #t) ; E
	  ((and (= num 2) (eq? char (integer->char #x53))) #t) ; S
	  ((and (= num 3) (eq? char (integer->char #x1a))) #t) ; MS-DOS EOF
	  (else #f))))

(define init-cpuram
  (lambda (bytes)
    (set! main-cpuram (make-vector bytes 0))
    ))

(define init-ppuram
  (lambda (bytes)
    (set! main-ppuram (make-vector bytes 0))
    ))

(define init-prgrom
  (lambda (cnt size rom-port)
    (begin
      (if (< cnt size)
	  (begin
	    (vector-set! main-cpuram (+ cnt #x8000) (char->integer (read-char rom-port)))
	    (init-prgrom (+ cnt 1) size rom-port)
	    )
	  )
      )
    ))

(define init-chrrom
  (lambda (rom-port)
    (set! main-chrrom (make-initialized-vector chrrom-size (lambda (idx) (char->integer (read-char rom-port)))))
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
    (display "(")
    (display (vector-ref main-cpuram reg-pc))
    (display ") ")
    ))

(define display2
  (lambda ()
    (display "(")
    (display (vector-ref main-cpuram reg-pc))
    (display " ")
    (display (vector-ref main-cpuram (+ reg-pc 1)))
    (display ") ")
    ))

(define get-memory-address
  (lambda ()
    (begin
      (define lowByte (vector-ref main-cpuram (+ reg-pc 0)))
      (define highByte (arithmetic-shift (vector-ref main-cpuram (+ reg-pc 1)) 8))
      )
    (display (string "[$" (itoh (+ highByte lowByte)) "] "))
    (+ highByte lowByte)
    ))

(define itoh
  (lambda (int)
    (if (eq? int 0)
	""
	(begin
	  (string (itoh (arithmetic-shift int -4)) (vector-ref hexIndex (bitwise-and int #xF)))
	  )
	)
    ))
    
(define inc-sp
  (lambda (num)
    (set! reg-pc (+ reg-pc num))
    ))

(define consume-instructions
  (lambda (cntr)
    (begin
      (define inst (vector-ref main-cpuram reg-pc))
      (inc-sp 1)
      (cond
					; Flag (Processor Status) Instructions
       ((eq? inst #x78) (begin (display "SEI Set Interrupt (78)") (set! flag-interrupt #t)))
       ((eq? inst #xd8) (begin (display "CLD Clear Decimal (d8)") (set! flag-decimal #f)))
       ((eq? inst #x9a) (begin (display "Transfer X to Stack ptr (9a)") (set! reg-sp reg-x)))
       ((eq? inst #x18) (begin (display "*Clear Carry (18)")))
       ((eq? inst #x38) (begin (display "*Set Carry (38)")))
       
					; Branch Instructions
       ((eq? inst #x10) (begin (display "*Branch on Plus (10) ") (display1)(inc-sp 1)))
       ((eq? inst #xb0) (begin (display "*Branch on Carry Set (b0) ") (display1)(inc-sp 1)))
       ((eq? inst #xd0) (begin (display "*Branch on Not Equal (d0) ") (display1)(inc-sp 1)))
       ((eq? inst #xf0) (begin (display "*Branch on Equal (f0) ") (display1)(inc-sp 1)))
       
					; ROR (ROtate Right)
       ((eq? inst #x7e) (begin (display "*Rotate right Absolute,X (7e) ") (display2)(inc-sp 2)))

					; LDA (LoaD Accumulator)
       ((eq? inst #xa9) (begin (display "Load Accumulator Immediate (a9) ") (set! reg-accumulator (vector-ref main-cpuram reg-pc))(display1)(inc-sp 1)))
       ((eq? inst #xa5) (begin (display "!Load Accumulator Zero Page (a5) ") (display1)))
       ((eq? inst #xb5) (begin (display "!Load Accumulator Zero Page,X (b5) ") (display1)))
       ((eq? inst #xad) (begin (display "Load Accumulator Absolute (ad) ") (set! reg-accumulator (vector-ref main-cpuram (get-memory-address)))(display2)(inc-sp 2)))
       ((eq? inst #xbd) (begin (display "Load Accumulator Absolute,X (bd) ") (set! reg-accumulator (vector-ref main-cpuram (+ (get-memory-address) reg-x)))(display2)(inc-sp 2)))
    
					; STA (STore Accumulator)
       ((eq? inst #x8d) (begin (display "Store accumulator Absolute (8d) ")  (vector-set! main-cpuram (get-memory-address) reg-accumulator)(inc-sp 2)))
       ((eq? inst #x85) (begin (display "!Store accumulator Zero Page (85) ") (display1)))
       ((eq? inst #x9d) (begin (display "!Store accumulator Absolute,X (9d) ") (display2)))
       
					; LDX (LoaD X register)
       ((eq? inst #xa2) (begin (display "Load X register Immediate (a2) ") (set! reg-x (vector-ref main-cpuram reg-pc)) (display1)(inc-sp 1)))
       ((eq? inst #xbe) (begin (display "!Load X register Absolute,Y (be) ") (display2)))
       ((eq? inst #xae) (begin (display "!Load X register Absolute (ae) ") (display2)))

					; RTS (ReTurn from Subroutine)
       ((eq? inst #x60) (begin (display "!Return from subroutine Implied (60)")))

					; LDY (LoaD Y register)
       ((eq? inst #xa0) (begin (display "Load Y register Immediate (a0) ") (set! reg-y (vector-ref main-cpuram reg-pc))(display1)(inc-sp 1)))
       ((eq? inst #xac) (begin (display "!Load Y register Absolute (ac) ") (display2)))
       
					; CMP (CoMPare accumulator)
       ((eq? inst #xc9) (begin (display "*Compare accumulator (c9) ") (display1)(inc-sp 1)))
       
					; CPX (ComPare X register)
       ((eq? inst #xec) (begin (display "!Compare X register Absolute (ec) ") (display2)))
       ((eq? inst #xe0) (begin (display "!Compare X register Immediate (e0) ") (display1)))
       
					; JMP (JuMP)
       ((eq? inst #x4c) (begin (display "!Jump Absolute (4c) ") (display2)))
       
					; AND (bitwise AND with accumulator)
       ((eq? inst #x29) (begin (display "!Bitwise AND with accumulator Immediate (29) ") (display1)))
       
					; ORA (bitwise OR with Accumulator)
       ((eq? inst #x09) (begin (display "!Bitwise OR with accumulator Immediate (09) ") (display1)))
       ((eq? inst #x01) (begin (display "!Bitwise OR with accumulator Indirect,X  (01) ") (display1)))
       
					; RTI (ReTurn from Interrupt)
       ((eq? inst #x40) (begin (display "!Return from interrupt Implied (40)")))

					; EOR (bitwise Exclusive OR)
       ((eq? inst #x41) (begin (display "!Bitwise exclusive OR Indirect,X (41) ") (display1)))
       ((eq? inst #x49) (begin (display "!Bitwise exclusive OR Immediate (49) ") (display1)))
       ((eq? inst #x45) (begin (display "!Bitwise exclusive OR Zero Page (45) ") (display1)))
       
					; INC (INCrement memory)
       ((eq? inst #xee) (begin (display "!Increment memory Absolute (ee) ") (display2)))
       ((eq? inst #xe6) (begin (display "!Increment memory Zero Page (e6) ") (display1)))
       
					; Register Instructions
       ((eq? inst #xca) (begin (display "Decrement X (ca)")(set! reg-x (- reg-x 1)))
       ((eq? inst #xc8) (begin (display "Increment Y (c8)")(set! reg-y (+ reg-y 1)))
       ((eq? inst #xa8) (begin (display "Transfer A to Y (a8)")(set! reg-y reg-a)))
       ((eq? inst #x88) (begin (display "Decrement Y (88)")(set! reg-y (- reg-y 1)))
       ((eq? inst #xe8) (begin (display "Increment X (e8)")(set! reg-x (+ reg-x 1)))

					; Stack Instructions
       ((eq? inst #x68) (begin (display "!Pull accumulator (68)")))
       ((eq? inst #x48) (begin (display "!Push accumulator (48)")))
       
					; JSR (Jump to SubRoutine)
       ((eq? inst #x20) (begin (display "*Jump to SubRoutine Absolute (20) ") (display2)(inc-sp 2)))

					; DEC (DECrement memory)
       ((eq? inst #xde) (begin (display "!Decrement memory Absolute,X (de) ") (display2)))
       ((eq? inst #xce) (begin (display "!Decrement memory Absolute (ce) ") (display2)))

					; STY (STore Y register)
       ((eq? inst #x8c) (begin (display "!Store Y register Absolute (8c) ") (display2)))
       ((eq? inst #x84) (begin (display "!Store Y register Zero Page (84) ") (display1)))
       
					; LSR (Logical Shift Right)
       ((eq? inst #x4a) (begin (display "!Logical shift right Accumulator (4a)")))

       (else (display (string "*** Unknown instruction: " inst)))
       )
      (newline)
      (if (< cntr 50)
	  (consume-instructions (+ cntr 1))
	  )
      )
    ))
  

(define run
  (lambda () 
    (begin
      (newline)
      (define rom-port (open-binary-input-file romfile))
      ; Check magic header for #\NES<EOF>
      (if (and (check-magic-header (read-char rom-port) 0) 
	       (check-magic-header (read-char rom-port) 1) 
	       (check-magic-header (read-char rom-port) 2) 
	       (check-magic-header (read-char rom-port) 3))
	  (if (parse-header rom-port)
	      (begin
                ; Init cpu memory to all zeros 64kB
		(init-cpuram 65536)
		; Init ppu memory to all zeros 16kB
		(init-ppuram 16384)
		; Copy prgrom to $8000 of cpu memory
		(init-prgrom 0 prgrom-size rom-port)
		(init-chrrom rom-port)
		(set! reg-pc #xFFFC)
		(set! interrupt-reset (get-memory-address))
		(display (string "Reset vector: " interrupt-reset))(newline)
		(set! reg-pc interrupt-reset)
		(run-program)
		)
	      (display "Parse header failed."))
	  (display "ROM check failed magic.\n")
	  )
      )
    ))

(run)