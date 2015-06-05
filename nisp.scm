; NES Instruction Set Processor (NISP)
; Authors: Mike
;
; Usage: Define romfile to an INES 1.0 rom file and run nisp.scm in mit-scheme
;
; Sources:
;     http://wiki.nesdev.com/w/index.php/INES
;     http://fms.komkon.org/EMUL8/NES.html#LABM
;     http://www.6502.org/tutorials/6502opcodes.html
;

(define romfile "smb.nes") ; Super Mario Bros. (Japan, USA).nes
(define prgrom-size -1)
(define chrrom-size -1)

(define check-magic-header
  (lambda (char num)
    (cond ((and (= num 0) (eq? char (integer->char #x4e))) #t) ; N
	  ((and (= num 1) (eq? char (integer->char #x45))) #t) ; E
	  ((and (= num 2) (eq? char (integer->char #x53))) #t) ; S
	  ((and (= num 3) (eq? char (integer->char #x1a))) #t) ; MS-DOS EOF
	  (else #f))))

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

(define run-prg-rom
  (lambda (rom-port)
    (consume-instructions rom-port 0)
    ))

(define eq-ch?
  (lambda (ch hex)
    (eq? ch (integer->char hex))))

(define display1
  (lambda (rom-port)
    (display (char->integer (read-char rom-port)))
    (newline)
    ))

(define display2
  (lambda (rom-port)
    (display (char->integer (read-char rom-port)))
    (display " ")
    (display (char->integer (read-char rom-port)))
    (newline)
    ))

(define consume-instructions
  (lambda (rom-port cntr)
    (begin
      (define inst (read-char rom-port))
      (cond 
					; Flag (Processor Status) Instructions
       ((eq-ch? inst #x78) (begin (display "SEI Set Interrupt\n")))
       ((eq-ch? inst #xd8) (begin (display "CLD Clear Decimal\n")))
       ((eq-ch? inst #x9a) (begin (display "Transfer X to Stack ptr\n")))
       
					; Branch Instructions
       ((eq-ch? inst #x10) (begin (display "Branch on Plus ") (display1 rom-port)))
       ((eq-ch? inst #xb0) (begin (display "Branch on Carry Set ") (display1 rom-port)))
       ((eq-ch? inst #xd0) (begin (display "Branch on Not Equal ") (display1 rom-port)))
       
					; LDA (LoaD Accumulator
       ((eq-ch? inst #xa9) (begin (display "Load Accumulator Immediate ") (display1 rom-port)))
       ((eq-ch? inst #xa5) (begin (display "Load Accumulator Zero Page ") (display1 rom-port)))
       ((eq-ch? inst #xb5) (begin (display "Load Accumulator Zero Page,X ") (display1 rom-port)))
       ((eq-ch? inst #xad) (begin (display "Load Accumulator Absolute ") (display2 rom-port)))
       ((eq-ch? inst #xbd) (begin (display "Load Accumulator Absolute,X ") (display2 rom-port)))
       
					; STA (STore Accumulator)
       ((eq-ch? inst #x8d) (begin (display "Store Accumulator Absolute ") (display2 rom-port)))
       
					; LDX (LoaD X register)
       ((eq-ch? inst #xa2) (begin (display "Load X register Immediate ") (display1 rom-port)))
       
					; LDY (LoaD Y register)
       ((eq-ch? inst #xa0) (begin (display "Load Y register Immediate ") (display1 rom-port)))
       
					; CMP (CoMPare accumulator)
       ((eq-ch? inst #xc9) (begin (display "Compare accumulator ") (display1 rom-port)))
       
					; CPX (ComPare X register)
       ((eq-ch? inst #xec) (begin (display "Compare X register Absolute ") (display2 rom-port)))
       
					; JMP (JuMP)
       ((eq-ch? inst #x4c) (begin (display "Jump Absolute ") (display2 rom-port)))
       
					; ORA (bitwise OR with Accumulator)
       ((eq-ch? inst #x09) (begin (display "Bitwise OR with accumulator Immediate ") (display1 rom-port)))
       ((eq-ch? inst #x01) (begin (display "Bitwise OR with accumulator Indirect,X ") (display1 rom-port)))
       
					; EOR (bitwise Exclusive OR)
       ((eq-ch? inst #x41) (begin (display "Bitwise exclusive OR Indirect,X ") (display1 rom-port)))
       
					; INC (INCrement memory)
       ((eq-ch? inst #xee) (begin (display "Increment memory Absolute ") (display2 rom-port)))
       
					; Register Instructions
       ((eq-ch? inst #xca) (begin (display "Decrement X\n")))
       ((eq-ch? inst #xc8) (begin (display "Increment Y\n")))
       
					; Stack Instructions
       ((eq-ch? inst #x68) (begin (display "Pull accumulator\n")))
       
					; JSR (Jump to SubRoutine)
       ((eq-ch? inst #x20) (begin (display "Jump to SubRoutine Absolute ") (display2 rom-port)))
       
       (else (display (string "Unknown instruction: " (char->integer inst) "\n")))
       )
      (if (< cntr 100)
	  (consume-instructions rom-port (+ cntr 1))
	  (#t))
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
	  (if (parse-header rom-port) (run-prg-rom rom-port) (display "Parse header failed."))
	  (display "ROM check failed magic.\n")
	  )
      )
    ))

(run)