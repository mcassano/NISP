; NES Instruction Set Processor (NISP)
; Authors: Mike
;
; Usage: Define romfile to an INES 1.0 rom file and run nisp.scm in mit-scheme
;
; Sources:
;     http://wiki.nesdev.com/w/index.php/INES
;     http://fms.komkon.org/EMUL8/NES.html#LABM
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
	  (parse-header rom-port)
	  (display "ROM check failed magic.\n")
	  )
      )
    ))

(run)