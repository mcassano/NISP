; NES Instruction Set Processor (NISP)
; Authors: Mike
;
; Usage: Define romfile to an INES 1.0 rom file and run nisp.scm in mit-scheme
;
; Sources:
;     http://wiki.nesdev.com/w/index.php/INES
;     

(define romfile "smb.nes") ; Super Mario Bros. (Japan, USA).nes
 
(define check-magic-header
  (lambda (char num)
    (cond ((and (= num 0) (eq? char (integer->char #x4e))) #t) ; N
	  ((and (= num 1) (eq? char (integer->char #x45))) #t) ; E
	  ((and (= num 2) (eq? char (integer->char #x53))) #t) ; S
	  (else #f))))

(define run
  (lambda () 
    (begin
      (define rom-port (open-binary-input-file romfile))
      (if (and (check-magic-header (read-char rom-port) 0) (check-magic-header (read-char rom-port) 1) (check-magic-header (read-char rom-port) 2))
	  #t ; Header magic passed
	  #f ; Header magic didn't pass
	  )
      )
    ))

(run)