(let (
  (sfcar (lambda (l) (if (eql l 0) 0 (car l))))
  (sfcdr (lambda (l) (if (eql l 0) 0 (cdr l))))
  
  (isspace (lambda (c) (or (eql c 32) (eql c 10) (eql c 13) (eql c 9))))
  (isdigit (lambda (c) (and (ge c 48) (le c 57))))
  (isdelim (lambda (c) (or (isspace c) (eql c 0) (eql c 40) (eql c 41) (eql c 39))))
  
  (putstr  (lambda (s) (syscall 1 1 (car s) (cdr s))))
  (putline (lambda (s) (putstr s) (putchar 10)))
  (eputstr  (lambda (s) (syscall 1 2 (car s) (cdr s))))
  (eputline (lambda (s) (eputstr s) (syscall 1 2 (car "\n") 1)))

  (eputsymc (lambda (symint count)
    (cond ((or (eql symint 0) (ge count 8)) 0)
          (1 (let ((ptr (getheap)))
               (pokebyte ptr (logand symint 255))
               (syscall 1 2 ptr 1)
               (eputsymc (ash symint -8) (add count 1)))))))
  (eputsym (lambda (symint) (eputsymc symint 0)))
  
  (cpybytes (lambda (src dst len)
    (cond ((gt len 0)
           (pokebyte dst (logand (peek src) 255))
           (cpybytes (add src 1) (add dst 1) (sub len 1)))
          (1 0))))
          
  (padzeros (lambda (ptr count target)
    (cond ((lt count target)
           (pokebyte (add ptr count) 0)
           (padzeros ptr (add count 1) target))
          (1 0))))
          
  (catstr (lambda (s1 s2)
    (let ((len1     (cdr s1))
          (len2     (cdr s2))
          (totallen (add len1 len2))
          (aligned  (mul (div (add totallen 8) 8) 8))
          (ptr      (getheap)))
      (alloc (div aligned 8))
      (cpybytes (car s1) ptr len1)
      (cpybytes (car s2) (add ptr len1) len2)
      (cons ptr totallen))))
      
  (puttmpl (lambda (ptr len arg)
    (cond
      ((le len 0) 0)
      (1 (let ((c (logand (peek ptr) 255)))
           (cond
             ((and (eql c 37) (gt len 1))
              (let ((nextc (logand (peek (add ptr 1)) 255)))
                (cond
                  ((eql nextc 49)
                   (putint arg)
                   (puttmpl (add ptr 2) (sub len 2) arg))
                  (1 (putchar c) (puttmpl (add ptr 1) (sub len 1) arg)))))
             (1 (putchar c) (puttmpl (add ptr 1) (sub len 1) arg))))))))
             
  (symlenc (lambda (symint count)
    (cond ((or (eql symint 0) (ge count 8)) count)
          (1 (symlenc (ash symint -8) (add count 1))))))
  (symlen (lambda (symint) (symlenc symint 0)))
  
  (sym2str (lambda (symint)
    (let ((ptr (getheap)))
      (poke ptr symint)
      (alloc 1)
      (cons ptr (symlen symint)))))
      
  (str2symc (lambda (ptr len shift acc)
    (cond ((or (le len 0) (ge shift 64)) acc)
          (1 (str2symc (add ptr 1) (sub len 1) (add shift 8)
                       (logior acc (ash (logand (peek ptr) 255) shift)))))))
                       
  (symeq (lambda (symint str)
    (eql symint (str2symc (car str) (cdr str) 0 0))))
    
  (putsymc (lambda (symint count)
    (cond ((or (eql symint 0) (ge count 8)) 0)
          (1 (putchar (logand symint 255))
             (putsymc (ash symint -8) (add count 1))))))
  (putsym (lambda (symint) (putsymc symint 0)))
  
  (getbyte (lambda (chunk charidx)
    (logand (ash chunk (mul charidx -8)) 255)))
    
  (streqbl (lambda (ptr1 ptr2 len idx)
    (cond 
      ((ge idx len) 1)
      (1 (let ((chunkidx (div idx 8))
               (charidx (sub idx (mul chunkidx 8)))
               (chunk1 (peek (add ptr1 (mul chunkidx 8))))
               (chunk2 (peek (add ptr2 (mul chunkidx 8))))
               (c1 (getbyte chunk1 charidx))
               (c2 (getbyte chunk2 charidx)))
           (cond 
             ((eql c1 c2) (streqbl ptr1 ptr2 len (add idx 1)))
             (1 0)))))))
             
  (streq (lambda (s1 s2)
    (cond ((and (eql s1 0) (eql s2 0)) 1)
          ((or (eql s1 0) (eql s2 0)) 0)
          ((eql (cdr s1) (cdr s2)) 
           (streqbl (car s1) (car s2) (cdr s1) 0))
          (1 0))))
          
  (nulterm (lambda (s)
    (let ((len (cdr s))
          (ptr (getheap)))
      (alloc (add (div len 8) 1))
      (cpybytes (car s) ptr len)
      (pokebyte (add ptr len) 0)
      ptr)))

  
  (lksym (lambda (str table)
    (cond ((eql table 0) 0)
          (1 (let ((entry (sfcar table)))
               (cond ((streq str (car entry)) (cdr entry))
                     (1 (lksym str (cdr table)))))))))

  (lksymi (lambda (symint table)
    (cond ((eql table 0) 0)
          (1 (let ((entry (sfcar table)))
               (cond ((eql symint (car entry)) (cdr entry))
                     (1 (lksymi symint (cdr table)))))))))

  (reverse (lambda (lst acc)
    (cond ((eql lst 0) acc)
          (1 (reverse (cdr lst) (cons (car lst) acc))))))
      
  (exit (lambda (code) (syscall 60 code)))
  
  (asserteq (lambda (name exp act)
    (putstr "TEST ") (putstr name) (putstr ": ")
    (cond
      ((eql exp act) (putline "PASS"))
      (1 (putstr "FAIL (Exp ") (putint exp) 
         (putstr ", Got ") (putint act) (putline ")")
         (exit 1)))))
         
  (assert (lambda (name act)
    (putstr "TEST ") (putstr name) (putstr ": ")
    (cond
      (act (putline "PASS"))
      (1 (putline "FAIL") 
         (exit 1)))))

  (with (macro (expr)
    (cons (quote let)
          (cons (cons (cons (quote oldh) (cons (cons (quote getheap) 0) 0))
                      (cons (cons (quote res) (cons expr 0)) 0))
                (cons (cons (quote setheap) (cons (quote oldh) 0))
                      (cons (quote res) 0)))))))
  
  ;; End with '1' so the let-block successfully evaluates and populates globals
  1)
