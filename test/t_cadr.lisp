(let ((asserteq (lambda (name exp act)
        (putstr "TEST ") (putstr name) (putstr ": ")
        (cond
          ((eql exp act) (putline "PASS"))
          (1 (putstr "FAIL (Exp ") (putint exp) 
             (putstr ", Got ") (putint act) (putline ")"))))))
             
  (let ((lst (cons 1 (cons 2 (cons 3 0)))))
    (asserteq "CADR"  2 (cadr lst))
    (asserteq "CADDR" 3 (caddr lst)))

  0)
