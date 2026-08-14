(let ((asserteq (lambda (name exp act)
        (putstr "TEST ") (putstr name) (putstr ": ")
        (cond
          ((eql exp act) (putline "PASS"))
          (1 (putstr "FAIL (Exp ") (putint exp) 
             (putstr ", Got ") (putint act) (putline ")"))))))
             
  (let ((pair (cons 100 200)))
    (asserteq "CAR" 100 (car pair))
    (asserteq "CDR" 200 (cdr pair)))

  0)
