(let ((asserteq (lambda (name exp act)
        (putstr "TEST ") (putstr name) (putstr ": ")
        (cond
          ((eql exp act) (putline "PASS"))
          (1 (putstr "FAIL (Exp ") (putint exp) 
             (putstr ", Got ") (putint act) (putline ")"))))))
             
  (asserteq "IF_THN" 99 (if (eql 1 1) 99 0))
  (asserteq "IF_ELS" 0  (if (eql 1 0) 99 0))
  
  (asserteq "COND" 42
             (cond ((eql 0 1) 0)
                   ((eql 1 1) 42)
                   (1 0)))

  0)
