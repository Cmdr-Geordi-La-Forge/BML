(let ((asserteq (lambda (name exp act)
        (putstr "TEST ") (putstr name) (putstr ": ")
        (cond
          ((eql exp act) (putline "PASS"))
          (1 (putstr "FAIL (Exp ") (putint exp) 
             (putstr ", Got ") (putint act) (putline ")"))))))
  
  (asserteq "ADD" 15 (add 10 5))
  (asserteq "SUB" 5  (sub 10 5))
  (asserteq "MUL" 50 (mul 10 5))
  (asserteq "DIV" 2  (div 10 5))

  0)
