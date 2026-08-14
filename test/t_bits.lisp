(let ((asserteq (lambda (name exp act)
        (putstr "TEST ") (putstr name) (putstr ": ")
        (cond
          ((eql exp act) (putline "PASS"))
          (1 (putstr "FAIL (Exp ") (putint exp) 
             (putstr ", Got ") (putint act) (putline ")"))))))
             
  (asserteq "ASH_L" 16 (ash 4 2))
  (asserteq "ASH_R" 2  (ash 8 -2))
  (asserteq "AND"   0  (and 1 0))
  (asserteq "OR"    1  (or 1 0))

  0)
