(let ((asserteq (lambda (name exp act)
        (putstr "TEST ") (putstr name) (putstr ": ")
        (cond
          ((eql exp act) (putline "PASS"))
          (1 (putstr "FAIL (Exp ") (putint exp) 
             (putstr ", Got ") (putint act) (putline ")")))))
             
      (sumto (lambda (n acc)
        (cond ((eql n 0) acc)
              (1 (sumto (sub n 1) (add acc n)))))))
              
  ;; Summing to 100 tests the TCE logic by ensuring we don't blow out the stack
  (asserteq "TCE" 5050 (sumto 100 0))

  0)
