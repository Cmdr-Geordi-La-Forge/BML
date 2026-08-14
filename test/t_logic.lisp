(let ((assert (lambda (name act)
        (putstr "TEST ") (putstr name) (putstr ": ")
        (cond
          (act (putline "PASS"))
          (1 (putline "FAIL"))))))
          
  (assert "EQL" (eql 42 42))
  (assert "LT"  (lt 5 10))
  (assert "GT"  (gt 10 5))
  (assert "LE"  (le 10 10))
  (assert "GE"  (ge 10 10))

  0)
