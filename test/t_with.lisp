;; 1. Define the scoped memory arena macro
(defmacro (with expr)
  (cons (quote let)
        (cons (cons (cons (quote oldh) (cons (cons (quote getheap) 0) 0))
                    (cons (cons (quote res) (cons expr 0)) 0))
              (cons (cons (quote setheap) (cons (quote oldh) 0))
                    (cons (quote res) 0)))))

;; 2. Test it!
(let ((h1 (getheap))
      
      ;; Execute a block inside the arena that allocates 100 blocks of memory
      (res (with (let ((ptr (alloc 100))) 
                   42))) 
                   
      (h2 (getheap)))
      
  ;; Assert that the inner block successfully returned its value
  (asserteq "WTH_VAL" 42 res)
  
  ;; Assert that the heap pointer was perfectly restored, freeing the 100 blocks!
  (asserteq "WTH_MEM" h1 h2)

  ;; Exit with 0 (Success)
  0)
