(add-load-path "../lib/" :relative)

(use gauche.test)
(test-start "srfi-1")
(use K-Compiler)
(use Util)
(use Read)
(use Check)

(define (load-file fname)
  (call-with-input-file fname
    (fn [file-port]
        (let* ([exprs*types (read-s-exprs (read-list file-port))]
               [checks? (type-check exprs*types)])
          exprs*types))))

(define (read-list port)
  (let ((exp (read port)))
    (if (eof-object? exp) '()
        (cons exp (read-list port)))))

(define *exprs*types*
  (fold (fn [fname exprs*types]
            (let1 res (load-file fname)
                  (hash-table-union! (car exprs*types) (car res))
                  (hash-table-union! (cdr exprs*types) (cdr res))
                  exprs*types))
        (cons (make-hash-table 'eq?) (make-hash-table 'eq?))
        '("examples/prelude.nadeko" "examples/srfi-1.nadeko")))

(define (run code)
  (let* ([res   (read-s-exprs `((show ,code)))]
         [_ (hash-table-union! (car *exprs*types*) (car res))]
         [_ (hash-table-union! (cdr *exprs*types*) (cdr res))]
         [c (type-check *exprs*types*)])
    (unless c (raise-error/message "type error"))
    (Krivine (compile (car *exprs*types*)))))

(test-section "constructors")
(test* "cons" "1 : 2 : 3 : []" (run `(cons 1 (cons 2 (cons 3 nil)))))
(test* "make-integers-from" "2 : 3 : 4 : []" (run `(take (make-integers-from 2) 3)))
(test* "integers" "0 : 1 : 2 : 3 : []" (run `(take integers 4)))

(test-section "selectors")
(test* "car" "0" (run `(car integers)))
(test* "cdr" "1 : 2 : 3 : []" (run `(cdr (take integers 4))))
(test* "list-ref" "2" (run `(list-ref integers 2)))
(test* "take" "0 : 1 : 2 : 3 : []" (run `(take integers 4)))
(test* "drop" "2 : 3 : []" (run `(take (drop integers 2) 2)))
(test* "take-right" "2 : 3 : 4 : []" (run `(take-right (take integers 5) 3)))
(test* "drop-right" "0 : 1 : []" (run `(drop-right (take integers 5) 3)))
(test* "split-at" "0 : 1 : 2 : 3 : 4 : []" (run `(fst (split-at (take integers 10) 5))))
(test* "split-at" "5 : 6 : 7 : 8 : 9 : []" (run `(snd (split-at (take integers 10) 5))))
(test* "last" "5" (run `(last (take integers 6))))

(test-section "miscellaneous")
(test* "length" "0" (run `(length nil)))
(test* "length" "10" (run `(length (take integers 10))))
(test* "append" "0 : 1 : 2 : 0 : 1 : 2 : 3 : []" (run `(append (take integers 3) (take integers 4))))
#|(test* "concatenate" "0 : 1 : 2 : 0 : 1 : 2 : 3 : []"
         (run `(concatenate (cons (take integers 3) (cons (take integers 4) nil)))))|#
(test* "reverse" "5 : 4 : 3 : 2 : 1 : 0 : []" (run `(reverse (take integers 6))))
(test* "zip" "0 : 2 : 1 : 1 : 2 : 0 : []" (run `(zip (take integers 3) (reverse (take integers 3)))))
;;(test* "unzip" "0 : 1 : 2 : []" (run `(fst (unzip (zip (take integers 3) (reverse (take integers 3)))))))
;;(test* "unzip" "2 : 1 : 0 : []" (run `(snd (unzip (zip (take integers 3) (reverse (take integers 3)))))))
(test* "count1" "3" (run `(count1 (cons 1 (cons 2 (cons 1 (cons 3 (cons 1 nil))))) (=? 1))))
(test* "count2" "3"
       (run `(count2 (cons 1 (cons 2 (cons 1 (cons 3 (cons 1 nil)))))
                     (cons 1 (cons 2 (cons 1 (cons 3 (cons 1 nil))))) (^ x y (=? 2 (+ x y))))))

(test-section "fold : map")
(test* "fold" "55" (run `(fold (take integers 11) + 0)))
(test* "fold" "2 : 1 : 0 : []" (run `(fold (take integers 3) cons nil)))
(test* "fold-right" "55" (run `(fold-right (take integers 11) + 0)))
(test* "fold-right" "0 : 1 : 2 : []" (run `(fold-right (take integers 3) cons nil)))
#|(test* "unfold" "1 : 4 : 9 : 16 : 25 : 36 : 49 : 64 : 81 : 100 : []"
         (run `(unfold (< 10) (^ x (* x x)) (+ 1) 1)))|#
(test* "map" "0 : 2 : 4 : 6 : 8 : []" (run `(map (take integers 5) (* 2))))

(test-section "filtering : partitioning")
(test* "filter" "1 : 3 : 2 : []"
       (run `(filter (cons 1 (cons 9 (cons 3 (cons 12 (cons 2 nil))))) (> 5))))
;;(test* "partition" "1 : 3 : []"
;;       (run `(fst (partition (cons 1 (cons "2" (cons 3 (cons "4" nil)))) number?))))
;;(test* "partition" "2 : 4 : []"
;;       (run `(snd (partition (cons 1 (cons "2" (cons 3 (cons "4" nil)))) number?))))
(test* "remove" "5 : 6 : 7 : []"
       (run `(remove (take integers 8) (> 5))))

(test-section "searching")
(test* "find" "6" (run `(find (take integers 10) (< 5))))
(test* "find-tail" "6 : 7 : 8 : 9 : []" (run `(find-tail (take integers 10) (< 5))))
(test* "take-while" "0 : 1 : 2 : 3 : []" (run `(take-while (take integers 8) (> 4))))
(test* "drop-while" "4 : 5 : 6 : 7 : []" (run `(drop-while (take integers 8) (> 4))))
(test* "any?" "true" (run `(any? (take integers 5) (=? 3) "true" "false")))
(test* "any?" "false" (run `(any? (take integers 5) (=? 100) "true" "false")))
(test* "every?" "true" (run `(every? (take integers 5) (> 8) "true" "false")))
(test* "every?" "false" (run `(every? (take integers 5) (> 3) "true" "false")))
(test* "list-index" "7" (run `(list-index (take integers 10) (< 6))))
(test* "member" "3 : 4 : 5 : []" (run `(member (take integers 6) 3)))
(test* "delete" "2 : 3 : []" (run `(delete (cons 1 (cons 2 (cons 1 (cons 3 nil)))) 1)))
(test* "delete-duplicates" "1 : 2 : 3 : []"
       (run `(delete-duplicates (cons 1 (cons 2 (cons 1 (cons 3 (cons 2 nil))))))))

(test-section "association list")
(test* "assq" "nori"
       (run `(assq "nazuna" (cons (pair "yuno" "miyako") (cons (pair "nazuna" "nori") nil)))))
(test* "alist-cons" "miyako"
       (run `(assq "yunocchi" (alist-cons "yunocchi" "miyako" (cons (pair "yuno" "miyako") (cons (pair "nazuna" "nori") nil))))))
(test* "alist-copy" "a : 1 : b : 5 : []"
       (run `(alist-copy (cons (pair "a" 1) (cons (pair "b" 5) nil)))))
(test* "alist-delete" "b : 5 : []"
       (run `(alist-delete (cons (pair "a" 1) (cons (pair "b" 5) nil)) "a")))


(test-end :exit-on-failure #t)
