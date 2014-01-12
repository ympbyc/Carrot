(add-load-path "../lib/" :relative)

(use gauche.test)
(use K-Compiler)
(use Util)
(use DataTypes)
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

(define (run- code)
  (let* ([res (read-s-exprs code)]
         [_   (hash-table-union! (car *exprs*types*) (car res))]
         [_   (hash-table-union! (cdr *exprs*types*) (cdr res))]
         [t   (type-check *exprs*types*)])
    (unless t (raise-error/message "type error"))
    (cons (Krivine (compile (car *exprs*types*))) t)))

(define (run code)
  (let* ([res1 (run- `((show ,code)))]
         [res2 (run- `(,code))])
    (list (car res1) (type->data (cdr res2)))))

(test-start "srfi-1-acceptance-test")

(test-section "constructors")
(test* "cons"
       '("1 : 2 : 3 : []" (List Number))
       (run `(cons 1 (cons 2 (cons 3 nil)))))
(test* "make-integers-from"
       '("2 : 3 : 4 : []" (List Number))
       (run `(take (make-integers-from 2) 3)))
(test* "integers"
       '("0 : 1 : 2 : 3 : []" (List Number))
       (run `(take integers 4)))

(test-section "selectors")
(test* "car"
       '("0" Number)
       (run `(car integers)))
(test* "cdr"
       '("1 : 2 : 3 : []" (List Number))
       (run `(cdr (take integers 4))))
(test* "list-ref"
       '("2" (Option Number))
       (run `(list-ref integers 2)))
(test* "list-ref nil"
       '("t" String)
       (run `(none? (list-ref (cons 1 nil) 8) "t" "f")))
(test* "take"
       '("0 : 1 : 2 : 3 : []" (List Number))
       (run `(take integers 4)))
(test* "drop"
       '("2 : 3 : []" (List Number))
       (run `(take (drop integers 2) 2)))
(test* "take-right"
       '("2 : 3 : 4 : []" (List Number))
       (run `(take-right (take integers 5) 3)))
(test* "drop-right"
       '("0 : 1 : []" (List Number))
       (run `(drop-right (take integers 5) 3)))
(test* "split-at"
       '("0 : 1 : 2 : 3 : 4 : []" (List Number))
       (run `(fst (split-at (take integers 10) 5))))
(test* "split-at"
       '("5 : 6 : 7 : 8 : 9 : []" (List Number))
       (run `(snd (split-at (take integers 10) 5))))
(test* "last"
       '("5" Number)
       (run `(last (take integers 6))))

(test-section "miscellaneous")
(test* "length"
       '("0" Number)
       (run `(length nil)))
(test* "length"
       '("10" Number)
       (run `(length (take integers 10))))
(test* "append"
       '("0 : 1 : 2 : 0 : 1 : 2 : 3 : []" (List Number))
       (run `(append (take integers 3) (take integers 4))))
#;(test* "concatenate" "0 : 1 : 2 : 0 : 1 : 2 : 3 : []"
       (run `(concatenate (cons (take integers 3) (cons (take integers 4) nil)))))
(test* "reverse"
       '("5 : 4 : 3 : 2 : 1 : 0 : []" (List Number))
       (run `(reverse (take integers 6))))
(test* "zip"
       '("0 : 2 : 1 : 1 : 2 : 0 : []" (List (Pair Number Number)))
       (run `(zip (take integers 3) (reverse (take integers 3)))))
;;(test* "unzip" "0 : 1 : 2 : []" (run `(fst (unzip (zip (take integers 3) (reverse (take integers 3)))))))
;;(test* "unzip" "2 : 1 : 0 : []" (run `(snd (unzip (zip (take integers 3) (reverse (take integers 3)))))))
(test* "count1"
       '("3" Number)
       (run `(count1 (cons 1 (cons 2 (cons 1 (cons 3 (cons 1 nil))))) (=? 1))))
(test* "count2"
       '("3" Number)
       (run `(count2 (cons 1 (cons 2 (cons 1 (cons 3 (cons 1 nil)))))
                     (cons 1 (cons 2 (cons 1 (cons 3 (cons 1 nil))))) (^ x y (=? 2 (+ x y))))))

(test-section "fold : map")
(test* "fold"
       '("55" Number)
       (run `(foldl (take integers 11) + 0)))
(test* "foldl"
       '("2 : 1 : 0 : []" (List Number))
       (run `(foldl (take integers 3) cons nil)))
(test* "foldr"
       '("55" Number)
       (run `(foldr (take integers 11) + 0)))
(test* "foldr"
       '("0 : 1 : 2 : []" (List Number))
       (run `(foldr (take integers 3) cons nil)))
#;(test* "unfold" "1 : 4 : 9 : 16 : 25 : 36 : 49 : 64 : 81 : 100 : []"
       (run `(unfold (< 10) (^ x (* x x)) (+ 1) 1)))
(test* "map"
       '("0 : 2 : 4 : 6 : 8 : []" (List Number))
       (run `(map (take integers 5) (* 2))))

(test-section "filtering : partitioning")
(test* "filter"
       '("1 : 3 : 2 : []" (List Number))
       (run `(filter (cons 1 (cons 9 (cons 3 (cons 12 (cons 2 nil))))) (> 5))))
;;(test* "partition" "1 : 3 : []"
;;       (run `(fst (partition (cons 1 (cons "2" (cons 3 (cons "4" nil)))) number?))))
;;(test* "partition" "2 : 4 : []"
;;       (run `(snd (partition (cons 1 (cons "2" (cons 3 (cons "4" nil)))) number?))))
(test* "remove"
       '("5 : 6 : 7 : []" (List Number))
       (run `(remove (take integers 8) (> 5))))

(test-section "searching")
(test* "find"
       '("6" (Option Number))
       (run `(find (take integers 10) (< 5))))
(test* "find"
       '("none" String)
       (run `(none? (find (cons 3 nil) (< 5)) "none" "some")))
(test* "find-tail"
       '("6 : 7 : 8 : 9 : []" (List Number))
       (run `(find-tail (take integers 10) (< 5))))
(test* "take-while"
       '("0 : 1 : 2 : 3 : []" (List Number))
       (run `(take-while (take integers 8) (> 4))))
(test* "drop-while"
       '("4 : 5 : 6 : 7 : []" (List Number))
       (run `(drop-while (take integers 8) (> 4))))
(test* "any?"
       '("true" String)
       (run `(any? (take integers 5) (=? 3) "true" "false")))
(test* "any?"
       '("false" String)
       (run `(any? (take integers 5) (=? 100) "true" "false")))
(test* "every?"
       '("true" String)
       (run `(every? (take integers 5) (> 8) "true" "false")))
(test* "every?"
       '("false" String)
       (run `(every? (take integers 5) (> 3) "true" "false")))
(test* "list-index"
       '("7" (Option Number))
       (run `(list-index (take integers 10) (< 6))))
(test* "list-index"
       '("none" String)
       (run `(none? (list-index (take integers 10) (< 20)) "none" "some")))
(test* "member"
       '("3 : 4 : 5 : []" (List Number))
       (run `(member (take integers 6) 3)))
(test* "delete"
       '("2 : 3 : []" (List Number))
       (run `(delete (cons 1 (cons 2 (cons 1 (cons 3 nil)))) 1)))
(test* "delete-duplicates"
       '("1 : 2 : 3 : []" (List Number))
       (run `(delete-duplicates (cons 1 (cons 2 (cons 1 (cons 3 (cons 2 nil))))))))

(test-section "association list")
(test* "assq"
       '("nori" (Option String))
       (run `(assq "nazuna" (cons (pair "yuno" "miyako") (cons (pair "nazuna" "nori") nil)))))
(test* "assq"
       '("none" String)
       (run `(none? (assq "hiro" (cons (pair "yuno" "miyako") (cons (pair "nazuna" "nori") nil))) "none" "some")))
(test* "acons"
       '("yunocchi : miyako : yuno : miyako : nazuna : nori : []" (List (Pair String String)))
       (run `(acons "yunocchi" "miyako" (acons "yuno" "miyako" (acons "nazuna" "nori" nil)))))
(test* "alist-copy"
       '("a : 1 : b : 5 : []" (List (Pair String Number)))
       (run `(alist-copy (acons "a" 1 (acons "b" 5 nil)))))
(test* "alist-delete"
       '("b : 5 : []" (List (Pair String Number)))
       (run `(alist-delete (cons (pair "a" 1) (cons (pair "b" 5) nil)) "a")))


(test-end :exit-on-failure #t)
