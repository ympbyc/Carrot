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
       (run `(take 3 (make-integers-from 2))))
(test* "integers"
       '("0 : 1 : 2 : 3 : []" (List Number))
       (run `(take 4 integers)))

(test-section "selectors")
(test* "car"
       '("0" Number)
       (run `(car integers)))
(test* "cdr"
       '("1 : 2 : 3 : []" (List Number))
       (run `(cdr (take 4 integers))))
(test* "list-ref"
       '("2" (Option Number))
       (run `(list-ref 2 integers)))
(test* "list-ref nil"
       '("t" String)
       (run `(none? (list-ref 8 (cons 1 nil)) "t" "f")))
(test* "take"
       '("0 : 1 : 2 : 3 : []" (List Number))
       (run `(take 4 integers)))
(test* "drop"
       '("2 : 3 : []" (List Number))
       (run `(take 2 (drop 2 integers))))
(test* "take-right"
       '("2 : 3 : 4 : []" (List Number))
       (run `(take-right 3 (take 5 integers))))
(test* "drop-right"
       '("0 : 1 : []" (List Number))
       (run `(drop-right 3 (take 5 integers))))
(test* "split-at"
       '("0 : 1 : 2 : 3 : 4 : []" (List Number))
       (run `(fst (split-at 5 (take 10 integers)))))
(test* "split-at"
       '("5 : 6 : 7 : 8 : 9 : []" (List Number))
       (run `(snd (split-at 5 (take 10 integers)))))
(test* "last"
       '("5" Number)
       (run `(last (take 6 integers))))

(test-section "miscellaneous")
(test* "length"
       '("0" Number)
       (run `(length nil)))
(test* "length"
       '("10" Number)
       (run `(length (take 10 integers))))
(test* "append"
       '("0 : 1 : 2 : 0 : 1 : 2 : 3 : []" (List Number))
       (run `(append (take 3 integers) (take 4 integers))))
#;(test* "concatenate" "0 : 1 : 2 : 0 : 1 : 2 : 3 : []"
       (run `(concatenate (cons (take integers 3) (cons (take integers 4) nil)))))
(test* "reverse"
       '("5 : 4 : 3 : 2 : 1 : 0 : []" (List Number))
       (run `(reverse (take 6 integers))))
(test* "zip"
       '("0 : 2 : 1 : 1 : 2 : 0 : []" (List (Pair Number Number)))
       (run `(zip (take 3 integers) (reverse (take 3 integers)))))
;;(test* "unzip" "0 : 1 : 2 : []" (run `(fst (unzip (zip (take integers 3) (reverse (take integers 3)))))))
;;(test* "unzip" "2 : 1 : 0 : []" (run `(snd (unzip (zip (take integers 3) (reverse (take integers 3)))))))
(test* "count1"
       '("3" Number)
       (run `(count1  (=? 1) (cons 1 (cons 2 (cons 1 (cons 3 (cons 1 nil))))))))
(test* "count2"
       '("3" Number)
       (run `(count2 (^ x y (=? 2 (+ x y)))
                     (cons 1 (cons 2 (cons 1 (cons 3 (cons 1 nil)))))
                     (cons 1 (cons 2 (cons 1 (cons 3 (cons 1 nil))))))))

(test-section "fold : map")
(test* "fold"
       '("55" Number)
       (run `(foldl  + 0 (take 11 integers))))
(test* "foldl"
       '("2 : 1 : 0 : []" (List Number))
       (run `(foldl cons nil (take 3 integers))))
(test* "foldr"
       '("55" Number)
       (run `(foldr + 0 (take 11 integers))))
(test* "foldr"
       '("0 : 1 : 2 : []" (List Number))
       (run `(foldr cons nil (take 3 integers))))
#;(test* "unfold" "1 : 4 : 9 : 16 : 25 : 36 : 49 : 64 : 81 : 100 : []"
       (run `(unfold (< 10) (^ x (* x x)) (+ 1) 1)))
(test* "map"
       '("0 : 2 : 4 : 6 : 8 : []" (List Number))
       (run `(map (* 2) (take 5 integers))))

(test-section "filtering : partitioning")
(test* "filter"
       '("1 : 3 : 2 : []" (List Number))
       (run `(filter (> 5) (cons 1 (cons 9 (cons 3 (cons 12 (cons 2 nil))))))))
;;(test* "partition" "1 : 3 : []"
;;       (run `(fst (partition (cons 1 (cons "2" (cons 3 (cons "4" nil)))) number?))))
;;(test* "partition" "2 : 4 : []"
;;       (run `(snd (partition (cons 1 (cons "2" (cons 3 (cons "4" nil)))) number?))))
(test* "remove"
       '("5 : 6 : 7 : []" (List Number))
       (run `(remove (> 5) (take 8 integers))))

(test-section "searching")
(test* "find"
       '("6" (Option Number))
       (run `(find  (< 5) (take 10 integers))))
(test* "find"
       '("none" String)
       (run `(none? (find (< 5) (cons 3 nil)) "none" "some")))
(test* "find-tail"
       '("6 : 7 : 8 : 9 : []" (List Number))
       (run `(find-tail (< 5) (take 10 integers))))
(test* "take-while"
       '("0 : 1 : 2 : 3 : []" (List Number))
       (run `(take-while (> 4) (take 8 integers))))
(test* "drop-while"
       '("4 : 5 : 6 : 7 : []" (List Number))
       (run `(drop-while (> 4) (take 8 integers))))
(test* "any?"
       '("true" String)
       (run `(any? (=? 3) (take 5 integers) "true" "false")))
(test* "any?"
       '("false" String)
       (run `(any? (=? 100) (take 5 integers) "true" "false")))
(test* "every?"
       '("true" String)
       (run `(every? (> 8) (take 5 integers) "true" "false")))
(test* "every?"
       '("false" String)
       (run `(every? (> 3) (take 5 integers) "true" "false")))
(test* "list-index"
       '("7" (Option Number))
       (run `(list-index (< 6) (take 10 integers))))
(test* "list-index"
       '("none" String)
       (run `(none? (list-index (< 20) (take 10 integers)) "none" "some")))
(test* "member"
       '("3 : 4 : 5 : []" (List Number))
       (run `(member 3 (take 6 integers))))
(test* "delete"
       '("2 : 3 : []" (List Number))
       (run `(delete 1 (cons 1 (cons 2 (cons 1 (cons 3 nil)))))))
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
       (run `(alist-delete "a" (cons (pair "a" 1) (cons (pair "b" 5) nil)))))


(test-end :exit-on-failure #t)
