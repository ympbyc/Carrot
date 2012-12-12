(add-load-path "../lib/" :relative)

(use gauche.test)
(test-start "srfi-1")
(use Compiler)

(define (pre-load fname g-e)
  (call-with-input-file fname (lambda (file-port)
    (receive (result g-env) 
             (Krivine (compile (read-list file-port)) '() '() g-e)
      g-env))))

(define (read-list port)
  (let ((exp (read port)))
    (if (eof-object? exp) '()
      (cons exp (read-list port)))))

(define g-env
  (pre-load "examples/prelude.nadeko" (pre-load "examples/srfi-1.nadeko" '())))

(define (run code)
  (receive (result bindings) (Krivine (compile `((show ,code))) '() '() g-env)
    result))

(test-section "constructors")
(test* "cons" "1, 2, 3, nil" (run `(cons 1 (cons 2 (cons 3 nil)))))
(test* "make-integers-from" "2, 3, 4, nil" (run `(take (make-integers-from 2) 3)))
(test* "integers" "0, 1, 2, 3, nil" (run `(take integers 4)))

(test-section "selectors")
(test* "car" "0" (run `(car integers)))
(test* "cdr" "1, 2, 3, nil" (run `(cdr (take integers 4))))
(test* "list-ref" "2" (run `(list-ref integers 2)))
(test* "take" "0, 1, 2, 3, nil" (run `(take integers 4)))
(test* "drop" "2, 3, nil" (run `(take (drop integers 2) 2)))
(test* "take-right" "2, 3, 4, nil" (run `(take-right (take integers 5) 3)))
(test* "drop-right" "0, 1, nil" (run `(drop-right (take integers 5) 3)))
(test* "split-at" "0, 1, 2, 3, 4, nil" (run `(fst (split-at (take integers 10) 5))))
(test* "split-at" "5, 6, 7, 8, 9, nil" (run `(snd (split-at (take integers 10) 5))))
(test* "last" "5" (run `(last (take integers 6))))

(test-section "miscellaneous")
(test* "length" "0" (run `(length nil)))
(test* "length" "10" (run `(length (take integers 10))))
(test* "append" "0, 1, 2, 0, 1, 2, 3, nil" (run `(append (take integers 3) (take integers 4))))
(test* "concatenate" "0, 1, 2, 0, 1, 2, 3, nil" 
  (run `(concatenate (cons (take integers 3) (cons (take integers 4) nil)))))
(test* "reverse" "5, 4, 3, 2, 1, 0, nil" (run `(reverse (take integers 6))))
(test* "zip" "0, 2, 1, 1, 2, 0, nil" (run `(zip (take integers 3) (reverse (take integers 3)))))
(test* "unzip" "0, 1, 2, nil" (run `(fst (unzip (zip (take integers 3) (reverse (take integers 3)))))))
(test* "unzip" "2, 1, 0, nil" (run `(snd (unzip (zip (take integers 3) (reverse (take integers 3)))))))
(test* "count1" "3" (run `(count1 (cons 1 (cons 2 (cons 1 (cons 3 (cons 1 nil))))) (eq? 1))))
(test* "count2" "3" 
  (run `(count2 (cons 1 (cons 2 (cons 1 (cons 3 (cons 1 nil)))))
                (cons 1 (cons 2 (cons 1 (cons 3 (cons 1 nil))))) (-> (x y) (eq? 2 (+ x y))))))

(test-section "fold, map")
(test* "fold" "55" (run `(fold (take integers 11) + 0)))
(test* "fold" "2, 1, 0, nil" (run `(fold (take integers 3) cons nil)))
(test* "fold-right" "55" (run `(fold-right (take integers 11) + 0)))
(test* "fold-right" "0, 1, 2, nil" (run `(fold-right (take integers 3) cons nil)))
(test* "unfold" "1, 4, 9, 16, 25, 36, 49, 64, 81, 100, nil"
  (run `(unfold (< 10) (-> (x) (* x x)) (+ 1) 1)))
(test* "map" "0, 2, 4, 6, 8, nil" (run `(map (take integers 5) (* 2))))

(test-section "filtering, partitioning")
(test* "filter" "1, 3, 2, nil"
  (run `(filter (cons 1 (cons 9 (cons 3 (cons 12 (cons 2 nil))))) (> 5))))
(test* "partition" "1, 3, nil"
  (run `(fst (partition (cons 1 (cons "2" (cons 3 (cons "4" nil)))) number?))))
(test* "partition" "2, 4, nil"
  (run `(snd (partition (cons 1 (cons "2" (cons 3 (cons "4" nil)))) number?))))
(test* "remove" "5, 6, 7, nil"
  (run `(remove (take integers 8) (> 5))))

(test-section "searching")
(test* "find" "6" (run `(find (take integers 10) (< 5))))
(test* "find-tail" "6, 7, 8, 9, nil" (run `(find-tail (take integers 10) (< 5))))
(test* "take-while" "0, 1, 2, 3, nil" (run `(take-while (take integers 8) (> 4))))
(test* "drop-while" "4, 5, 6, 7, nil" (run `(drop-while (take integers 8) (> 4))))
(test* "any?" "true" (run `(any? (take integers 5) (= 3) "true" "false")))
(test* "any?" "false" (run `(any? (take integers 5) (= 100) "true" "false")))
(test* "every?" "true" (run `(every? (take integers 5) (> 8) "true" "false")))
(test* "every?" "false" (run `(every? (take integers 5) (> 3) "true" "false")))
(test* "list-index" "7" (run `(list-index (take integers 10) (< 6))))
(test* "member" "3, 4, 5, nil" (run `(member (take integers 6) 3)))
(test* "delete" "2, 3, nil" (run `(delete (cons 1 (cons 2 (cons 1 (cons 3 nil)))) 1)))
(test* "delete-duplicates" "1, 2, 3, nil" 
  (run `(delete-duplicates (cons 1 (cons 2 (cons 1 (cons 3 (cons 2 nil))))))))

(test-section "association list")
(test* "assoc" "nazuna, nori" 
  (run `(assoc (cons (2-tuple "a" 1) (cons (2-tuple "nazuna" "nori") nil)) "nazuna")))
(test* "alist-cons" "yunocchi, miyako"
  (run `(assoc (alist-cons "yunocchi" "miyako" (cons (2-tuple "a" 1) (cons (2-tuple "nazuna" "nori") nil))) "yunocchi")))
(test* "alist-copy" "a, 1, nazuna, nori, nil"
  (run `(alist-copy (cons (2-tuple "a" 1) (cons (2-tuple "nazuna" "nori") nil)))))
(test* "alist-delete" "nazuna, nori, nil"
  (run `(alist-delete (cons (2-tuple "a" 1) (cons (2-tuple "nazuna" "nori") nil)) "a")))


(test-end :exit-on-failure #t)
