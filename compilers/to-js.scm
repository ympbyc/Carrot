;;;; Carrot -> JS
;;; 2014 Minori Yamashita <ympbyc@gmail.com>
;;;

(define-module Compiler
  (export compile)
  (use srfi-1)
  (use Util)
  (use DataTypes)

  (define (compile exprs-ht)
    (str
     (fold (fn [name*expr js-src]
               (let ([name (car name*expr)]
                     [expr (cdr name*expr)])
                 (str js-src
                      "var " (compatible-symbol name)
                      " = "
                      (if (lambda-expr? expr)
                          (compile-function expr '())
                          (thunk (compile-expr expr '())))
                      ";\n")))
           ""
           (hash-table->alist exprs-ht))
     js-prelude
     "console.log(main());"))

  (define (compile-function fn env)
    (let1 params (butlast (cdr fn))
          (if (null? params)
              (compile-expr (last fn) env)
              (str (compatible-symbol (car params))
                   " => "  (compile-function (cons '^ (cddr fn))
                                             (cons (car params) env))))))

  (define (compile-funcall f args env)
    (if (null? args) f
        (compile-funcall (str f "("
                              (thunk (compile-expr (car args) env)) ")")
                         (cdr args) env)))

  (define (compile-nativecall f args env)
    (if (null? args) f
        (compile-nativecall (str f "("
                              (compile-expr (car args) env) ")")
                         (cdr args) env)))

  (define (compatible-symbol sym)
    (replace-incompatible-chars (symbol->string sym)))

  (define (replace-incompatible-chars str)
    (let* ([str (regexp-replace-all #/-/ str "_")]
           [str (regexp-replace-all #/!/ str "_BANG_")]
           [str (regexp-replace-all #/\?/ str "_Q_")]
           [str (regexp-replace-all #/\*/ str "_STAR_")]
           [str (regexp-replace-all #/</ str "_LT_")]
           [str (regexp-replace-all #/>/ str "_GT_")]
           [str (regexp-replace-all #/\// str "_SLASH_")]
           [str (regexp-replace-all #/\+/ str "_SUM_")]
           [str (regexp-replace-all #/=/ str "_EQ_")]
           [str (regexp-replace-all #/%/ str "_PERC_")]
           [str (regexp-replace-all #/^false$/ str "_FALSE_")]
           [str (regexp-replace-all #/^true$/ str "_TRUE_")]
           [str (regexp-replace-all #/^if$/ str "_IF_")]
           [str (regexp-replace-all #/^delete$/ str "_DELETE_")])))


  (define (thunk exp)
    (str "() => " exp))


  (define (compile-expr expr env)
    (cond
     [(and (symbol? expr)
           (member expr env))
      (str (compatible-symbol expr) "()")]

     [(symbol? expr) (compatible-symbol expr)]

     [(or (string? expr) (keyword? expr) (char? expr))
      (str "'" (regexp-replace-all #/\n/
                (regexp-replace-all
                 (string->regexp "'") (str expr) "")
                "\\\\n") "'")]

     [(atom? expr) (str expr)]

     [(quote-expr? expr)
      (str "'(quote " (cadr expr) ")'")]

     [(lambda-expr? expr)
      (compile-function expr env)]

     [(native-expr? expr)
      (compile-nativecall (compile-expr (cadr expr) env) (cddr expr) env)]

     [else ;;funcall
      (compile-funcall (compile-expr (car expr) env) (cdr expr) env)]))




  (define js-prelude "
var c_equal_Q_  = x => y => { if (x == y) return _TRUE_; else return _FALSE_; };
var c__LT_      = x => y => { if (x < y)  return _TRUE_; else return _FALSE_; };
var c__GT_      = x => y => { if (x > y)  return _TRUE_; else return _FALSE_; };
var c__LT__EQ_  = x => y => { if (x <= y) return _TRUE_; else return _FALSE_; };
var c__GT__EQ   = x => y => { if (x >= y) return _TRUE_; else return _FALSE_; };
var c__SUM_     = x => y => x + y;
var c__         = x => y => x - y;
var c__STAR_    = x => y => x * y;
var c__SLASH_   = x => y => x / y;
var c__PERC_    = x => y => x % y;
var c_string_append     = x => y => x + y;
var number__GT_string   = x => '' + x
var keyword__GT_string  = x => x;
var timed_print = t => s => { console.log(s); return t + 1; };
var read = t => prompt()
"))
