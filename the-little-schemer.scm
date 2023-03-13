(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define member?
  (lambda (s l)
    (cond
     ((null? l) #f)
     (else (or (equal? (car l) s)
               (member? s (cdr l)))))))

(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? (car l) s) (cdr l))
     (else (cons (car l) (rember s (cdr l)))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l))
                 (firsts (cdr l)))))))

(define seconds
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (cadr (car l))
                 (seconds (cdr l)))))))

(define insertR
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((equal? (car l) old)
      (cons old (cons new (cdr l))))
     (else (cons (car l)
                 (insertR new old (cdr l)))))))

(define insertL
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((equal? (car l) old)
      (cons new l))
     (else (cons (car l)
                 (insertL new old (cdr l)))))))

(define subst
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((equal? (car l) old)
      (cons new (cdr l)))
     (else (cons (car l)
                 (subst new old (cdr l)))))))

(define subst2
  (lambda (new o1 o2 l)
    (cond
     ((null? l) '())
     ((or (equal? (car l) o1)
          (equal? (car l) o2))
      (cons new (cdr l)))
     (else (cons (car l)
                 (subst2 new o1 o2 (cdr l)))))))

(define multirember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? (car l) s)
      (multirember s (cdr l)))
     (else (cons (car l)
                 (multirember s (cdr l)))))))

(define multiinsertR
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((equal? (car l) old)
      (cons old
            (cons new
                  (multiinsertR new old (cdr l)))))
     (else (cons (car l)
                 (multiinsertR new old (cdr l)))))))

(define multiinsertL
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((equal? (car l) old)
      (cons new
            (cons old
                  (multiinsertL new old (cdr l)))))
     (else (cons (car l)
                 (multiinsertL new old (cdr l)))))))

(define multisubst
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((equal? (car l) old)
      (cons new
            (multisubst new old (cdr l))))
     (else (cons (car l)
                 (multisubst new old (cdr l)))))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup)
               (addtup (cdr tup)))))))

(define o*
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (o* n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (o+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
     ((zero? m) (zero? n))
     ((zero? n) #f)
     (else (o= (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
     ((or (o> n m) (o< n m)) #f)
     (else #t))))

(define oexpt
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (o* n (oexpt n (sub1 m)))))))

(define o/
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (o/ (o- n m) m))))))

(define length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (length (cdr l)))))))

(define pick
  (lambda (n l)
    (cond
     ((zero? (sub1 n)) (car l))
     (else (pick (sub1 n) (cdr l))))))

(define one?
  (lambda (n)
    (equal? n 1)))

(define rempick
  (lambda (n l)
    (cond
     ((one? n) (cdr l))
     (else (cons (car l)
                 (rempick (sub1 n) (cdr l)))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat))
      (no-nums (cdr lat)))
     (else (cons (car lat)
                 (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat))
      (cons (car lat)
            (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (o= a1 a2))
     ((or (number? a1) (number? a2))
      #f)
     (else (eq? a1 a2)))))

(define occur
  (lambda (s l)
    (cond
     ((null? l) 0)
     ((equal? (car l) s)
      (add1 (occur s (cdr l))))
     (else (occur s (cdr l))))))

(define rember*
  (lambda (s l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((equal? (car l) s) (rember* s (cdr l)))
       (else (cons (car l) (rember s (cdr l))))))
     (else
      (cond
       ((equal? (car l) s) (rember* s (cdr l)))
       (else (cons (rember* s (car l))
                   (rember* s (cdr l)))))))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((equal? (car l) old)
        (cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
     (else
      (cond
       ((equal? (car l) old)
        (cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (insertR* new old (car l))
                   (insertR* new old (cdr l)))))))))

(define occur*
  (lambda (s l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((equal? (car l) s)
        (add1 (occur* s (cdr l))))
       (else (occur* s (cdr l)))))
     (else
      (cond
       ((equal? (car l) s)
        (add1 (occur* s (cdr l))))
       (else (o+ (occur* s (car l))
                 (occur* s (cdr l)))))))))
(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((equal? (car l) old)
        (cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
     (else
      (cond
       ((equal? (car l) old)
        (cons new (subst* new old (cdr l))))
       (else (cons (subst* new old (car l))
                   (subst* new old (cdr l)))))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((equal? (car l) old)
        (cons new (cons old (insertL* new old (cdr l)))))
       (else (cons (car l) (insertL* new old (cdr l))))))
     (else
      (cond
       ((equal? (car l) old)
        (cons new (cons old (insertL* new old (cdr l)))))
       (else (cons (insertL* new old (car l))
                   (insertL* new old (cdr l)))))))))

(define member*
  (lambda (s l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (or (equal? (car l) s)
          (member* s (cdr l))))
     (else (or (equal? (car l) s)
               (member* s (car l))
               (member* s (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else (and (equal? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else (and (numbered? (car aexp))
                (numbered? (caddr aexp)))))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (cadr nexp) 'o+)
      (o+ (value (car nexp))
          (value (caddr nexp))))
     ((eq? (cadr nexp) 'o*)
      (o* (value (car nexp))
          (value (caddr nexp))))
     (else
      (oexpt (value (car nexp))
             (value (caddr nexp)))))))

(define 1st-sub-exp
  (lambda (aexp)
    (cadr aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (caddr aexp)))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) 'o+)
      (o+ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) 'o*)
      (o* (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
     (else
      (oexpt (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp)))))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define s+
  (lambda (n m)
    (cond
     ((sero? m) n)
     (else (edd1 (s+ n (zub1 m)))))))

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat))
      (makeset (cdr lat)))
     (else (cons (car lat) (makeset (cdr lat)))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cons (car lat)
                 (makeset
                  (multirember (car lat)
                               (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (and (member? (car set1) set2)
           (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else (or (member? (car set1) set2)
               (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (cons (car set1) (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else (cons (car set1)
                 (union (cdr set1) set2))))))

(define difference
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (difference (cdr set1) set2))
     (else (cons (car set1)
                 (difference (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set)
                      (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (revpair (car rel))
                 (revrel (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
        (cons new (cons old (cdr l))))
       (else (cons (car l)
                   ((insertL-f test?) new old
                    (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
        (cons old (cons new (cdr l))))
       (else (cons (car l)
                   ((insertR-f test?) new old
                    (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define seqrem
  (lambda (new old l)
    l))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((eq? (car l) old)
        (seq new old (cdr l)))
       (else (cons (car l)
                   ((insert-g seq) new old (cdr l))))))))

(define insertL
  (insert-g
   (lambda (new old l) ; seqL
     (cons new (cons old l)))))

(define insertR
  (insert-g
   (lambda (new old l) ; seqR
     (cons old (cons new l)))))

(define subst (insert-g seqS))

(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x 'o+) o+)
     ((eq? x 'o*) o*)
     (else oexpt))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else ((atom-to-function (operator nexp))
            (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (s l)
      (cond
       ((null? l) '())
       ((test? (car l) s)
        ((multirember-f test?) s (cdr l)))
       (else (cons (car l)
                   ((multirember-f test?) s (cdr l))))))))

(define multirember-eq?
  (multirember-f eq?))

(define multiremberT
  (lambda (test? l)
    (cond
     ((null? l) '())
     ((test? (car l))
      (multiremberT test? (cdr l)))
     (else (cons (car l)
                 (multiremberT test? (cdr l)))))))

(define eq?-tuna
  (eq?-c 'tuna))

(define a-friend
  (lambda (x y)
    (null? y)))

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col '() '()))
     ((eq? (car lat) a)
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col newlat
                             (cons (car lat) seen)))))
     (else
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col (cons (car lat) newlat)
                             seen)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new
            (cons oldL
                  (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR
            (cons new
                  (multiinsertLR new oldL oldR (cdr lat)))))
     (else
      (cons (car lat)
            (multiinsertLR new oldL oldR (cdr lat)))))))


(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col '() 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R)
                          (col (cons new (cons oldL newlat))
                               (add1 L) R))))
     ((eq? (car lat) oldR)
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R)
                          (col (cons oldR (cons new newlat))
                               L (add1 R)))))
     (else
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R)
                          (col (cons (car lat) newlat) L R)))))))

(define even?
  (lambda (n)
    (o= (o* (o/ n 2) 2) n)))

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((even? (car l))
        (cons (car l) (evens-only* (cdr l))))
       (else
        (evens-only* (cdr l)))))
     (else
      (cons (evens-only* (car l))
            (evens-only* (cdr l)))))))

(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l)
      (col '() 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
        (evens-only*&co (cdr l)
                        (lambda (newl p s)
                          (col (cons (car l) newl)
                               (o* (car l) p)
                               s))))
       (else
        (evens-only*&co (cdr l)
                        (lambda (newl p s)
                          (col newl
                               p
                               (o+ (car l) s)))))))
     (else
      (evens-only*&co (car l)
                      (lambda (al ap as)
                        (evens-only*&co (cdr l)
                                        (lambda (dl dp ds)
                                          (col (cons al dl)
                                               (o* ap dp)
                                               (o+ as ds))))))))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn)
      (keep-looking a (pick sorn lat) lat))
     (else (eq? sorn a)))))

(define eternity
  (lambda (x)
    (eternity x)))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else (build (first pora)
                  (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (o+ (length* (first pora))
               (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (o+ (o* (weight* (first pora)) 2)
               (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (shuffle (revpair pora)))
     (else (build (first pora)
                  (shuffle (second pora)))))))

(define C
  (lambda (n)
    (cond
     ((one? n) 1)
     (else
      (cond
       ((even? n) (C (o/ n 2)))
       (else (C (add1 (o* 3 n)))))))))

(define A
  (lambda (n m)
    (cond
     ((zero? n) (add1 m))
     ((zero? m) (A (sub1 n) 1))
     (else (A (sub1 n)
              (A n (sub1 m)))))))

;; length<=2
((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
   eternity)))

;; length<=3
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length
      (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

;; length
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1
             ((mk-length mk-length)
              (cdr l))))))))

;; length
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))

;; length
((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help
     name
     (first entry)
     (second entry)
     entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name)
      (car values))
     (else
      (lookup-in-entry-help
       name
       (cdr names)
       (cdr values)
       entry-f)))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else (lookup-in-entry
            name
            (car table)
            (lambda (name)
              (lookup-in-table name (cdr table) table-f)))))))

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e 'cons) *const)
     ((eq? e 'car) *const)
     ((eq? e 'cdr) *const)
     ((eq? e 'null?) *const)
     ((eq? e 'eq?) *const)
     ((eq? e 'atom?) *const)
     ((eq? e 'zero?) *const)
     ((eq? e 'add1) *const)
     ((eq? e 'sub1) *const)
     ((eq? e 'number?) *const)
     (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) 'quote)
        *quote)
       ((eq? (car e) 'lambda)
        *lambda)
       ((eq? (car e) 'cond)
        *cond)
       (else *application)))
     (else *application))))

(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)) table)
      (meaning (answer-of (car lines)) table))
     (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x 'else))
     (else #f))))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else
      (cons (meaning (car args) table)
            (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define apply
  (lambda (fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive (second fun) vals))
     ((non-primitive? fun)
      (apply-closure (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name 'cons)
      (cons (first vals) (second vals)))
     ((eq? name 'car)
      (car (first vals)))
     ((eq? name 'cdr)
      (cdr (first vals)))
     ((eq? name 'null?)
      (null? (first vals)))
     ((eq? name 'eq?)
      (eq? (first vals) (second vals)))
     ((eq? name 'atom?)
      (:atom? (first vals)))
     ((eq? name 'zero?)
      (zero? (first vals)))
     ((eq? name 'add1)
      (add1 (first vals)))
     ((eq? name 'sub1)
      (sub1 (first vals)))
     ((eq? name 'number?)
      (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) 'primitive) #t)
     ((eq? (car x) 'non-primitive) #t)
     (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))
