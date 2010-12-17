(import
 (rnrs)
 ;;for AD fun comment these in and (rnrs) out:
 ;;(except (rnrs) real? negative? positive? zero? >= <= > < = atan cos sin expt log exp sqrt / * - +)
 ;;(church utils AD)
 
 (rnrs mutable-pairs) ;;because the header uses set-car! when note storethreading.
 (_srfi :1) ;;provides some list functions that are used.
 (_srfi :19) ;;date+time for inference timing
 (rename (church external math-env) (sample-discrete discrete-sampler)) ;;this provides the gsl sampling/scoring functions.
 (rename (only (ikarus)
               gensym ;;this is needed.
               pretty-print
               exact->inexact) ;;this isn't really needed.
         (gensym scheme-gensym))

 (church trie)

 (church compiler)
 (rnrs eval)
 ;;personal library
 ;;(pi lazy)
 (pi abstract)
 (util)
 (sym)
 )

(define church-pretty-print
  (lambda (address store . args) (apply pretty-print args)))
(define church-null?
  (lambda (address store . args) (apply null? args)))
(define church-list-ref
  (lambda (address store . args) (apply list-ref args)))
(define church-length
  (lambda (address store . args) (apply length args)))
(define church-*
  (lambda (address store . args) (apply * args)))
(define church-+
  (lambda (address store . args) (apply + args)))
(define church-=
  (lambda (address store . args) (apply = args)))
(define church--
  (lambda (address store . args) (apply - args)))
(define church-append
  (lambda (address store . args) (apply append args)))
(define church-list
  (lambda (address store . args) (apply list args)))
(define church-equal?
  (lambda (address store . args) (apply equal? args)))
(define church-expt
  (lambda (address store . args) (apply expt args)))
(define church-lev-dist
  (lambda (address store . args) (apply lev-dist args)))
(define church-list?
  (lambda (address store . args) (apply list? args)))
(define church-not
  (lambda (address store . args) (apply not args)))
(define church-max
  (lambda (address store . args) (apply max args)))
(define church-/
  (lambda (address store . args) (apply / args)))
(define church-display
  (lambda (address store . args) (apply display args)))
(define church-third
  (lambda (address store . args) (apply third args)))
(define church-second
  (lambda (address store . args) (apply second args)))
(define church-test-abstraction-proposer
  (lambda (address store . args)
    (apply test-abstraction-proposer args)))
(define church-mcmc-state->query-value
  (lambda (address store . args)
    (apply mcmc-state->query-value args)))
(define church-mcmc-state->score
  (lambda (address store . args)
    (apply mcmc-state->score args)))
(define church-iota
  (lambda (address store . args) (apply iota args)))
(define church-<
  (lambda (address store . args) (apply < args)))
(define church-random-real
  (lambda (address store . args) (apply random-real args)))
(define church-car
  (lambda (address store . args) (apply car args)))
(define church-log
  (lambda (address store . args) (apply log args)))
(define church-exp
  (lambda (address store . args) (apply exp args)))
(define church-sample-dirichlet
  (lambda (address store . args)
    (apply sample-dirichlet args)))
(define church-dirichlet-lnpdf
  (lambda (address store . args)
    (apply dirichlet-lnpdf args)))
(define church-discrete-sampler
  (lambda (address store . args)
    (apply discrete-sampler args)))
(define church->=
  (lambda (address store . args) (apply >= args)))
(define church->
  (lambda (address store . args) (apply > args)))
(define church-discrete-pdf
  (lambda (address store . args) (apply discrete-pdf args)))
(define church-random-integer
  (lambda (address store . args) (apply random-integer args)))
(define church-integer?
  (lambda (address store . args) (apply integer? args)))
(define church-sample-gaussian
  (lambda (address store . args)
    (apply sample-gaussian args)))
(define church-gaussian-lnpdf
  (lambda (address store . args) (apply gaussian-lnpdf args)))
(define church-scheme-gensym
  (lambda (address store . args) (apply scheme-gensym args)))
(define church-lnfact
  (lambda (address store . args) (apply lnfact args)))
(define church-take
  (lambda (address store . args) (apply take args)))
(define church-drop
  (lambda (address store . args) (apply drop args)))
(define church-make-list
  (lambda (address store . args) (apply make-list args)))
(define church-eq?
  (lambda (address store . args) (apply eq? args)))
(define church-error
  (lambda (address store . args) (apply error args)))
(define church-addbox-empty?
  (lambda (address store . args) (apply addbox-empty? args)))
(define church-mcmc-state->xrp-draws
  (lambda (address store . args)
    (apply mcmc-state->xrp-draws args)))
(define church-addbox-size
  (lambda (address store . args) (apply addbox-size args)))
(define church-counterfactual-update
  (lambda (address store . args)
    (apply counterfactual-update args)))
(define church-xrp-draw-proposer
  (lambda (address store . args)
    (apply xrp-draw-proposer args)))
(define church-addbox->alist
  (lambda (address store . args) (apply addbox->alist args)))
(define church-min
  (lambda (address store . args) (apply min args)))
(define church-current-date
  (lambda (address store . args) (apply current-date args)))
(define church-nan?
  (lambda (address store . args) (apply nan? args)))
(define church-exact->inexact
  (lambda (address store . args) (apply exact->inexact args)))
(define church-procedure?
  (lambda (address store . args) (apply procedure? args)))
(define church-pair?
  (lambda (address store . args) (apply pair? args)))
(define church-sym
  (lambda (address store . args) (apply sym args)))
(define (church-apply address store proc args)
  (apply proc address store args))
(define (church-eval addr store sexpr)
  ((eval
     `(letrec
        ,(map
           (lambda (def)
             (if (symbol? (cadr def))
                 (list (cadr def) (caddr def))
                 `(,(car (cadr def))
                    (lambda ,(cdr (cadr def)) ,@(cddr def)))))
           (compile (list sexpr) '()))
        church-main)
     (environment '(rnrs) '(rnrs mutable-pairs) '(_srfi :1)
       '(rename (church external math-env)
          (sample-discrete discrete-sampler))
       '(rename
          (only (ikarus) gensym pretty-print exact->inexact)
          (gensym scheme-gensym))
       '(_srfi :19) '(church compiler) '(rnrs eval)))
    addr store))
(define (church-get-current-environment address store)
  (error 'gce "gce not implemented"))
(define church-true #t)
(define church-false #f)
(define church-pair
  (lambda
    (address store #{g0 |3<Ox!VGv>lPCR630|}
      #{g1 |2GP%&FNpDN6qw<9e|})
    (cons #{g0 |3<Ox!VGv>lPCR630|} #{g1 |2GP%&FNpDN6qw<9e|})))
(define church-first
  (lambda (address store #{g2 |52Up%4KE%86%?OsJ|})
    (car #{g2 |52Up%4KE%86%?OsJ|})))
(define church-rest
  (lambda (address store #{g3 |BuCH$ZoRwaQ&%sri|})
    (cdr #{g3 |BuCH$ZoRwaQ&%sri|})))
(define (church-or address store . args)
  (fold (lambda (x y) (or x y)) #f args))
(define (church-and address store . args)
  (fold (lambda (x y) (and x y)) #t args))
(define (lev-dist) (error "lev-dist not implemented"))
(define (church-force address store val)
  (if (and (pair? val) (eq? (car val) 'delayed))
      (church-force address store ((cadr val) address store))
      val))
(define (make-store xrp-draws xrp-stats score tick
          enumeration-flag)
  (list xrp-draws xrp-stats score tick enumeration-flag))
(define (make-empty-store)
  (make-store (make-addbox) (make-addbox) 0.0 0 #f))
(define store->xrp-draws first)
(define store->xrp-stats second)
(define store->score third)
(define store->tick fourth)
(define store->enumeration-flag fifth)
(define (church-reset-store-xrp-draws address store)
  (return-with-store store
    (make-store (make-addbox) (store->xrp-stats store)
      (store->score store) (store->tick store)
      (store->enumeration-flag store))
    'foo))
(define (return-with-store store new-store value)
  (begin
    (set-car! store (car new-store))
    (set-cdr! store (cdr new-store))
    value))
(define alist-insert
  (lambda (addbox address info)
    (cons (cons address info) addbox)))
(define alist-pop
  (lambda (addbox address)
    (if (null? addbox)
        (cons #f '())
        (if (equal? address (caar addbox))
            (cons (cdar addbox) (cdr addbox))
            (let ((ret (alist-pop (cdr addbox) address)))
              (cons (car ret) (cons (car addbox) (cdr ret))))))))
(define (make-empty-alist) '())
(define alist-size length)
(define alist-empty? null?)
(define add-into-addbox alist-insert)
(define pull-outof-addbox alist-pop)
(define make-addbox make-empty-alist)
(define addbox->alist (lambda (addbox) addbox))
(define alist->addbox (lambda (alist) alist))
(define addbox-size alist-size)
(define addbox-empty? alist-empty?)
(define (make-xrp-draw address value xrp-name proposer-thunk
          ticks score support)
  (list address value xrp-name proposer-thunk ticks score
    support))
(define xrp-draw-address first)
(define xrp-draw-value second)
(define xrp-draw-name third)
(define xrp-draw-proposer fourth)
(define xrp-draw-ticks fifth)
(define xrp-draw-score sixth)
(define xrp-draw-support seventh)
(define (church-make-xrp address store xrp-name sample
          incr-stats decr-stats score init-stats hyperparams
          proposer support)
  (let* ((xrp-name (church-force address store xrp-name))
         (sample (church-force address store sample))
         (incr-stats (church-force address store incr-stats))
         (decr-stats (church-force address store decr-stats))
         (score (church-force address store score))
         (init-stats (church-force address store init-stats))
         (hyperparams
          (church-force address store hyperparams))
         (proposer (church-force address store proposer))
         (support (church-force address store support)))
    (return-with-store store
      (let* ((ret
              (pull-outof-addbox (store->xrp-stats store)
                address))
             (oldstats (car ret))
             (reststatsbox (cdr ret))
             (tick (store->tick store)))
        (if (and (not (eq? #f oldstats))
                 (= tick (second oldstats)))
            store
            (make-store (store->xrp-draws store)
              (add-into-addbox reststatsbox address
                (list init-stats tick))
              (store->score store) tick
              (store->enumeration-flag store))))
      (let* ((xrp-address address)
             (proposer
              (if (null? proposer)
                  (lambda (address store operands old-value)
                    (let* ((dec
                            (decr-stats address store
                              old-value
                              (caar
                                (pull-outof-addbox
                                  (store->xrp-stats store)
                                  xrp-address))
                              hyperparams operands))
                           (decstats (second dec))
                           (decscore (third dec))
                           (inc
                            (sample address store decstats
                              hyperparams operands))
                           (proposal-value (first inc))
                           (incscore (third inc)))
                      (list proposal-value incscore decscore)))
                  proposer)))
        (lambda (address store . args)
          (let* ((tmp
                  (pull-outof-addbox
                    (store->xrp-draws store) address))
                 (old-xrp-draw (car tmp))
                 (rest-xrp-draws (cdr tmp))
                 (old-tick
                  (if (eq? #f old-xrp-draw)
                      '()
                      (first (xrp-draw-ticks old-xrp-draw)))))
            (if (equal? (store->tick store) old-tick)
                (return-with-store store store
                  (xrp-draw-value old-xrp-draw))
                (let* ((tmp
                        (pull-outof-addbox
                          (store->xrp-stats store)
                          xrp-address))
                       (stats (caar tmp))
                       (rest-statsbox (cdr tmp))
                       (support-vals
                        (if (null? support)
                            '()
                            (support address store stats
                              hyperparams args)))
                       (tmp
                        (if (eq? #f old-xrp-draw)
                            (if (store->enumeration-flag
                                  store)
                                (incr-stats address store
                                  (first support-vals) stats
                                  hyperparams args)
                                (sample address store stats
                                  hyperparams args))
                            (incr-stats address store
                              (xrp-draw-value old-xrp-draw)
                              stats hyperparams args)))
                       (value (first tmp))
                       (new-stats
                        (list (second tmp)
                          (store->tick store)))
                       (incr-score (third tmp))
                       (new-xrp-draw
                        (make-xrp-draw address value
                          xrp-name
                          (lambda (address store state)
                            (let ((store
                                   (cons
                                     (first
                                       (mcmc-state->store
                                         state))
                                     (cdr
                                       (mcmc-state->store
                                         state)))))
                              (church-apply
                                (mcmc-state->address state)
                                store proposer
                                (list args value))))
                          (cons (store->tick store) old-tick)
                          incr-score support-vals))
                       (new-store
                        (make-store
                          (add-into-addbox rest-xrp-draws
                            address new-xrp-draw)
                          (add-into-addbox rest-statsbox
                            xrp-address new-stats)
                          (+ (store->score store) incr-score)
                          (store->tick store)
                          (store->enumeration-flag store))))
                  (return-with-store store new-store value)))))))))
(define (make-mcmc-state store value address)
  (list store value address))
(define (add-proc-to-state state proc)
  (append state (list proc)))
(define mcmc-state->store first)
(define mcmc-state->address third)
(define (mcmc-state->xrp-draws state)
  (store->xrp-draws (mcmc-state->store state)))
(define (mcmc-state->score state)
  (if (not (eq? #t (first (second state))))
      -inf.0
      (store->score (mcmc-state->store state))))
(define (mcmc-state->query-value state)
  (let ((store
         (cons (first (mcmc-state->store state))
           (cdr (mcmc-state->store state)))))
    (church-apply (mcmc-state->address state) store
      (cdr (second state)) '())))
(define (church-make-initial-mcmc-state address store)
  (make-mcmc-state (cons (first store) (cdr store))
    'init-val address))
(define (church-make-initial-enumeration-state address store)
  (make-mcmc-state
    (make-store '() (store->xrp-stats store)
      (store->score store) (store->tick store) #t)
    'init-val address))
(define (counterfactual-update state nfqp . interventions)
  (let* ((new-tick
          (+ 1 (store->tick (mcmc-state->store state))))
         (interv-store
          (make-store
            (fold
              (lambda (interv xrps)
                (add-into-addbox
                  (cdr
                    (pull-outof-addbox xrps
                      (xrp-draw-address (first interv))))
                  (xrp-draw-address (first interv))
                  (make-xrp-draw
                    (xrp-draw-address (first interv))
                    (cdr interv)
                    (xrp-draw-name (first interv))
                    (xrp-draw-proposer (first interv))
                    (xrp-draw-ticks (first interv))
                    'dummy-score
                    (xrp-draw-support (first interv)))))
              (store->xrp-draws (mcmc-state->store state))
              interventions)
            (store->xrp-stats (mcmc-state->store state)) 0.0
            new-tick
            (store->enumeration-flag
              (mcmc-state->store state))))
         (ret
          (list
            (church-apply (mcmc-state->address state)
              interv-store nfqp '())
            interv-store))
         (value (first ret))
         (new-store (second ret))
         (ret2
          (if (store->enumeration-flag new-store)
              (list new-store 0)
              (clean-store new-store)))
         (new-store (first ret2))
         (cd-bw/fw (second ret2))
         (proposal-state
          (make-mcmc-state new-store value
            (mcmc-state->address state))))
    (list proposal-state cd-bw/fw)))
(define (clean-store store)
  (let* ((state-tick (store->tick store))
         (draws-bw/fw
          (let loop ((draws
                      (addbox->alist
                        (store->xrp-draws store)))
                     (used-draws '())
                     (bw/fw 0.0))
            (if (null? draws)
                (list used-draws bw/fw)
                (if (= (first (xrp-draw-ticks (cdar draws)))
                      state-tick)
                    (if (null?
                          (cdr (xrp-draw-ticks (cdar draws))))
                        (loop (cdr draws)
                          (cons (car draws) used-draws)
                          (- bw/fw
                            (xrp-draw-score (cdar draws))))
                        (loop (cdr draws)
                          (cons (car draws) used-draws)
                          bw/fw))
                    (loop (cdr draws) used-draws
                      (+ bw/fw (xrp-draw-score (cdar draws)))))))))
    (list
      (make-store (alist->addbox (first draws-bw/fw))
        (store->xrp-stats store) (store->score store)
        (store->tick store) (store->enumeration-flag store))
      (second draws-bw/fw))))
(define (church-main address store)
  (letrec ((church-sample
            (lambda (address store church-thunk)
              (church-thunk (cons 'a1 address) store)))
           (church-uniform-draw
            (lambda (address store church-lst)
              (if (church-null? (cons 'a2 address) store
                    church-lst)
                  '()
                  (church-list-ref (cons 'a3 address) store
                    church-lst
                    (church-sample-integer
                      (cons 'a4 address) store
                      (church-length (cons 'a5 address)
                        store church-lst))))))
           (church-all
            (lambda (address store church-lst)
              (church-apply (cons 'a6 address) store
                church-and church-lst)))
           (church-any
            (lambda (address store church-lst)
              (church-apply (cons 'a7 address) store
                church-or church-lst)))
           (church-product
            (lambda (address store church-lst)
              (church-apply (cons 'a8 address) store
                church-* church-lst)))
           (church-sum
            (lambda (address store church-lst)
              (church-apply (cons 'a9 address) store
                church-+ church-lst)))
           (church-repeat
            (lambda (address store church-N church-proc)
              (if (church-= (cons 'a10 address) store
                    church-N 0)
                  '()
                  (church-pair (cons 'a11 address) store
                    (church-proc (cons 'a12 address) store)
                    (church-repeat (cons 'a13 address) store
                      (church-- (cons 'a14 address) store
                        church-N 1)
                      church-proc)))))
           (church-for-each
            (lambda (address store church-proc church-lst)
              (if (church-null? (cons 'a15 address) store
                    church-lst)
                  '()
                  (begin
                    (church-proc (cons 'a16 address) store
                      (church-first (cons 'a17 address)
                        store church-lst))
                    (church-for-each (cons 'a18 address)
                      store church-proc
                      (church-rest (cons 'a19 address) store
                        church-lst))))))
           (church-fold
            (lambda
              (address store church-proc church-init
               . church-lsts)
              (if (church-null? (cons 'a20 address) store
                    (church-first (cons 'a21 address) store
                      church-lsts))
                  church-init
                  (church-apply (cons 'a22 address) store
                    church-fold
                    (church-pair (cons 'a23 address) store
                      church-proc
                      (church-pair (cons 'a24 address) store
                        (church-apply (cons 'a25 address)
                          store church-proc
                          (church-append (cons 'a26 address)
                            store
                            (church-single-map
                              (cons 'a27 address) store
                              church-first church-lsts)
                            (church-list (cons 'a28 address)
                              store church-init)))
                        (church-single-map
                          (cons 'a29 address) store
                          church-rest church-lsts)))))))
           (church-reverse
            (lambda (address store church-lst)
              (church-fold (cons 'a30 address) store
                church-pair '() church-lst)))
           (church-map
            (lambda
              (address store church-proc . church-lsts)
              (if (church-null? (cons 'a31 address) store
                    (church-rest (cons 'a32 address) store
                      church-lsts))
                  (church-single-map (cons 'a33 address)
                    store church-proc
                    (church-first (cons 'a34 address) store
                      church-lsts))
                  (church-multi-map (cons 'a35 address)
                    store church-proc church-lsts))))
           (church-single-map
            (lambda (address store church-proc church-lst)
              (if (church-null? (cons 'a36 address) store
                    church-lst)
                  '()
                  (church-pair (cons 'a37 address) store
                    (church-proc (cons 'a38 address) store
                      (church-first (cons 'a39 address)
                        store church-lst))
                    (church-map (cons 'a40 address) store
                      church-proc
                      (church-rest (cons 'a41 address) store
                        church-lst))))))
           (church-multi-map
            (lambda (address store church-proc church-lsts)
              (if (church-null? (cons 'a42 address) store
                    (church-first (cons 'a43 address) store
                      church-lsts))
                  '()
                  (church-pair (cons 'a44 address) store
                    (church-apply (cons 'a45 address) store
                      church-proc
                      (church-single-map (cons 'a46 address)
                        store church-first church-lsts))
                    (church-multi-map (cons 'a47 address)
                      store church-proc
                      (church-single-map (cons 'a48 address)
                        store church-rest church-lsts))))))
           (church-many-map
            (lambda
              (address store church-proc . church-lsts)
              (church-multi-map (cons 'a49 address) store
                church-proc church-lsts)))
           (church-filter
            (lambda (address store church-pred church-lst)
              (if (church-null? (cons 'a50 address) store
                    church-lst)
                  '()
                  (if (church-pred (cons 'a51 address) store
                        (church-first (cons 'a52 address)
                          store church-lst))
                      (church-pair (cons 'a53 address) store
                        (church-first (cons 'a54 address)
                          store church-lst)
                        (church-filter (cons 'a55 address)
                          store church-pred
                          (church-rest (cons 'a56 address)
                            store church-lst)))
                      (church-filter (cons 'a57 address)
                        store church-pred
                        (church-rest (cons 'a58 address)
                          store church-lst))))))
           (church-list-index
            (lambda
              (address store church-pred church-lst
               . church-i)
              (if (church-null? (cons 'a59 address) store
                    church-lst)
                  church-false
                  ((lambda (address store church-i)
                     (if (church-pred (cons 'a60 address)
                           store
                           (church-first (cons 'a61 address)
                             store church-lst))
                         church-i
                         (church-list-index
                           (cons 'a62 address) store
                           church-pred
                           (church-rest (cons 'a63 address)
                             store church-lst)
                           (church-+ (cons 'a64 address)
                             store church-i 1))))
                    (cons 'a65 address) store
                    (if (church-null? (cons 'a66 address)
                          store church-i)
                        0
                        (church-first (cons 'a67 address)
                          store church-i))))))
           (church-zip
            (lambda (address store . church-lists)
              (church-multi-map (cons 'a68 address) store
                church-list church-lists)))
           (church-rejection-query
            (lambda (address store church-nfqp)
              ((lambda (address store church-val)
                 (if (church-first (cons 'a69 address) store
                       church-val)
                     ((church-rest (cons 'a70 address) store
                        church-val)
                       (cons 'a71 address) store)
                     (church-rejection-query
                       (cons 'a72 address) store church-nfqp)))
                (cons 'a73 address) store
                (church-nfqp (cons 'a74 address) store))))
           (church-discrete church-sample-discrete)
           (church-multinomial
            (lambda (address store church-vals church-probs)
              (church-list-ref (cons 'a75 address) store
                church-vals
                (church-discrete (cons 'a76 address) store
                  church-probs))))
           (church-beta
            (lambda (address store church-a church-b)
              (church-dirichlet (cons 'a77 address) store
                (church-list (cons 'a78 address) store
                  church-a church-b))))
           (church-make-GEM
            (lambda (address store church-alpha)
              ((lambda (address store church-sticks)
                 (lambda (address store)
                   (church-pick-a-stick (cons 'a79 address)
                     store church-sticks 1)))
                (cons 'a80 address) store
                ((lambda (mem-address store proc)
                   (lambda (address store . args)
                     (church-apply (cons args mem-address)
                       store proc args)))
                  address store
                  (lambda (address store church-x)
                    (church-beta (cons 'a81 address) store
                      1.0 church-alpha))))))
           (church-pick-a-stick
            (lambda (address store church-sticks church-J)
              (if (church-flip (cons 'a82 address) store
                    (church-sticks (cons 'a83 address) store
                      church-J))
                  church-J
                  (church-pick-a-stick (cons 'a84 address)
                    store church-sticks
                    (church-+ (cons 'a85 address) store
                      church-J 1)))))
           (church-sticky-DPmem
            (lambda (address store church-alpha church-proc)
              ((lambda
                 (address store church-augmented-proc
                   church-crps)
                 (lambda (address store . church-argsin)
                   (church-augmented-proc
                     (cons 'a86 address) store church-argsin
                     ((church-crps (cons 'a87 address) store
                        church-argsin)
                       (cons 'a88 address) store))))
                (cons 'a89 address) store
                ((lambda (mem-address store proc)
                   (lambda (address store . args)
                     (church-apply (cons args mem-address)
                       store proc args)))
                  address store
                  (lambda
                    (address store church-args church-part)
                    (church-apply (cons 'a90 address) store
                      church-proc church-args)))
                ((lambda (mem-address store proc)
                   (lambda (address store . args)
                     (church-apply (cons args mem-address)
                       store proc args)))
                  address store
                  (lambda (address store church-args)
                    (church-make-GEM (cons 'a91 address)
                      store church-alpha))))))
           (church-make-PYP
            (lambda (address store church-a church-b)
              ((lambda (address store church-sticks)
                 (lambda (address store)
                   (church-pick-a-stick (cons 'a92 address)
                     store church-sticks 1)))
                (cons 'a93 address) store
                ((lambda (mem-address store proc)
                   (lambda (address store . args)
                     (church-apply (cons args mem-address)
                       store proc args)))
                  address store
                  (lambda (address store church-x)
                    (church-beta (cons 'a94 address) store
                      (church-- (cons 'a95 address) store
                        1.0 church-a)
                      (church-+ (cons 'a96 address) store
                        church-b
                        (church-* (cons 'a97 address) store
                          church-a church-x))))))))
           (church-PYmem
            (lambda
              (address store church-a church-b church-proc)
              ((lambda
                 (address store church-augmented-proc
                   church-crps)
                 (lambda (address store . church-argsin)
                   (church-augmented-proc
                     (cons 'a98 address) store church-argsin
                     ((church-crps (cons 'a99 address) store
                        church-argsin)
                       (cons 'a100 address) store))))
                (cons 'a101 address) store
                ((lambda (mem-address store proc)
                   (lambda (address store . args)
                     (church-apply (cons args mem-address)
                       store proc args)))
                  address store
                  (lambda
                    (address store church-args church-part)
                    (church-apply (cons 'a102 address) store
                      church-proc church-args)))
                ((lambda (mem-address store proc)
                   (lambda (address store . args)
                     (church-apply (cons args mem-address)
                       store proc args)))
                  address store
                  (lambda (address store church-args)
                    (church-make-PYP (cons 'a103 address)
                      store church-a church-b))))))
           (church-noisy-lev-list-equal?
            (lambda
              (address store church-list1 church-list2
                church-coupling)
              (if (church-= (cons 'a104 address) store 0.0
                    church-coupling)
                  (church-equal? (cons 'a105 address) store
                    church-list1 church-list2)
                  (church-flip (cons 'a106 address) store
                    (church-expt (cons 'a107 address) store
                      church-coupling
                      (church-lev-dist (cons 'a108 address)
                        store church-list1 church-list2))))))
           (church-flatten
            (lambda (address store church-lst)
              (if (church-null? (cons 'a109 address) store
                    church-lst)
                  '()
                  (if (church-list? (cons 'a110 address)
                        store
                        (church-first (cons 'a111 address)
                          store church-lst))
                      (church-append (cons 'a112 address)
                        store
                        (church-flatten (cons 'a113 address)
                          store
                          (church-first (cons 'a114 address)
                            store church-lst))
                        (church-flatten (cons 'a115 address)
                          store
                          (church-rest (cons 'a116 address)
                            store church-lst)))
                      (church-pair (cons 'a117 address)
                        store
                        (church-first (cons 'a118 address)
                          store church-lst)
                        (church-flatten (cons 'a119 address)
                          store
                          (church-rest (cons 'a120 address)
                            store church-lst)))))))
           (church-depth
            (lambda (address store church-tree)
              (if (church-not (cons 'a121 address) store
                    (church-list? (cons 'a122 address) store
                      church-tree))
                  0
                  (church-+ (cons 'a123 address) store 1
                    (church-apply (cons 'a124 address) store
                      church-max
                      (church-map (cons 'a125 address) store
                        church-depth church-tree))))))
           (church-mean
            (lambda (address store church-lst)
              (church-/ (cons 'a126 address) store
                (church-apply (cons 'a127 address) store
                  church-+ church-lst)
                (church-length (cons 'a128 address) store
                  church-lst))))
           (church-variance
            (lambda (address store church-lst)
              ((lambda (address store church-mn)
                 (church-mean (cons 'a129 address) store
                   (church-map (cons 'a130 address) store
                     (lambda (address store church-x)
                       (church-expt (cons 'a131 address)
                         store
                         (church-- (cons 'a132 address)
                           store church-x church-mn)
                         2))
                     church-lst)))
                (cons 'a133 address) store
                (church-mean (cons 'a134 address) store
                  church-lst))))
           (church-no-proposals
            (lambda (address store church-x)
              (begin
                (church-display (cons 'a135 address) store
                  "warning: no-proposals not implemented.\n")
                church-x)))
           (church-range
            (lambda (address store church-a church-b)
              (if (church-= (cons 'a136 address) store
                    church-a church-b)
                  (church-list (cons 'a137 address) store
                    church-b)
                  (church-pair (cons 'a138 address) store
                    church-a
                    (church-range (cons 'a139 address) store
                      (church-+ (cons 'a140 address) store
                        church-a 1)
                      church-b)))))
           (church-make-proposer
            (lambda (address store church-scorer)
              (lambda (address store church-old-state)
                ((lambda (address store church-program)
                   ((lambda (address store church-ret1)
                      ((lambda
                         (address store church-new-program)
                         ((lambda
                            (address store
                              church-abstraction-fw-prob)
                            ((lambda
                               (address store
                                 church-abstraction-bw-prob)
                               ((lambda
                                  (address store
                                    church-new-program-score)
                                  ((lambda
                                     (address store
                                       church-old-program-score)
                                     (church-list
                                       (cons 'a141 address)
                                       store
                                       church-new-program
                                       (church-+
                                         (cons 'a142 address)
                                         store
                                         church-abstraction-fw-prob
                                         church-new-program-score)
                                       (church-+
                                         (cons 'a143 address)
                                         store
                                         church-abstraction-bw-prob
                                         church-old-program-score)))
                                    (cons 'a144 address)
                                    store
                                    (church-scorer
                                      (cons 'a145 address)
                                      store '()
                                      church-program)))
                                 (cons 'a146 address) store
                                 (church-scorer
                                   (cons 'a147 address)
                                   store '()
                                   church-new-program)))
                              (cons 'a148 address) store
                              (church-third
                                (cons 'a149 address) store
                                church-ret1)))
                           (cons 'a150 address) store
                           (church-second
                             (cons 'a151 address) store
                             church-ret1)))
                        (cons 'a152 address) store
                        (church-first (cons 'a153 address)
                          store church-ret1)))
                     (cons 'a154 address) store
                     (church-test-abstraction-proposer
                       (cons 'a155 address) store
                       church-program)))
                  (cons 'a156 address) store
                  (church-mcmc-state->query-value
                    (cons 'a157 address) store
                    church-old-state)))))
           (church-make-scorer
            (lambda (address store church-grammar)
              (lambda
                (address store church-args church-program)
                ((lambda (address store church-lazy-program)
                   ((lambda
                      (address store church-temps-nfqp)
                      ((lambda (address store church-depths)
                         ((lambda
                            (address store church-states)
                            ((lambda
                               (address store church-score)
                               church-score)
                              (cons 'a158 address) store
                              (church-mcmc-state->score
                                (cons 'a159 address) store
                                (church-uniform-draw
                                  (cons 'a160 address) store
                                  church-states))))
                           (cons 'a161 address) store
                           (church-smc-core
                             (cons 'a162 address) store
                             church-depths 10 20
                             church-temps-nfqp)))
                        (cons 'a163 address) store
                        (church-map (cons 'a164 address)
                          store church-list
                          (church-iota (cons 'a165 address)
                            store 20))))
                     (cons 'a166 address) store
                     (church-make-temps-nfqp
                       (cons 'a167 address) store
                       church-lazy-program church-grammar)))
                  (cons 'a168 address) store
                  (church-list->lazy-list
                    (cons 'a169 address) store
                    church-program)))))
           (church-make-temps-nfqp
            (lambda
              (address store church-target-program
                church-grammar)
              (lambda (address store church-temp)
                (lambda (address store)
                  (letrec ((church-p
                            (church-grammar
                              (cons 'a170 address) store)))
                    (church-pair (cons 'a171 address) store
                      (church-lazy-equal?
                        (cons 'a172 address) store church-p
                        church-target-program church-temp)
                      (lambda (address store)
                        (church-first (cons 'a173 address)
                          store
                          (church-lazy-list->list
                            (cons 'a174 address) store
                            church-p church-temp)))))))))
           (church-make-stateless-xrp
            (lambda
              (address store church-xrp-name church-sampler
               church-scorer . church-proposal-support)
              (church-make-xrp (cons 'a175 address) store
                church-xrp-name
                (lambda
                  (address store church-stats
                    church-hyperparams church-args)
                  ((lambda (address store church-value)
                     (church-list (cons 'a176 address) store
                       church-value church-stats
                       (church-scorer (cons 'a177 address)
                         store church-args church-value)))
                    (cons 'a178 address) store
                    (church-apply (cons 'a179 address) store
                      church-sampler church-args)))
                (lambda
                  (address store church-value church-stats
                    church-hyperparams church-args)
                  (church-list (cons 'a180 address) store
                    church-value church-stats
                    (church-scorer (cons 'a181 address)
                      store church-args church-value)))
                (lambda
                  (address store church-value church-stats
                    church-hyperparams church-args)
                  (church-list (cons 'a182 address) store
                    church-value church-stats
                    (church-scorer (cons 'a183 address)
                      store church-args church-value)))
                'scorer '() '()
                (if (church-null? (cons 'a184 address) store
                      church-proposal-support)
                    '()
                    (church-first (cons 'a185 address) store
                      church-proposal-support))
                (if (church-null? (cons 'a186 address) store
                      church-proposal-support)
                    '()
                    (if (church-null? (cons 'a187 address)
                          store
                          (church-rest (cons 'a188 address)
                            store church-proposal-support))
                        '()
                        ((lambda (address store church-pr)
                           (lambda
                             (address store church-stats
                               church-hyperparams
                               church-args)
                             (church-pr (cons 'a189 address)
                               store church-args)))
                          (cons 'a190 address) store
                          (church-second
                            (cons 'a191 address) store
                            church-proposal-support)))))))
           (church-flip
            (church-make-stateless-xrp (cons 'a192 address)
              store 'flip
              (lambda (address store . church-w)
                (if (church-null? (cons 'a193 address) store
                      church-w)
                    (church-< (cons 'a194 address) store
                      (church-random-real
                        (cons 'a195 address) store)
                      0.5)
                    (church-< (cons 'a196 address) store
                      (church-random-real
                        (cons 'a197 address) store)
                      (church-car (cons 'a198 address) store
                        church-w))))
              (lambda (address store church-args church-val)
                (if (church-null? (cons 'a199 address) store
                      church-args)
                    (church-- (cons 'a200 address) store
                      (church-log (cons 'a201 address) store
                        2.0))
                    (if church-val
                        (church-log (cons 'a202 address)
                          store
                          (church-first (cons 'a203 address)
                            store church-args))
                        (church-log (cons 'a204 address)
                          store
                          (church-- (cons 'a205 address)
                            store 1
                            (church-first
                              (cons 'a206 address) store
                              church-args))))))
              '()
              (lambda (address store church-args)
                (church-list (cons 'a207 address) store
                  church-true church-false))))
           (church-log-flip
            (church-make-stateless-xrp (cons 'a208 address)
              store 'log-flip
              (lambda (address store . church-w)
                (if (church-null? (cons 'a209 address) store
                      church-w)
                    (church-< (cons 'a210 address) store
                      (church-random-real
                        (cons 'a211 address) store)
                      0.5)
                    (church-< (cons 'a212 address) store
                      (church-log (cons 'a213 address) store
                        (church-random-real
                          (cons 'a214 address) store))
                      (church-car (cons 'a215 address) store
                        church-w))))
              (lambda (address store church-args church-val)
                (if (church-null? (cons 'a216 address) store
                      church-args)
                    (church-- (cons 'a217 address) store
                      (church-log (cons 'a218 address) store
                        2.0))
                    (if church-val
                        (church-first (cons 'a219 address)
                          store church-args)
                        (church-log (cons 'a220 address)
                          store
                          (church-- (cons 'a221 address)
                            store 1.0
                            (church-exp (cons 'a222 address)
                              store
                              (church-first
                                (cons 'a223 address) store
                                church-args)))))))))
           (church-dirichlet
            (church-make-stateless-xrp (cons 'a224 address)
              store 'dirichlet church-sample-dirichlet
              (lambda (address store church-args church-val)
                (church-dirichlet-lnpdf (cons 'a225 address)
                  store
                  (church-first (cons 'a226 address) store
                    church-args)
                  church-val))))
           (church-sample-discrete
            (church-make-stateless-xrp (cons 'a227 address)
              store 'sample-discrete church-discrete-sampler
              (lambda (address store church-args church-val)
                (if (church->= (cons 'a228 address) store
                      church-val
                      (church-length (cons 'a229 address)
                        store
                        (church-first (cons 'a230 address)
                          store church-args)))
                    -inf.0
                    ((lambda (address store church-p)
                       (if (church-> (cons 'a231 address)
                             store church-p 0)
                           (church-log (cons 'a232 address)
                             store church-p)
                           -inf.0))
                      (cons 'a233 address) store
                      (church-discrete-pdf
                        (cons 'a234 address) store
                        (church-first (cons 'a235 address)
                          store church-args)
                        church-val))))
              '()
              (lambda (address store church-args)
                (church-iota (cons 'a236 address) store
                  (church-length (cons 'a237 address) store
                    (church-first (cons 'a238 address) store
                      church-args))))))
           (church-sample-integer
            (church-make-stateless-xrp (cons 'a239 address)
              store 'sample-integer church-random-integer
              (lambda (address store church-args church-val)
                ((lambda (address store church-n)
                   (if (church-and (cons 'a240 address)
                         store
                         (church-integer?
                           (cons 'a241 address) store
                           church-val)
                         (church->= (cons 'a242 address)
                           store church-val 0)
                         (church-< (cons 'a243 address)
                           store church-val church-n))
                       (church-- (cons 'a244 address) store
                         (church-log (cons 'a245 address)
                           store church-n))
                       -inf.0))
                  (cons 'a246 address) store
                  (church-first (cons 'a247 address) store
                    church-args)))
              '()
              (lambda (address store church-args)
                (church-iota (cons 'a248 address) store
                  (church-first (cons 'a249 address) store
                    church-args)))))
           (church-uniform
            (church-make-stateless-xrp (cons 'a250 address)
              store 'uniform
              (lambda (address store church-a church-b)
                (church-+ (cons 'a251 address) store
                  (church-* (cons 'a252 address) store
                    (church-- (cons 'a253 address) store
                      church-b church-a)
                    (church-random-real (cons 'a254 address)
                      store))
                  church-a))
              (lambda (address store church-args church-val)
                ((lambda (address store church-a church-b)
                   (if (church-or (cons 'a255 address) store
                         (church-< (cons 'a256 address)
                           store church-val church-a)
                         (church-> (cons 'a257 address)
                           store church-val church-b))
                       -inf.0
                       (church-- (cons 'a258 address) store
                         (church-log (cons 'a259 address)
                           store
                           (church-- (cons 'a260 address)
                             store church-b church-a)))))
                  (cons 'a261 address) store
                  (church-first (cons 'a262 address) store
                    church-args)
                  (church-second (cons 'a263 address) store
                    church-args)))))
           (church-exponential
            (church-make-stateless-xrp (cons 'a264 address)
              store 'exponential
              (lambda (address store church-inv-mean)
                (church-- (cons 'a265 address) store
                  (church-/ (cons 'a266 address) store
                    (church-log (cons 'a267 address) store
                      (church-random-real
                        (cons 'a268 address) store))
                    church-inv-mean)))
              (lambda (address store church-args church-val)
                (if (church-< (cons 'a269 address) store
                      church-val 0)
                    -inf.0
                    ((lambda (address store church-inv-mean)
                       (church-+ (cons 'a270 address) store
                         (church-log (cons 'a271 address)
                           store church-inv-mean)
                         (church-- (cons 'a272 address)
                           store
                           (church-* (cons 'a273 address)
                             store church-inv-mean
                             church-val))))
                      (cons 'a274 address) store
                      (church-first (cons 'a275 address)
                        store church-args))))))
           (church-gaussian
            (church-make-stateless-xrp (cons 'a276 address)
              store 'gaussian
              (lambda (address store . church-args)
                (church-sample-gaussian (cons 'a277 address)
                  store
                  (church-first (cons 'a278 address) store
                    church-args)
                  (church-second (cons 'a279 address) store
                    church-args)))
              (lambda (address store church-args church-val)
                (church-gaussian-lnpdf (cons 'a280 address)
                  store church-val
                  (church-first (cons 'a281 address) store
                    church-args)
                  (church-second (cons 'a282 address) store
                    church-args)))))
           (church-gensym
            (church-make-stateless-xrp (cons 'a283 address)
              store 'gensym
              (lambda (address store . church-prefix)
                (church-apply (cons 'a284 address) store
                  church-scheme-gensym church-prefix))
              (lambda (address store church-args church-val)
                (church-log (cons 'a285 address) store 0.9))))
           (church-random-permutation
            (church-make-stateless-xrp (cons 'a286 address)
              store 'random-permutation
              (lambda (address store church-len)
                (letrec ((church-loop
                          (lambda
                            (address store church-perm
                              church-n)
                            (if (church-=
                                  (cons 'a287 address) store
                                  church-n 0)
                                church-perm
                                ((lambda
                                   (address store church-k)
                                   (church-loop
                                     (cons 'a288 address)
                                     store
                                     (church-put
                                       (cons 'a289 address)
                                       store
                                       (church-put
                                         (cons 'a290 address)
                                         store church-perm
                                         church-k
                                         (church-list-ref
                                           (cons 'a291
                                             address)
                                           store church-perm
                                           church-n))
                                       church-n
                                       (church-list-ref
                                         (cons 'a292 address)
                                         store church-perm
                                         church-k))
                                     (church--
                                       (cons 'a293 address)
                                       store church-n 1)))
                                  (cons 'a294 address) store
                                  (church-sample-integer
                                    (cons 'a295 address)
                                    store
                                    (church-+
                                      (cons 'a296 address)
                                      store church-n 1)))))))
                  (church-loop (cons 'a297 address) store
                    (church-iota (cons 'a298 address) store
                      church-len)
                    (church-- (cons 'a299 address) store
                      church-len 1))))
              (lambda (address store church-args church-val)
                ((lambda (address store church-len)
                   (if (church-= (cons 'a300 address) store
                         church-len
                         (church-length (cons 'a301 address)
                           store church-val))
                       (church-- (cons 'a302 address) store
                         (church-lnfact (cons 'a303 address)
                           store church-len))
                       (church-log (cons 'a304 address)
                         store 0)))
                  (cons 'a305 address) store
                  (church-first (cons 'a306 address) store
                    church-args)))))
           (church-put
            (lambda
              (address store church-lst church-ind
                church-elt)
              (church-append (cons 'a307 address) store
                (church-take (cons 'a308 address) store
                  church-lst church-ind)
                (church-list (cons 'a309 address) store
                  church-elt)
                (church-drop (cons 'a310 address) store
                  church-lst
                  (church-+ (cons 'a311 address) store 1
                    church-ind)))))
           (church-permute
            (lambda (address store church-lst)
              (church-map (cons 'a312 address) store
                (lambda (address store church-ind)
                  (church-list-ref (cons 'a313 address)
                    store church-lst church-ind))
                (church-random-permutation
                  (cons 'a314 address) store
                  (church-length (cons 'a315 address) store
                    church-lst)))))
           (church-make-dirichlet-discrete
            (lambda (address store church-hyp)
              (church-make-xrp (cons 'a316 address) store
                'dirichlet-discrete
                (lambda
                  (address store church-stats
                    church-hyperparams church-args)
                  ((lambda (address store church-counts)
                     ((lambda
                        (address store church-total-counts)
                        ((lambda
                           (address store church-probs)
                           ((lambda
                              (address store church-value)
                              ((lambda
                                 (address store
                                   church-new-stats)
                                 (church-list
                                   (cons 'a317 address)
                                   store church-value
                                   church-new-stats
                                   (church-log
                                     (cons 'a318 address)
                                     store
                                     (church-list-ref
                                       (cons 'a319 address)
                                       store church-probs
                                       church-value))))
                                (cons 'a320 address) store
                                (church-append
                                  (cons 'a321 address) store
                                  (church-take
                                    (cons 'a322 address)
                                    store church-stats
                                    church-value)
                                  (church-list
                                    (cons 'a323 address)
                                    store
                                    (church-+
                                      (cons 'a324 address)
                                      store 1
                                      (church-list-ref
                                        (cons 'a325 address)
                                        store church-stats
                                        church-value)))
                                  (church-drop
                                    (cons 'a326 address)
                                    store church-stats
                                    (church-+
                                      (cons 'a327 address)
                                      store 1 church-value)))))
                             (cons 'a328 address) store
                             (church-sample-discrete
                               (cons 'a329 address) store
                               church-probs)))
                          (cons 'a330 address) store
                          (church-map (cons 'a331 address)
                            store
                            (lambda (address store church-c)
                              (church-/ (cons 'a332 address)
                                store church-c
                                church-total-counts))
                            church-counts)))
                       (cons 'a333 address) store
                       (church-apply (cons 'a334 address)
                         store church-+ church-counts)))
                    (cons 'a335 address) store
                    (church-map (cons 'a336 address) store
                      church-+ church-stats
                      church-hyperparams)))
                (lambda
                  (address store church-value church-stats
                    church-hyperparams church-args)
                  (church-list (cons 'a337 address) store
                    church-value
                    (church-append (cons 'a338 address)
                      store
                      (church-take (cons 'a339 address)
                        store church-stats church-value)
                      (church-list (cons 'a340 address)
                        store
                        (church-+ (cons 'a341 address) store
                          (church-list-ref
                            (cons 'a342 address) store
                            church-stats church-value)
                          1))
                      (church-drop (cons 'a343 address)
                        store church-stats
                        (church-+ (cons 'a344 address) store
                          1 church-value)))
                    (church-- (cons 'a345 address) store
                      (church-log (cons 'a346 address) store
                        (church-+ (cons 'a347 address) store
                          (church-list-ref
                            (cons 'a348 address) store
                            church-stats church-value)
                          (church-list-ref
                            (cons 'a349 address) store
                            church-hyperparams church-value)))
                      (church-log (cons 'a350 address) store
                        (church-+ (cons 'a351 address) store
                          (church-apply (cons 'a352 address)
                            store church-+ church-stats)
                          (church-apply (cons 'a353 address)
                            store church-+
                            church-hyperparams))))))
                (lambda
                  (address store church-value church-stats
                    church-hyperparams church-args)
                  (church-list (cons 'a354 address) store
                    church-value
                    (church-append (cons 'a355 address)
                      store
                      (church-take (cons 'a356 address)
                        store church-stats church-value)
                      (church-list (cons 'a357 address)
                        store
                        (church-- (cons 'a358 address) store
                          (church-list-ref
                            (cons 'a359 address) store
                            church-stats church-value)
                          1))
                      (church-drop (cons 'a360 address)
                        store church-stats
                        (church-+ (cons 'a361 address) store
                          1 church-value)))
                    (church-+ (cons 'a362 address) store
                      (church-log (cons 'a363 address) store
                        (church-+ (cons 'a364 address) store
                          -1
                          (church-list-ref
                            (cons 'a365 address) store
                            church-stats church-value)
                          (church-list-ref
                            (cons 'a366 address) store
                            church-hyperparams church-value)))
                      (church-- (cons 'a367 address) store
                        (church-log (cons 'a368 address)
                          store
                          (church-+ (cons 'a369 address)
                            store -1
                            (church-apply
                              (cons 'a370 address) store
                              church-+ church-stats)
                            (church-apply
                              (cons 'a371 address) store
                              church-+ church-hyperparams)))))))
                'dirichlet-discrete-scorer
                (church-make-list (cons 'a372 address) store
                  (church-length (cons 'a373 address) store
                    church-hyp)
                  0.0)
                church-hyp '()
                (lambda
                  (address store church-stats
                    church-hyperparams church-args)
                  (church-iota (cons 'a374 address) store
                    (church-length (cons 'a375 address)
                      store church-hyperparams))))))
           (church-make-beta-binomial
            (lambda (address store church-alpha church-beta)
              ((lambda (address store church-dd)
                 (lambda (address store)
                   (church-= (cons 'a376 address) store
                     (church-dd (cons 'a377 address) store)
                     1)))
                (cons 'a378 address) store
                (church-make-dirichlet-discrete
                  (cons 'a379 address) store
                  (church-list (cons 'a380 address) store
                    church-alpha church-beta)))))
           (church-make-symmetric-dirichlet-discrete
            (lambda (address store church-N church-hyp)
              (church-make-dirichlet-discrete
                (cons 'a381 address) store
                (church-make-list (cons 'a382 address) store
                  church-N church-hyp))))
           (church-make-CRP
            (lambda (address store church-alpha)
              (church-make-xrp (cons 'a383 address) store
                'CRP
                (lambda
                  (address store church-stats
                    church-hyperparam church-args)
                  ((lambda (address store church-count-map)
                     ((lambda (address store church-counts)
                        ((lambda
                           (address store
                             church-total-counts)
                           ((lambda
                              (address store church-probs)
                              ((lambda
                                 (address store
                                   church-table-index)
                                 (if (church-=
                                       (cons 'a384 address)
                                       store
                                       church-table-index 0)
                                     ((lambda
                                        (address store
                                          church-table-symbol)
                                        ((lambda
                                           (address store
                                             church-new-count-map)
                                           (church-list
                                             (cons 'a385
                                               address)
                                             store
                                             church-table-symbol
                                             church-new-count-map
                                             (church-list-ref
                                               (cons 'a386
                                                 address)
                                               store
                                               church-probs
                                               church-table-index)))
                                          (cons 'a387
                                            address)
                                          store
                                          (church-pair
                                            (cons 'a388
                                              address)
                                            store
                                            (church-pair
                                              (cons 'a389
                                                address)
                                              store
                                              church-table-symbol
                                              1)
                                            church-count-map)))
                                       (cons 'a390 address)
                                       store
                                       (church-gensym
                                         (cons 'a391 address)
                                         store))
                                     ((lambda
                                        (address store
                                          church-table-symbol)
                                        ((lambda
                                           (address store
                                             church-table-count)
                                           ((lambda
                                              (address store
                                                church-new-count-map)
                                              (church-list
                                                (cons 'a392
                                                  address)
                                                store
                                                church-table-symbol
                                                church-new-count-map
                                                (church-list-ref
                                                  (cons
                                                    'a393
                                                    address)
                                                  store
                                                  church-probs
                                                  church-table-index)))
                                             (cons 'a394
                                               address)
                                             store
                                             (church-append
                                               (cons 'a395
                                                 address)
                                               store
                                               (church-take
                                                 (cons 'a396
                                                   address)
                                                 store
                                                 church-count-map
                                                 (church--
                                                   (cons
                                                     'a397
                                                     address)
                                                   store
                                                   church-table-index
                                                   1))
                                               (church-list
                                                 (cons 'a398
                                                   address)
                                                 store
                                                 (church-pair
                                                   (cons
                                                     'a399
                                                     address)
                                                   store
                                                   church-table-symbol
                                                   church-table-count))
                                               (church-drop
                                                 (cons 'a400
                                                   address)
                                                 store
                                                 church-count-map
                                                 church-table-index))))
                                          (cons 'a401
                                            address)
                                          store
                                          (church-+
                                            (cons 'a402
                                              address)
                                            store 1
                                            (church-rest
                                              (cons 'a403
                                                address)
                                              store
                                              (church-list-ref
                                                (cons 'a404
                                                  address)
                                                store
                                                church-count-map
                                                (church--
                                                  (cons
                                                    'a405
                                                    address)
                                                  store
                                                  church-table-index
                                                  1))))))
                                       (cons 'a406 address)
                                       store
                                       (church-first
                                         (cons 'a407 address)
                                         store
                                         (church-list-ref
                                           (cons 'a408
                                             address)
                                           store
                                           church-count-map
                                           (church--
                                             (cons 'a409
                                               address)
                                             store
                                             church-table-index
                                             1))))))
                                (cons 'a410 address) store
                                (church-sample-discrete
                                  (cons 'a411 address) store
                                  church-probs)))
                             (cons 'a412 address) store
                             (church-map
                               (cons 'a413 address) store
                               (lambda
                                 (address store church-c)
                                 (church-/
                                   (cons 'a414 address)
                                   store church-c
                                   church-total-counts))
                               church-counts)))
                          (cons 'a415 address) store
                          (church-apply (cons 'a416 address)
                            store church-+ church-counts)))
                       (cons 'a417 address) store
                       (church-pair (cons 'a418 address)
                         store church-hyperparam
                         (church-map (cons 'a419 address)
                           store church-rest
                           church-count-map))))
                    (cons 'a420 address) store church-stats))
                (lambda
                  (address store church-value church-stats
                    church-hyperparam church-args)
                  ((lambda (address store church-count-map)
                     ((lambda (address store church-counts)
                        ((lambda
                           (address store
                             church-total-counts)
                           ((lambda
                              (address store church-probs)
                              ((lambda
                                 (address store
                                   church-table-index)
                                 (if (church-eq?
                                       (cons 'a421 address)
                                       store church-false
                                       church-table-index)
                                     (church-list
                                       (cons 'a422 address)
                                       store church-value
                                       (church-pair
                                         (cons 'a423 address)
                                         store
                                         (church-pair
                                           (cons 'a424
                                             address)
                                           store
                                           church-value 1)
                                         church-count-map)
                                       (church-list-ref
                                         (cons 'a425 address)
                                         store church-probs
                                         0))
                                     ((lambda
                                        (address store
                                          church-table-count)
                                        ((lambda
                                           (address store
                                             church-new-table-count)
                                           ((lambda
                                              (address store
                                                church-new-count-map)
                                              (church-list
                                                (cons 'a426
                                                  address)
                                                store
                                                church-value
                                                church-new-count-map
                                                (church-list-ref
                                                  (cons
                                                    'a427
                                                    address)
                                                  store
                                                  church-probs
                                                  church-table-index)))
                                             (cons 'a428
                                               address)
                                             store
                                             (church-append
                                               (cons 'a429
                                                 address)
                                               store
                                               (church-take
                                                 (cons 'a430
                                                   address)
                                                 store
                                                 church-count-map
                                                 church-table-index)
                                               (church-list
                                                 (cons 'a431
                                                   address)
                                                 store
                                                 (church-pair
                                                   (cons
                                                     'a432
                                                     address)
                                                   store
                                                   church-value
                                                   church-new-table-count))
                                               (church-drop
                                                 (cons 'a433
                                                   address)
                                                 store
                                                 church-count-map
                                                 (church-+
                                                   (cons
                                                     'a434
                                                     address)
                                                   store 1
                                                   church-table-index)))))
                                          (cons 'a435
                                            address)
                                          store
                                          (church-+
                                            (cons 'a436
                                              address)
                                            store
                                            church-table-count
                                            1)))
                                       (cons 'a437 address)
                                       store
                                       (church-rest
                                         (cons 'a438 address)
                                         store
                                         (church-list-ref
                                           (cons 'a439
                                             address)
                                           store
                                           church-count-map
                                           church-table-index)))))
                                (cons 'a440 address) store
                                (church-list-index
                                  (cons 'a441 address) store
                                  (lambda
                                    (address store church-c)
                                    (church-eq?
                                      (cons 'a442 address)
                                      store church-value
                                      (church-first
                                        (cons 'a443 address)
                                        store church-c)))
                                  church-count-map)))
                             (cons 'a444 address) store
                             (church-map
                               (cons 'a445 address) store
                               (lambda
                                 (address store church-c)
                                 (church-/
                                   (cons 'a446 address)
                                   store church-c
                                   church-total-counts))
                               church-counts)))
                          (cons 'a447 address) store
                          (church-apply (cons 'a448 address)
                            store church-+ church-counts)))
                       (cons 'a449 address) store
                       (church-pair (cons 'a450 address)
                         store church-hyperparam
                         (church-map (cons 'a451 address)
                           store church-rest
                           church-count-map))))
                    (cons 'a452 address) store church-stats))
                (lambda
                  (address store church-value church-stats
                    church-hyperparam church-args)
                  ((lambda (address store church-count-map)
                     ((lambda (address store church-counts)
                        ((lambda
                           (address store church-table-index)
                           (if (church-eq?
                                 (cons 'a453 address) store
                                 church-false
                                 church-table-index)
                               (church-error
                                 (cons 'a454 address) store
                                 church-table-index
                                 "can't decr a value from CRP that doesn't label any table!")
                               ((lambda
                                  (address store
                                    church-table-count)
                                  ((lambda
                                     (address store
                                       church-new-table-count)
                                     ((lambda
                                        (address store
                                          church-new-count-map)
                                        (church-list
                                          (cons 'a455
                                            address)
                                          store church-value
                                          church-new-count-map
                                          (if (church-=
                                                (cons 'a456
                                                  address)
                                                store 0
                                                church-new-table-count)
                                              (church-/
                                                (cons 'a457
                                                  address)
                                                store
                                                church-hyperparam
                                                (church-+
                                                  (cons
                                                    'a458
                                                    address)
                                                  store
                                                  church-hyperparam
                                                  (church-apply
                                                    (cons
                                                      'a459
                                                      address)
                                                    store
                                                    church-+
                                                    church-counts)
                                                  (church--
                                                    (cons
                                                      'a460
                                                      address)
                                                    store 1)))
                                              (church-/
                                                (cons 'a461
                                                  address)
                                                store
                                                church-new-table-count
                                                (church-+
                                                  (cons
                                                    'a462
                                                    address)
                                                  store
                                                  church-hyperparam
                                                  (church-apply
                                                    (cons
                                                      'a463
                                                      address)
                                                    store
                                                    church-+
                                                    church-counts)
                                                  (church--
                                                    (cons
                                                      'a464
                                                      address)
                                                    store 1))))))
                                       (cons 'a465 address)
                                       store
                                       (if (church-=
                                             (cons 'a466
                                               address)
                                             store 0
                                             church-new-table-count)
                                           (church-append
                                             (cons 'a467
                                               address)
                                             store
                                             (church-take
                                               (cons 'a468
                                                 address)
                                               store
                                               church-count-map
                                               church-table-index)
                                             (church-drop
                                               (cons 'a469
                                                 address)
                                               store
                                               church-count-map
                                               (church-+
                                                 (cons 'a470
                                                   address)
                                                 store 1
                                                 church-table-index)))
                                           (church-append
                                             (cons 'a471
                                               address)
                                             store
                                             (church-take
                                               (cons 'a472
                                                 address)
                                               store
                                               church-count-map
                                               church-table-index)
                                             (church-list
                                               (cons 'a473
                                                 address)
                                               store
                                               (church-pair
                                                 (cons 'a474
                                                   address)
                                                 store
                                                 church-value
                                                 church-new-table-count))
                                             (church-drop
                                               (cons 'a475
                                                 address)
                                               store
                                               church-count-map
                                               (church-+
                                                 (cons 'a476
                                                   address)
                                                 store 1
                                                 church-table-index))))))
                                    (cons 'a477 address)
                                    store
                                    (church--
                                      (cons 'a478 address)
                                      store
                                      church-table-count 1)))
                                 (cons 'a479 address) store
                                 (church-rest
                                   (cons 'a480 address)
                                   store
                                   (church-list-ref
                                     (cons 'a481 address)
                                     store church-count-map
                                     church-table-index)))))
                          (cons 'a482 address) store
                          (church-list-index
                            (cons 'a483 address) store
                            (lambda (address store church-c)
                              (church-eq?
                                (cons 'a484 address) store
                                church-value
                                (church-first
                                  (cons 'a485 address) store
                                  church-c)))
                            church-count-map)))
                       (cons 'a486 address) store
                       (church-map (cons 'a487 address)
                         store church-rest church-count-map)))
                    (cons 'a488 address) store church-stats))
                'CRP-scorer '() church-alpha '() '())))
           (church-DPmem
            (lambda (address store church-alpha church-proc)
              ((lambda
                 (address store church-augmented-proc
                   church-crps)
                 (lambda (address store . church-argsin)
                   (church-augmented-proc
                     (cons 'a489 address) store
                     church-argsin
                     ((church-crps (cons 'a490 address)
                        store church-argsin)
                       (cons 'a491 address) store))))
                (cons 'a492 address) store
                ((lambda (mem-address store proc)
                   (lambda (address store . args)
                     (church-apply (cons args mem-address)
                       store proc args)))
                  address store
                  (lambda
                    (address store church-args church-part)
                    (church-apply (cons 'a493 address) store
                      church-proc church-args)))
                ((lambda (mem-address store proc)
                   (lambda (address store . args)
                     (church-apply (cons args mem-address)
                       store proc args)))
                  address store
                  (lambda (address store church-args)
                    (church-make-CRP (cons 'a494 address)
                      store church-alpha))))))
           (church-basic-proposal-distribution
            (lambda
              (address store church-state
                church-normal-form-proc)
              (if (church-addbox-empty? (cons 'a495 address)
                    store
                    (church-mcmc-state->xrp-draws
                      (cons 'a496 address) store
                      church-state))
                  (church-list (cons 'a497 address) store
                    0.0 church-state)
                  ((lambda (address store church-chosen-xrp)
                     ((lambda (address store church-ret1)
                        ((lambda
                           (address store
                             church-proposed-val)
                           ((lambda
                              (address store
                                church-proposal-fw-score)
                              ((lambda
                                 (address store
                                   church-proposal-bw-score)
                                 ((lambda
                                    (address store
                                      church-ret2)
                                    ((lambda
                                       (address store
                                         church-proposal-state)
                                       ((lambda
                                          (address store
                                            church-cd-bw/fw)
                                          ((lambda
                                             (address store
                                               church-ind-fw)
                                             ((lambda
                                                (address
                                                  store
                                                  church-ind-bw)
                                                (church-list
                                                  (cons
                                                    'a498
                                                    address)
                                                  store
                                                  (church-+
                                                    (cons
                                                      'a499
                                                      address)
                                                    store
                                                    (church--
                                                      (cons
                                                        'a500
                                                        address)
                                                      store
                                                      church-proposal-bw-score
                                                      church-proposal-fw-score)
                                                    church-cd-bw/fw
                                                    (church--
                                                      (cons
                                                        'a501
                                                        address)
                                                      store
                                                      church-ind-bw
                                                      church-ind-fw))
                                                  church-proposal-state))
                                               (cons 'a502
                                                 address)
                                               store
                                               (church--
                                                 (cons 'a503
                                                   address)
                                                 store
                                                 (church-log
                                                   (cons
                                                     'a504
                                                     address)
                                                   store
                                                   (church-addbox-size
                                                     (cons
                                                       'a505
                                                       address)
                                                     store
                                                     (church-mcmc-state->xrp-draws
                                                       (cons
                                                         'a506
                                                         address)
                                                       store
                                                       church-proposal-state))))))
                                            (cons 'a507
                                              address)
                                            store
                                            (church--
                                              (cons 'a508
                                                address)
                                              store
                                              (church-log
                                                (cons 'a509
                                                  address)
                                                store
                                                (church-addbox-size
                                                  (cons
                                                    'a510
                                                    address)
                                                  store
                                                  (church-mcmc-state->xrp-draws
                                                    (cons
                                                      'a511
                                                      address)
                                                    store
                                                    church-state))))))
                                         (cons 'a512 address)
                                         store
                                         (church-second
                                           (cons 'a513
                                             address)
                                           store church-ret2)))
                                      (cons 'a514 address)
                                      store
                                      (church-first
                                        (cons 'a515 address)
                                        store church-ret2)))
                                   (cons 'a516 address)
                                   store
                                   (church-counterfactual-update
                                     (cons 'a517 address)
                                     store church-state
                                     church-normal-form-proc
                                     (church-pair
                                       (cons 'a518 address)
                                       store
                                       church-chosen-xrp
                                       church-proposed-val))))
                                (cons 'a519 address) store
                                (church-third
                                  (cons 'a520 address) store
                                  church-ret1)))
                             (cons 'a521 address) store
                             (church-second
                               (cons 'a522 address) store
                               church-ret1)))
                          (cons 'a523 address) store
                          (church-first (cons 'a524 address)
                            store church-ret1)))
                       (cons 'a525 address) store
                       ((church-xrp-draw-proposer
                          (cons 'a526 address) store
                          church-chosen-xrp)
                         (cons 'a527 address) store
                         church-state)))
                    (cons 'a528 address) store
                    (church-rest (cons 'a529 address) store
                      (church-uniform-draw
                        (cons 'a530 address) store
                        (church-addbox->alist
                          (cons 'a531 address) store
                          (church-mcmc-state->xrp-draws
                            (cons 'a532 address) store
                            church-state))))))))
           (church-rejection-initializer
            (lambda (address store church-normal-form-proc)
              ((lambda (address store church-proposal-state)
                 (if (church-= (cons 'a533 address) store
                       -inf.0
                       (church-mcmc-state->score
                         (cons 'a534 address) store
                         church-proposal-state))
                     (church-rejection-initializer
                       (cons 'a535 address) store
                       church-normal-form-proc)
                     church-proposal-state))
                (cons 'a536 address) store
                (church-first (cons 'a537 address) store
                  (church-counterfactual-update
                    (cons 'a538 address) store
                    (church-make-initial-mcmc-state
                      (cons 'a539 address) store)
                    church-normal-form-proc)))))
           (church-verbose-init #f)
           (church-annealing-initializer
            (lambda
              (address store church-rej-steps
                church-temps:low->high church-temps->nfqp)
              ((lambda
                 (address store church-temps:high->low)
                 ((lambda
                    (address store church-normal-form-proc)
                    ((lambda
                       (address store church-initial-state)
                       (letrec ((church-next-temp
                                 (lambda
                                   (address store
                                     church-temps
                                     church-mcmc-state)
                                   (if (church-=
                                         (cons 'a540 address)
                                         store -inf.0
                                         (church-mcmc-state->score
                                           (cons 'a541
                                             address)
                                           store
                                           church-mcmc-state))
                                       (begin
                                         (if church-verbose-init
                                             (begin
                                               (church-display
                                                 (cons 'a542
                                                   address)
                                                 store
                                                 "annealing-initializer: failed, restarting at top ...\n"))
                                             '(void))
                                         (church-annealing-initializer
                                           (cons 'a543
                                             address)
                                           store
                                           church-rej-steps
                                           church-temps:low->high
                                           church-temps->nfqp))
                                       (if (church-null?
                                             (cons 'a544
                                               address)
                                             store
                                             church-temps)
                                           (begin
                                             (if church-verbose-init
                                                 (begin
                                                   (church-display
                                                     (cons
                                                       'a545
                                                       address)
                                                     store
                                                     "annealing-initializer: succeeded!\n"))
                                                 '(void))
                                             church-mcmc-state)
                                           (begin
                                             (if church-verbose-init
                                                 (begin
                                                   (church-for-each
                                                     (cons
                                                       'a546
                                                       address)
                                                     store
                                                     church-display
                                                     (church-list
                                                       (cons
                                                         'a547
                                                         address)
                                                       store
                                                       "annealing-initializer:\n"
                                                       "  temps remaining: "
                                                       (church-length
                                                         (cons
                                                           'a548
                                                           address)
                                                         store
                                                         church-temps)
                                                       "\n  current temp: "
                                                       (church-first
                                                         (cons
                                                           'a549
                                                           address)
                                                         store
                                                         church-temps)
                                                       "\n  current val: "
                                                       (church-mcmc-state->query-value
                                                         (cons
                                                           'a550
                                                           address)
                                                         store
                                                         church-mcmc-state)
                                                       "\n")))
                                                 '(void))
                                             ((lambda
                                                (address
                                                  store
                                                  church-nfqp)
                                                ((lambda
                                                   (address
                                                     store
                                                     church-rescored-state)
                                                   ((lambda
                                                      (address
                                                        store
                                                        church-rej-state)
                                                      (church-next-temp
                                                        (cons
                                                          'a551
                                                          address)
                                                        store
                                                        (church-rest
                                                          (cons
                                                            'a552
                                                            address)
                                                          store
                                                          church-temps)
                                                        church-rej-state))
                                                     (cons
                                                       'a553
                                                       address)
                                                     store
                                                     ((church-basic-repeat-kernel
                                                        (cons
                                                          'a554
                                                          address)
                                                        store
                                                        church-rej-steps
                                                        church-nfqp)
                                                       (cons
                                                         'a555
                                                         address)
                                                       store
                                                       church-rescored-state)))
                                                  (cons
                                                    'a556
                                                    address)
                                                  store
                                                  (church-first
                                                    (cons
                                                      'a557
                                                      address)
                                                    store
                                                    (church-counterfactual-update
                                                      (cons
                                                        'a558
                                                        address)
                                                      store
                                                      church-mcmc-state
                                                      church-nfqp))))
                                               (cons 'a559
                                                 address)
                                               store
                                               (church-apply
                                                 (cons 'a560
                                                   address)
                                                 store
                                                 church-temps->nfqp
                                                 (church-first
                                                   (cons
                                                     'a561
                                                     address)
                                                   store
                                                   church-temps)))))))))
                         (church-next-temp
                           (cons 'a562 address) store
                           (church-rest (cons 'a563 address)
                             store church-temps:high->low)
                           church-initial-state)))
                      (cons 'a564 address) store
                      (begin
                        (church-reset-store-xrp-draws
                          (cons 'a565 address) store)
                        (church-rejection-initializer
                          (cons 'a566 address) store
                          church-normal-form-proc))))
                   (cons 'a567 address) store
                   (church-apply (cons 'a568 address) store
                     church-temps->nfqp
                     (church-first (cons 'a569 address)
                       store church-temps:high->low))))
                (cons 'a570 address) store
                (church-reverse (cons 'a571 address) store
                  church-temps:low->high))))
           (church-make-mh-kernel
            (lambda
              (address store church-proposal-distribution
                church-scorer)
              (lambda (address store church-state)
                ((lambda (address store church-ret)
                   ((lambda (address store church-bw/fw)
                      ((lambda
                         (address store
                           church-proposal-state)
                         ((lambda
                            (address store church-old-p)
                            ((lambda
                               (address store church-new-p)
                               ((lambda
                                  (address store
                                    church-accept)
                                  ((lambda
                                     (address store
                                       church-dummy)
                                     (if church-accept
                                         church-proposal-state
                                         church-state))
                                    (cons 'a572 address)
                                    store
                                    (church-reset-store-xrp-draws
                                      (cons 'a573 address)
                                      store)))
                                 (cons 'a574 address) store
                                 (church-log-flip
                                   (cons 'a575 address)
                                   store
                                   (church-min
                                     (cons 'a576 address)
                                     store 0.0
                                     (church-+
                                       (cons 'a577 address)
                                       store
                                       (church--
                                         (cons 'a578 address)
                                         store church-new-p
                                         church-old-p)
                                       church-bw/fw)))))
                              (cons 'a579 address) store
                              (church-scorer
                                (cons 'a580 address) store
                                church-proposal-state)))
                           (cons 'a581 address) store
                           (church-scorer
                             (cons 'a582 address) store
                             church-state)))
                        (cons 'a583 address) store
                        (church-second (cons 'a584 address)
                          store church-ret)))
                     (cons 'a585 address) store
                     (church-first (cons 'a586 address)
                       store church-ret)))
                  (cons 'a587 address) store
                  (church-proposal-distribution
                    (cons 'a588 address) store church-state)))))
           (church-cycle-kernel
            (lambda (address store . church-kernels)
              (lambda (address store church-state)
                (church-fold (cons 'a589 address) store
                  (lambda (address store church-k church-s)
                    (church-k (cons 'a590 address) store
                      church-s))
                  church-state church-kernels))))
           (church-repeat-kernel
            (lambda
              (address store church-steps church-kernel)
              (church-apply (cons 'a591 address) store
                church-cycle-kernel
                (church-make-list (cons 'a592 address) store
                  church-steps church-kernel))))
           (church-basic-repeat-kernel
            (lambda (address store church-steps church-nfqp)
              (church-repeat-kernel (cons 'a593 address)
                store church-steps
                (church-make-mh-kernel (cons 'a594 address)
                  store
                  (lambda (address store church-state)
                    (church-basic-proposal-distribution
                      (cons 'a595 address) store
                      church-state church-nfqp))
                  church-mcmc-state->score))))
           (church-inference-timing #f)
           (church-repeated-mcmc-query-core
            (lambda
              (address store church-initializer
                church-kernel church-num-samples)
              ((lambda (address store church-init-state)
                 (begin
                   (if church-inference-timing
                       (begin
                         (church-display
                           (cons 'a596 address) store
                           "initialized: ")
                         (church-display
                           (cons 'a597 address) store
                           (church-current-date
                             (cons 'a598 address) store))
                         (church-display
                           (cons 'a599 address) store "\n"))
                       '(void))
                   ((lambda (address store church-ret)
                      (begin
                        (if church-inference-timing
                            (begin
                              (church-display
                                (cons 'a600 address) store
                                "done: ")
                              (church-display
                                (cons 'a601 address) store
                                (church-current-date
                                  (cons 'a602 address) store))
                              (church-display
                                (cons 'a603 address) store
                                "\n"))
                            '(void))
                        church-ret))
                     (cons 'a604 address) store
                     (church-mcmc-loop (cons 'a605 address)
                       store church-kernel church-init-state
                       church-num-samples '()))))
                (cons 'a606 address) store
                (church-initializer (cons 'a607 address)
                  store))))
           (church-mcmc-loop
            (lambda
              (address store church-kernel church-state
                church-samples-left church-samples)
              (if (church-< (cons 'a608 address) store
                    church-samples-left 1)
                  (church-reverse (cons 'a609 address) store
                    church-samples)
                  (church-mcmc-loop (cons 'a610 address)
                    store church-kernel
                    (church-kernel (cons 'a611 address)
                      store church-state)
                    (church-- (cons 'a612 address) store
                      church-samples-left 1)
                    (church-pair (cons 'a613 address) store
                      (church-mcmc-state->query-value
                        (cons 'a614 address) store
                        church-state)
                      church-samples)))))
           (church-mh-query
            (lambda
              (address store church-samples church-lag
                church-normal-form-proc)
              (church-repeated-mcmc-query-core
                (cons 'a615 address) store
                (lambda (address store)
                  (church-rejection-initializer
                    (cons 'a616 address) store
                    church-normal-form-proc))
                (church-basic-repeat-kernel
                  (cons 'a617 address) store church-lag
                  church-normal-form-proc)
                church-samples)))
           (church-mh-query/annealed-init
            (lambda
              (address store church-temps church-samples
                church-lag church-rej-steps
                church-temps->nfqp)
              ((lambda
                 (address store church-normal-form-proc)
                 (church-repeated-mcmc-query-core
                   (cons 'a618 address) store
                   (lambda (address store)
                     (church-annealing-initializer
                       (cons 'a619 address) store
                       church-rej-steps church-temps
                       church-temps->nfqp))
                   (church-basic-repeat-kernel
                     (cons 'a620 address) store church-lag
                     church-normal-form-proc)
                   church-samples))
                (cons 'a621 address) store
                (church-apply (cons 'a622 address) store
                  church-temps->nfqp
                  (church-first (cons 'a623 address) store
                    church-temps)))))
           (church-psmc-query
            (lambda
              (address store church-temps church-popsize
                church-lag church-temps->nfqp)
              (church-map (cons 'a624 address) store
                church-mcmc-state->query-value
                (church-smc-core (cons 'a625 address) store
                  church-temps church-popsize church-lag
                  church-temps->nfqp))))
           (church-smc-core
            (lambda
              (address store church-temps church-popsize
                church-lag church-temps->nfqp)
              (letrec ((church-smc
                        (lambda
                          (address store church-temps
                            church-population church-weights)
                          ((lambda
                             (address store church-rets)
                             ((lambda
                                (address store
                                  church-new-population)
                                ((lambda
                                   (address store
                                     church-cd-bw/fw)
                                   ((lambda
                                      (address store
                                        church-weights)
                                      ((lambda
                                         (address store
                                           church-resample-distribution)
                                         ((lambda
                                            (address store
                                              church-collapse?)
                                            ((lambda
                                               (address
                                                 store
                                                 church-new2-population)
                                               ((lambda
                                                  (address
                                                    store
                                                    church-weights)
                                                  ((lambda
                                                     (address
                                                       store
                                                       church-kernel)
                                                     ((lambda
                                                        (address
                                                          store
                                                          church-new3-population)
                                                        (begin
                                                          (church-map
                                                            (cons
                                                              'a626
                                                              address)
                                                            store
                                                            (lambda
                                                              (address
                                                                store
                                                                church-x
                                                                church-y)
                                                              (begin
                                                                (church-display
                                                                  (cons
                                                                    'a627
                                                                    address)
                                                                  store
                                                                  "  ")
                                                                (church-display
                                                                  (cons
                                                                    'a628
                                                                    address)
                                                                  store
                                                                  church-x)
                                                                (church-display
                                                                  (cons
                                                                    'a629
                                                                    address)
                                                                  store
                                                                  "  ")
                                                                (church-display
                                                                  (cons
                                                                    'a630
                                                                    address)
                                                                  store
                                                                  church-y)
                                                                (church-display
                                                                  (cons
                                                                    'a631
                                                                    address)
                                                                  store
                                                                  "\n")))
                                                            (church-map
                                                              (cons
                                                                'a632
                                                                address)
                                                              store
                                                              church-mcmc-state->query-value
                                                              church-new3-population)
                                                            (church-map
                                                              (cons
                                                                'a633
                                                                address)
                                                              store
                                                              church-mcmc-state->score
                                                              church-new3-population))
                                                          (church-display
                                                            (cons
                                                              'a634
                                                              address)
                                                            store
                                                            "\n")
                                                          (if (church-or
                                                                (cons
                                                                  'a635
                                                                  address)
                                                                store
                                                                church-collapse?
                                                                (church-null?
                                                                  (cons
                                                                    'a636
                                                                    address)
                                                                  store
                                                                  (church-rest
                                                                    (cons
                                                                      'a637
                                                                      address)
                                                                    store
                                                                    church-temps)))
                                                              church-new3-population
                                                              (church-smc
                                                                (cons
                                                                  'a638
                                                                  address)
                                                                store
                                                                (church-rest
                                                                  (cons
                                                                    'a639
                                                                    address)
                                                                  store
                                                                  church-temps)
                                                                church-new3-population
                                                                church-weights))))
                                                       (cons
                                                         'a640
                                                         address)
                                                       store
                                                       (church-map
                                                         (cons
                                                           'a641
                                                           address)
                                                         store
                                                         church-kernel
                                                         church-new2-population)))
                                                    (cons
                                                      'a642
                                                      address)
                                                    store
                                                    (church-repeat-kernel
                                                      (cons
                                                        'a643
                                                        address)
                                                      store
                                                      church-lag
                                                      (church-make-mh-kernel
                                                        (cons
                                                          'a644
                                                          address)
                                                        store
                                                        (lambda
                                                          (address
                                                            store
                                                            church-state)
                                                          (church-basic-proposal-distribution
                                                            (cons
                                                              'a645
                                                              address)
                                                            store
                                                            church-state
                                                            (church-apply
                                                              (cons
                                                                'a646
                                                                address)
                                                              store
                                                              church-temps->nfqp
                                                              (church-first
                                                                (cons
                                                                  'a647
                                                                  address)
                                                                store
                                                                church-temps))))
                                                        church-mcmc-state->score))))
                                                 (cons 'a648
                                                   address)
                                                 store
                                                 (church-make-list
                                                   (cons
                                                     'a649
                                                     address)
                                                   store
                                                   church-popsize
                                                   0)))
                                              (cons 'a650
                                                address)
                                              store
                                              (if church-collapse?
                                                  '()
                                                  (church-repeat
                                                    (cons
                                                      'a651
                                                      address)
                                                    store
                                                    church-popsize
                                                    (lambda
                                                      (address
                                                        store)
                                                      (begin
                                                        (church-reset-store-xrp-draws
                                                          (cons
                                                            'a652
                                                            address)
                                                          store)
                                                        (church-multinomial
                                                          (cons
                                                            'a653
                                                            address)
                                                          store
                                                          church-new-population
                                                          church-resample-distribution)))))))
                                           (cons 'a654
                                             address)
                                           store
                                           (church-nan?
                                             (cons 'a655
                                               address)
                                             store
                                             (church-first
                                               (cons 'a656
                                                 address)
                                               store
                                               church-resample-distribution))))
                                        (cons 'a657 address)
                                        store
                                        (church-map
                                          (cons 'a658
                                            address)
                                          store church-exp
                                          (church-log-normalize
                                            (cons 'a659
                                              address)
                                            store
                                            church-weights))))
                                     (cons 'a660 address)
                                     store
                                     (church-map
                                       (cons 'a661 address)
                                       store
                                       (lambda
                                         (address store
                                           church-old-weight
                                           church-old-state
                                           church-new-state
                                           church-cd-bw/fw)
                                         (church-+
                                           (cons 'a662
                                             address)
                                           store
                                           church-old-weight
                                           (church--
                                             (cons 'a663
                                               address)
                                             store
                                             (church-mcmc-state->score
                                               (cons 'a664
                                                 address)
                                               store
                                               church-new-state)
                                             (church-mcmc-state->score
                                               (cons 'a665
                                                 address)
                                               store
                                               church-old-state))
                                           church-cd-bw/fw))
                                       church-weights
                                       church-population
                                       church-new-population
                                       church-cd-bw/fw)))
                                  (cons 'a666 address) store
                                  (church-map
                                    (cons 'a667 address)
                                    store church-second
                                    church-rets)))
                               (cons 'a668 address) store
                               (church-map
                                 (cons 'a669 address) store
                                 church-first church-rets)))
                            (cons 'a670 address) store
                            (church-map (cons 'a671 address)
                              store
                              (lambda
                                (address store church-state)
                                (church-counterfactual-update
                                  (cons 'a672 address) store
                                  church-state
                                  (church-apply
                                    (cons 'a673 address)
                                    store church-temps->nfqp
                                    (church-first
                                      (cons 'a674 address)
                                      store church-temps))))
                              church-population)))))
                (church-smc (cons 'a675 address) store
                  church-temps
                  (church-repeat (cons 'a676 address) store
                    church-popsize
                    (lambda (address store)
                      (begin
                        (church-reset-store-xrp-draws
                          (cons 'a677 address) store)
                        (church-rejection-initializer
                          (cons 'a678 address) store
                          (church-apply (cons 'a679 address)
                            store church-temps->nfqp
                            (church-first
                              (cons 'a680 address) store
                              church-temps))))))
                  (church-make-list (cons 'a681 address)
                    store church-popsize 0)))))
           (church-log-sum-exp
            (lambda (address store . church-log-vals)
              ((lambda (address store church-max-log-val)
                 (if (church-equal? (cons 'a682 address)
                       store church-max-log-val -inf.0)
                     -inf.0
                     (church-+ (cons 'a683 address) store
                       (church-log (cons 'a684 address)
                         store
                         (church-exact->inexact
                           (cons 'a685 address) store
                           (church-sum (cons 'a686 address)
                             store
                             (church-map
                               (cons 'a687 address) store
                               (lambda
                                 (address store church-val)
                                 (church-exp
                                   (cons 'a688 address)
                                   store
                                   (church--
                                     (cons 'a689 address)
                                     store church-val
                                     church-max-log-val)))
                               church-log-vals))))
                       church-max-log-val)))
                (cons 'a690 address) store
                (church-apply (cons 'a691 address) store
                  church-max church-log-vals))))
           (church-log-normalize
            (lambda (address store church-log-scores)
              ((lambda (address store church-score-sum)
                 (church-map (cons 'a692 address) store
                   (lambda (address store church-score)
                     (church-- (cons 'a693 address) store
                       church-score church-score-sum))
                   church-log-scores))
                (cons 'a694 address) store
                (church-apply (cons 'a695 address) store
                  church-log-sum-exp church-log-scores))))
           (church-lazy-null '())
           (church-lazy-null? church-null?)
           (church-lazy-pair
            (lambda (address store church-a church-b)
              (lambda (address store church-tag)
                (if (church-eq? (cons 'a696 address) store
                      church-tag 'first)
                    church-a
                    (if (church-eq? (cons 'a697 address)
                          store church-tag 'rest)
                        church-b
                        'lazy-pair)))))
           (church-lazy-pair?
            (lambda (address store church-a)
              (if (church-procedure? (cons 'a698 address)
                    store church-a)
                  (church-eq? (cons 'a699 address) store
                    'lazy-pair
                    (church-a (cons 'a700 address) store
                      'type?))
                  church-false)))
           (church-lazy-list
            (lambda (address store . church-args)
              (if (church-pair? (cons 'a701 address) store
                    church-args)
                  (church-lazy-pair (cons 'a702 address)
                    store
                    (church-first (cons 'a703 address) store
                      church-args)
                    (church-apply (cons 'a704 address) store
                      church-lazy-list
                      (church-rest (cons 'a705 address)
                        store church-args)))
                  church-args)))
           (church-seq-sexpr-equal?
            (lambda
              (address store church-t1 church-t2
                church-depth)
              (if (church-= (cons 'a706 address) store
                    church-depth 0)
                  0
                  (if (church-and (cons 'a707 address) store
                        (church-lazy-pair?
                          (cons 'a708 address) store
                          church-t1)
                        (church-lazy-pair?
                          (cons 'a709 address) store
                          church-t2))
                      ((lambda (address store church-left)
                         (if (church-eq?
                               (cons 'a710 address) store
                               church-false church-left)
                             church-false
                             (church-seq-sexpr-equal?
                               (cons 'a711 address) store
                               (church-t1
                                 (cons 'a712 address) store
                                 'rest)
                               (church-t2
                                 (cons 'a713 address) store
                                 'rest)
                               church-left)))
                        (cons 'a714 address) store
                        (church-seq-sexpr-equal?
                          (cons 'a715 address) store
                          (church-t1 (cons 'a716 address)
                            store 'first)
                          (church-t2 (cons 'a717 address)
                            store 'first)
                          (church-- (cons 'a718 address)
                            store church-depth 1)))
                      (if (church-eq? (cons 'a719 address)
                            store church-t1 church-t2)
                          (church-- (cons 'a720 address)
                            store church-depth 1)
                          church-false)))))
           (church-lazy-equal?
            (lambda
              (address store church-a church-b church-depth)
              (church-not (cons 'a721 address) store
                (church-eq? (cons 'a722 address) store
                  church-false
                  (church-seq-sexpr-equal?
                    (cons 'a723 address) store church-a
                    church-b church-depth)))))
           (church-lazy-all-equal?
            (lambda
              (address store church-lazy-lst1
                church-lazy-lst2)
              ((lambda
                 (address store church-lst1 church-lst2)
                 (church-equal? (cons 'a724 address) store
                   church-lst1 church-lst2))
                (cons 'a725 address) store
                (church-lazy-list->all-list
                  (cons 'a726 address) store
                  church-lazy-lst1)
                (church-lazy-list->all-list
                  (cons 'a727 address) store
                  church-lazy-lst2))))
           (church-lazy-list->all-list
            (lambda (address store church-lazy-lst)
              (if (church-not (cons 'a728 address) store
                    (church-lazy-pair? (cons 'a729 address)
                      store church-lazy-lst))
                  church-lazy-lst
                  ((lambda (address store church-left)
                     ((lambda (address store church-right)
                        (church-pair (cons 'a730 address)
                          store church-left church-right))
                       (cons 'a731 address) store
                       (church-lazy-list->all-list
                         (cons 'a732 address) store
                         (church-lazy-lst
                           (cons 'a733 address) store 'rest))))
                    (cons 'a734 address) store
                    (church-lazy-list->all-list
                      (cons 'a735 address) store
                      (church-lazy-lst (cons 'a736 address)
                        store 'first))))))
           (church-lazy-append
            (lambda
              (address store church-lazy-lst1
                church-lazy-lst2)
              (if (church-lazy-null? (cons 'a737 address)
                    store church-lazy-lst1)
                  church-lazy-lst2
                  (church-lazy-pair (cons 'a738 address)
                    store
                    (church-lazy-lst1 (cons 'a739 address)
                      store 'first)
                    (church-lazy-append (cons 'a740 address)
                      store
                      (church-lazy-lst1 (cons 'a741 address)
                        store 'rest)
                      church-lazy-lst2)))))
           (church-compute-depth
            (lambda (address store church-lazy-lst)
              (church-pretty-print (cons 'a742 address)
                store "define compute-depth")))
           (church-lazy-length
            (lambda (address store church-lazy-lst)
              (if (church-null? (cons 'a743 address) store
                    church-lazy-lst)
                  0
                  (church-+ (cons 'a744 address) store 1
                    (church-lazy-length (cons 'a745 address)
                      store
                      (church-lazy-lst (cons 'a746 address)
                        store 'rest))))))
           (church-list->lazy-list
            (lambda (address store church-lst)
              (if (church-pair? (cons 'a747 address) store
                    church-lst)
                  (church-apply (cons 'a748 address) store
                    church-lazy-list
                    (church-map (cons 'a749 address) store
                      church-list->lazy-list church-lst))
                  church-lst)))
           (church-lazy-remove
            (lambda
              (address store church-item church-lazy-lst)
              (if (church-not (cons 'a750 address) store
                    (church-lazy-pair? (cons 'a751 address)
                      store church-lazy-lst))
                  church-lazy-lst
                  (if (church-lazy-all-equal?
                        (cons 'a752 address) store
                        (church-lazy-lst
                          (cons 'a753 address) store 'first)
                        church-item)
                      (church-lazy-remove
                        (cons 'a754 address) store
                        church-item
                        (church-lazy-lst
                          (cons 'a755 address) store 'rest))
                      (church-lazy-pair (cons 'a756 address)
                        store
                        (church-lazy-lst
                          (cons 'a757 address) store 'first)
                        (church-lazy-remove
                          (cons 'a758 address) store
                          church-item
                          (church-lazy-lst
                            (cons 'a759 address) store 'rest)))))))
           (church-lazy-uniform-draw
            (lambda (address store church-lazy-lst)
              (church-uniform-draw (cons 'a760 address)
                store
                (church-lazy-list->all-list
                  (cons 'a761 address) store church-lazy-lst))))
           (church-lazy-list->list
            (lambda (address store church-a church-depth)
              (if (church-= (cons 'a762 address) store 0
                    church-depth)
                  (church-pair (cons 'a763 address) store
                    'unf 0)
                  (if (church-lazy-pair?
                        (cons 'a764 address) store church-a)
                      ((lambda (address store church-left)
                         ((lambda
                            (address store church-right)
                            (church-pair
                              (cons 'a765 address) store
                              (church-pair
                                (cons 'a766 address) store
                                (church-first
                                  (cons 'a767 address) store
                                  church-left)
                                (church-first
                                  (cons 'a768 address) store
                                  church-right))
                              (church-rest
                                (cons 'a769 address) store
                                church-right)))
                           (cons 'a770 address) store
                           (church-lazy-list->list
                             (cons 'a771 address) store
                             (church-a (cons 'a772 address)
                               store 'rest)
                             (church-rest
                               (cons 'a773 address) store
                               church-left))))
                        (cons 'a774 address) store
                        (church-lazy-list->list
                          (cons 'a775 address) store
                          (church-a (cons 'a776 address)
                            store 'first)
                          (church-- (cons 'a777 address)
                            store church-depth 1)))
                      (church-pair (cons 'a778 address)
                        store church-a
                        (church-- (cons 'a779 address) store
                          church-depth 1))))))
           (church-MAX-FUNCS 5)
           (church-MAX-ARITY 3)
           (church-gen-program
            (lambda (address store)
              ((lambda (address store church-num-of-funcs)
                 ((lambda
                    (address store
                      church-function-signatures)
                    ((lambda
                       (address store church-functions)
                       ((lambda (address store church-body)
                          (church-combine-program-parts
                            (cons 'a780 address) store
                            church-functions church-body))
                         (cons 'a781 address) store
                         (church-gen-expr
                           (cons 'a782 address) store '()
                           church-function-signatures)))
                      (cons 'a783 address) store
                      (church-gen-functions
                        (cons 'a784 address) store
                        church-function-signatures)))
                   (cons 'a785 address) store
                   (church-gen-func-signatures
                     (cons 'a786 address) store
                     church-num-of-funcs)))
                (cons 'a787 address) store
                (church-random-integer (cons 'a788 address)
                  store church-MAX-FUNCS))))
           (church-combine-program-parts
            (lambda
              (address store church-functions church-body)
              (church-lazy-append (cons 'a789 address) store
                (church-lazy-append (cons 'a790 address)
                  store
                  (church-lazy-list (cons 'a791 address)
                    store 'let '())
                  church-functions)
                (church-lazy-list (cons 'a792 address) store
                  church-body))))
           (church-get-name
            (lambda (address store church-sig)
              (church-sig (cons 'a793 address) store 'first)))
           (church-get-vars
            (lambda (address store church-sig)
              (church-sig (cons 'a794 address) store 'rest)))
           (church-get-arity
            (lambda (address store church-signature)
              (church-lazy-length (cons 'a795 address) store
                (church-get-vars (cons 'a796 address) store
                  church-signature))))
           (church-gen-func-signatures
            (lambda (address store church-num-of-funcs)
              (if (church-= (cons 'a797 address) store
                    church-num-of-funcs 0)
                  '()
                  (church-lazy-pair (cons 'a798 address)
                    store
                    (church-gen-func-signature
                      (cons 'a799 address) store)
                    (church-gen-func-signatures
                      (cons 'a800 address) store
                      (church-- (cons 'a801 address) store
                        church-num-of-funcs 1))))))
           (church-gen-func-signature
            (lambda (address store)
              ((lambda (address store church-name)
                 ((lambda (address store church-arity)
                    ((lambda (address store church-vars)
                       (church-combine-sig-parts
                         (cons 'a802 address) store
                         church-name church-vars))
                      (cons 'a803 address) store
                      (church-gen-vars (cons 'a804 address)
                        store church-arity)))
                   (cons 'a805 address) store
                   (church-random-integer
                     (cons 'a806 address) store
                     church-MAX-ARITY)))
                (cons 'a807 address) store
                (church-sym (cons 'a808 address) store 'F))))
           (church-combine-sig-parts church-lazy-pair)
           (church-gen-vars
            (lambda (address store church-arity)
              (if (church-= (cons 'a809 address) store 0
                    church-arity)
                  '()
                  (church-lazy-pair (cons 'a810 address)
                    store
                    (church-sym (cons 'a811 address) store
                      'V)
                    (church-gen-vars (cons 'a812 address)
                      store
                      (church-- (cons 'a813 address) store
                        church-arity 1))))))
           (church-gen-functions
            (lambda
              (address store church-function-signatures)
              (if (church-lazy-null? (cons 'a814 address)
                    store church-function-signatures)
                  '()
                  (church-lazy-pair (cons 'a815 address)
                    store
                    (church-gen-function
                      (cons 'a816 address) store
                      (church-function-signatures
                        (cons 'a817 address) store 'first)
                      church-function-signatures)
                    (church-gen-functions
                      (cons 'a818 address) store
                      (church-function-signatures
                        (cons 'a819 address) store 'rest))))))
           (church-gen-function
            (lambda
              (address store church-function-signature
                church-function-signatures)
              ((lambda
                 (address store church-function-signatures)
                 ((lambda (address store church-vars)
                    ((lambda (address store church-body)
                       (church-combine-function-parts
                         (cons 'a820 address) store
                         church-function-signature
                         church-body))
                      (cons 'a821 address) store
                      (church-gen-expr (cons 'a822 address)
                        store church-vars
                        church-function-signatures)))
                   (cons 'a823 address) store
                   (church-get-vars (cons 'a824 address)
                     store church-function-signature)))
                (cons 'a825 address) store
                (church-lazy-remove (cons 'a826 address)
                  store church-function-signature
                  church-function-signatures))))
           (church-combine-function-parts
            (lambda
              (address store church-function-signature
                church-body)
              (church-lazy-list (cons 'a827 address) store
                'define church-function-signature
                church-body)))
           (church-gen-expr
            (lambda
              (address store church-vars
                church-function-signatures)
              (if (church-and (cons 'a828 address) store
                    (church-null? (cons 'a829 address) store
                      church-vars)
                    (church-null? (cons 'a830 address) store
                      church-function-signatures))
                  (church-sample (cons 'a831 address) store
                    (church-multinomial (cons 'a832 address)
                      store
                      (church-list (cons 'a833 address)
                        store
                        (lambda (address store)
                          (church-gen-if
                            (cons 'a834 address) store
                            church-vars
                            church-function-signatures))
                        church-gen-bool)
                      (church-list (cons 'a835 address)
                        store 1/4 3/4)))
                  (if (church-null? (cons 'a836 address)
                        store church-vars)
                      (church-sample (cons 'a837 address)
                        store
                        (church-multinomial
                          (cons 'a838 address) store
                          (church-list (cons 'a839 address)
                            store
                            (lambda (address store)
                              (church-gen-if
                                (cons 'a840 address) store
                                church-vars
                                church-function-signatures))
                            church-gen-bool
                            (lambda (address store)
                              (church-gen-application
                                (cons 'a841 address) store
                                church-vars
                                church-function-signatures)))
                          (church-list (cons 'a842 address)
                            store 1/8 3/4 1/8)))
                      (if (church-null? (cons 'a843 address)
                            store church-function-signatures)
                          (church-sample
                            (cons 'a844 address) store
                            (church-multinomial
                              (cons 'a845 address) store
                              (church-list
                                (cons 'a846 address) store
                                (lambda (address store)
                                  (church-gen-if
                                    (cons 'a847 address)
                                    store church-vars
                                    church-function-signatures))
                                church-gen-bool
                                (lambda (address store)
                                  (church-gen-vars
                                    (cons 'a848 address)
                                    store church-vars)))
                              (church-list
                                (cons 'a849 address) store
                                1/8 3/4 1/8)))
                          (church-sample
                            (cons 'a850 address) store
                            (church-multinomial
                              (cons 'a851 address) store
                              (church-list
                                (cons 'a852 address) store
                                (lambda (address store)
                                  (church-gen-if
                                    (cons 'a853 address)
                                    store church-vars
                                    church-function-signatures))
                                church-gen-bool
                                (lambda (address store)
                                  (church-gen-application
                                    (cons 'a854 address)
                                    store church-vars
                                    church-function-signatures))
                                (lambda (address store)
                                  (church-gen-var
                                    (cons 'a855 address)
                                    store church-vars)))
                              (church-list
                                (cons 'a856 address) store
                                1/12 3/4 1/12 1/12))))))))
           (church-gen-bool
            (lambda (address store)
              (church-uniform-draw (cons 'a857 address)
                store '(t f))))
           (church-gen-if
            (lambda
              (address store church-vars
                church-function-signatures)
              (church-combine-if-parts (cons 'a858 address)
                store 'if
                (church-gen-expr (cons 'a859 address) store
                  church-vars church-function-signatures)
                (church-gen-expr (cons 'a860 address) store
                  church-vars church-function-signatures)
                (church-gen-expr (cons 'a861 address) store
                  church-vars church-function-signatures))))
           (church-combine-if-parts church-lazy-list)
           (church-gen-application
            (lambda
              (address store church-vars
                church-function-signatures)
              ((lambda
                 (address store church-function-signature)
                 ((lambda (address store church-function)
                    ((lambda (address store church-arity)
                       ((lambda (address store church-args)
                          (church-combine-application-parts
                            (cons 'a862 address) store
                            church-function church-args))
                         (cons 'a863 address) store
                         (church-gen-args
                           (cons 'a864 address) store
                           church-arity church-vars
                           church-function-signatures)))
                      (cons 'a865 address) store
                      (church-get-arity (cons 'a866 address)
                        store church-function-signature)))
                   (cons 'a867 address) store
                   (church-get-name (cons 'a868 address)
                     store church-function-signature)))
                (cons 'a869 address) store
                (church-lazy-uniform-draw
                  (cons 'a870 address) store
                  church-function-signatures))))
           (church-combine-application-parts
            church-lazy-pair)
           (church-gen-args
            (lambda
              (address store church-arity church-vars
                church-function-signatures)
              (if (church-= (cons 'a871 address) store
                    church-arity 0)
                  '()
                  (church-lazy-pair (cons 'a872 address)
                    store
                    (church-gen-expr (cons 'a873 address)
                      store church-vars
                      church-function-signatures)
                    (church-gen-args (cons 'a874 address)
                      store
                      (church-- (cons 'a875 address) store
                        church-arity 1)
                      church-vars church-function-signatures)))))
           (church-gen-var church-lazy-uniform-draw)
           (church-thunk (lambda (address store) 'a)))
    (begin
      (church-pretty-print (cons 'a876 address) store
        (church-gen-bool (cons 'a877 address) store))
      (church-pretty-print (cons 'a878 address) store
        (church-lazy-list->all-list (cons 'a879 address)
          store
          (church-gen-vars (cons 'a880 address) store 3)))
      (church-pretty-print (cons 'a881 address) store
        (church-lazy-list->all-list (cons 'a882 address)
          store
          (church-gen-func-signature (cons 'a883 address)
            store)))
      (church-pretty-print (cons 'a884 address) store
        (church-lazy-list->all-list (cons 'a885 address)
          store
          (church-gen-expr (cons 'a886 address) store '()
            '())))
      (church-pretty-print (cons 'a887 address) store
        (church-lazy-list->all-list (cons 'a888 address)
          store
          (church-get-arity (cons 'a889 address) store
            (church-lazy-list (cons 'a890 address) store 'F1
              'V1 'V2)))))))

;;seed the random number generator
(randomize-rng)

(display
(church-main '(top) (make-empty-store))
) (newline)
;;done