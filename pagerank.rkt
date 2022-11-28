#lang racket

;; Project 2: Implementing PageRank (see README.md and video)
;;
;; PageRank is a popular graph algorithm used for information
;; retrieval and was first popularized as an algorithm powering
;; the Google search engine. Details of the PageRank algorithm will be
;; discussed in class. Here, you will implement several functions that
;; implement the PageRank algorithm in Racket.
;;
;; Hints: 
;; 
;; - For this assignment, you may assume that no graph will include
;; any "self-links" (pages that link to themselves) and that each page
;; will link to at least one other page.
;;
;; - you can use the code in `testing-facilities.rkt` to help generate
;; test input graphs for the project. The test suite was generated
;; using those functions.
;;
;; - You may want to define "helper functions" to break up complicated
;; function definitions.

(provide graph?
         pagerank?
         num-pages
         num-links
         get-backlinks
         mk-initial-pagerank
         step-pagerank
         iterate-pagerank-until
         rank-pages)

;; This program accepts graphs as input. Graphs are represented as a
;; list of links, where each link is a list `(,src ,dst) that signals
;; page src links to page dst.
;; (-> any? boolean?)
(define (graph? glst)
  (and (list? glst)
       (andmap
        (lambda (element)
          (match element
                 [`(,(? symbol? src) ,(? symbol? dst)) #t]
                 [else #f]))
        glst)))

;; Our implementation takes input graphs and turns them into
;; PageRanks. A PageRank is a Racket hash-map that maps pages (each 
;; represented as a Racket symbol) to their corresponding weights,
;; where those weights must sum to 1 (over the whole map).
;; A PageRank encodes a discrete probability distribution over pages.
;;
;; The test graphs for this assignment adhere to several constraints:
;; + There are no "terminal" nodes. All nodes link to at least one
;; other node.
;; + There are no "self-edges," i.e., there will never be an edge `(n0
;; n0).
;; + To maintain consistenty with the last two facts, each graph will
;; have at least two nodes.
;; + There will be no "repeat" edges. I.e., if `(n0 n1) appears once
;; in the graph, it will not appear a second time.
;;
;; (-> any? boolean?)
(define (pagerank? pr)
  (and (hash? pr)
       (andmap symbol? (hash-keys pr))
       (andmap rational? (hash-values pr))
       ;; All the values in the PageRank must sum to 1. I.e., the
       ;; PageRank forms a probability distribution.
       (= 1 (foldl + 0 (hash-values pr)))))

;; Takes some input graph and computes the number of pages in the
;; graph. For example, the graph '((n0 n1) (n1 n2)) has 3 pages, n0,
;; n1, and n2.
;;
;; (-> graph? nonnegative-integer?)
(define (comb-pages graph)
  (cond
    [(empty? graph) '()]
    [(append (first graph) (comb-pages (rest graph)))]))

(define (num-pages graph)
   (length (remove-duplicates (comb-pages graph))))

;; Takes some input graph and computes the number of links emanating
;; from page. For example, (num-links '((n0 n1) (n1 n0) (n0 n2)) 'n0)
;; should return 2, as 'n0 links to 'n1 and 'n2.
;;
;; (-> graph? symbol? nonnegative-integer?

(define (num-links graph page)
  (define (helper g p acc)
    (match g
    ['() acc]
    [`(,t0 . ,t1) (if (equal? p (first t0)) (helper t1 p (+ 1 acc))
                      (helper t1 p acc))]))
  (helper graph page 0))
  

;; Calculates a set of pages that link to page within graph. For
;; example, (get-backlinks '((n0 n1) (n1 n2) (n0 n2)) n2) should
;; return (set 'n0 'n1).
;;
;; (-> graph? symbol? (set/c symbol?))

(define (backlinks graph page)
  (match graph
    ['() '()]
    [`(,t0 . ,t1) (if (equal? (last t0) page) (append (list(first t0)) (backlinks t1 page)) (backlinks t1 page))]))

(define (get-backlinks graph page)
  (list->set (backlinks graph page)))

;; Generate an initial pagerank for the input graph g. The returned
;; PageRank must satisfy pagerank?, and each value of the hash must be
;; equal to (/ 1 N), where N is the number of pages in the given
;; graph.
;; (-> graph? pagerank?)

(define (get-pages1 graph)
  (remove-duplicates (comb-pages graph)))

(define (make-hash1 graph len)
  (match graph
    ['() '()]
    [`(,t1 . ,t0) (cons (cons t1 len) (make-hash1 t0 len))]))


(define (mk-initial-pagerank graph)
  (make-hash (make-hash1 (get-pages1 graph) (/ 1 (length (get-pages1 graph))))))

;; Perform one step of PageRank on the specified graph. Return a new
;; PageRank with updated values after running the PageRank
;; calculation. The next iteration's PageRank is calculated as
;;
;; NextPageRank(page-i) = (1 - d) / N + d * S
;;
;; Where:
;;  + d is a specified "dampening factor." in range [0,1]; e.g., 0.85
;;  + N is the number of pages in the graph
;;  + S is the sum of P(page-j) for all page-j.
;;  + P(page-j) is CurrentPageRank(page-j)/NumLinks(page-j)
;;  + NumLinks(page-j) is the number of outbound links of page-j
;;  (i.e., the number of pages to which page-j has links).
;;
;; (-> pagerank? rational? graph? pagerank?)



(define test-hash '#hash((n0 . 1/5) (n1 . 1/5) (n2 . 1/5) (n3 . 1/5) (n4 . 1/5)))
(define test-graph '((n2 n0)
             (n1 n4)
             (n4 n0)
             (n1 n3)
             (n2 n1)
             (n0 n1)
             (n3 n4)
             (n0 n4)
             (n4 n1)
             (n4 n2)
             (n1 n0)))




(define (get-sum pr page graph d)
  (let ([lst (backlinks graph page)])
    (define (h lst page graph d)
      (match lst
        ['() (/ (- 1 (/ 85 100)) (num-pages graph))]
        [`(,t1 . ,t2) (+ (* d (/ (hash-ref pr t1) (num-links graph t1))) (h t2 page graph d))]))
    (h lst page graph d)))


(define (step-pagerank pr d graph)
  (let ([lst (hash->list pr)])
    (define (h lst d graph)
      (match lst
        ['() '()]
        [`((,t1 . ,t2) . ,t3) (cons (cons t1 (get-sum pr t1 graph d)) (h t3 d graph))]))
    (make-hash (h lst d graph))))



;; Iterate PageRank until the largest change in any page's rank is
;; smaller than a specified delta.
;;
;; To explain the reasoning behind this function: the PageRank step
;; function is constructed so that it converges to some "final" result
;; via a long series of steps. In practice, PageRank is iterated some
;; large number of times. Because our computers use finite
;; approximations, we often only want to iterate an equation until it
;; reaches some delta within true convergence. This function allows us
;; to do that for PageRanks.
;;
;; (-> pagerank? rational? graph? rational? pagerank?)


(define (get-keys-list pr)
  (hash-keys pr))

(define (check-val keys pr delta d graph)

  (define (it-keys k)
     (if (< (- (hash-ref (step-pagerank pr d graph) (first k))
                (hash-ref pr (first k))) delta) #t (it-keys (rest k))))
 
  (define (step pr delta d graph)
    (if (it-keys k) pr (step-pagerank pr d graph))))

    
  



(define (iterate-pagerank-until pr d graph delta)
    ())
  

;; Given a PageRank, returns the list of pages it contains in ranked
;; order (from least-popular to most-popular) as a list. You may
;; assume that the none of the pages in the pagerank have the same
;; value (i.e., there will be no ambiguity in ranking)
;;
;; (-> pagerank? (listof symbol?))
(define (rank-pages pr)
  'todo)
