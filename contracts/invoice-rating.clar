;; Invoice Rating System
;; Tracks debtor reputation and payment reliability for informed investment decisions

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_INVOICE_NOT_FOUND (err u201))
(define-constant ERR_INVALID_RATING (err u202))
(define-constant ERR_ALREADY_RATED (err u203))
(define-constant ERR_CANNOT_RATE_SELF (err u204))
(define-constant ERR_PAYMENT_NOT_RECORDED (err u205))
(define-constant ERR_RATING_NOT_FOUND (err u206))

;; Data variables
(define-data-var total-ratings uint u0)
(define-data-var rating-weight-factor uint u100)

;; Debtor reputation tracking
(define-map debtor-reputation
  principal
  {
    total-ratings: uint,
    average-rating: uint,
    total-invoices: uint,
    paid-invoices: uint,
    late-payments: uint,
    reputation-score: uint,
    last-payment: uint
  }
)

;; Individual ratings for invoices
(define-map invoice-ratings
  { invoice-id: uint, rater: principal }
  {
    rating: uint,
    comment: (string-ascii 140),
    rated-at: uint,
    payment-timeliness: uint,
    debtor: principal
  }
)

;; Payment behavior tracking
(define-map payment-records
  { debtor: principal, invoice-id: uint }
  {
    paid-at: uint,
    due-date: uint,
    was-late: bool,
    days-overdue: uint
  }
)

;; Rating aggregates by debtor
(define-map rating-summary
  principal
  {
    one-star: uint,
    two-star: uint,
    three-star: uint,
    four-star: uint,
    five-star: uint,
    total-score: uint
  }
)

;; Submit a rating for a debtor after invoice payment
(define-public (rate-debtor 
  (invoice-id uint) 
  (debtor principal) 
  (rating uint) 
  (comment (string-ascii 140))
  (payment-timeliness uint))
  (let (
    (rating-key { invoice-id: invoice-id, rater: tx-sender })
    (current-block stacks-block-height)
    (current-reputation (default-to 
      { total-ratings: u0, average-rating: u0, total-invoices: u0, paid-invoices: u0, 
        late-payments: u0, reputation-score: u0, last-payment: u0 }
      (map-get? debtor-reputation debtor)))
    (current-summary (default-to
      { one-star: u0, two-star: u0, three-star: u0, four-star: u0, five-star: u0, total-score: u0 }
      (map-get? rating-summary debtor)))
  )
    ;; Validations
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
    (asserts! (not (is-eq tx-sender debtor)) ERR_CANNOT_RATE_SELF)
    (asserts! (is-none (map-get? invoice-ratings rating-key)) ERR_ALREADY_RATED)
    (asserts! (and (>= payment-timeliness u1) (<= payment-timeliness u5)) ERR_INVALID_RATING)
    
    ;; Store the rating
    (map-set invoice-ratings rating-key
      {
        rating: rating,
        comment: comment,
        rated-at: current-block,
        payment-timeliness: payment-timeliness,
        debtor: debtor
      })
    
    ;; Update rating summary
    (let ((new-summary (update-rating-summary current-summary rating)))
      (map-set rating-summary debtor new-summary)
      
      ;; Update debtor reputation
      (let ((new-total-ratings (+ (get total-ratings current-reputation) u1))
            (new-total-score (+ (get total-score new-summary) u0))
            (new-average (if (> new-total-ratings u0) 
                           (/ new-total-score new-total-ratings) 
                           u0))
            (reputation-score (calculate-reputation-score 
                               new-average 
                               new-total-ratings 
                               (get paid-invoices current-reputation)
                               (get late-payments current-reputation))))
        
        (map-set debtor-reputation debtor
          (merge current-reputation {
            total-ratings: new-total-ratings,
            average-rating: new-average,
            reputation-score: reputation-score
          }))
      ))
    
    ;; Update global counter
    (var-set total-ratings (+ (var-get total-ratings) u1))
    (ok true)
  ))

;; Record payment behavior when invoice is paid
(define-public (record-payment 
  (invoice-id uint) 
  (debtor principal) 
  (due-date uint))
  (let (
    (current-block stacks-block-height)
    (was-late (> current-block due-date))
    (days-overdue (if was-late (- current-block due-date) u0))
    (current-reputation (default-to 
      { total-ratings: u0, average-rating: u0, total-invoices: u0, paid-invoices: u0, 
        late-payments: u0, reputation-score: u0, last-payment: u0 }
      (map-get? debtor-reputation debtor)))
  )
    
    ;; Record payment details
    (map-set payment-records
      { debtor: debtor, invoice-id: invoice-id }
      {
        paid-at: current-block,
        due-date: due-date,
        was-late: was-late,
        days-overdue: days-overdue
      })
    
    ;; Update debtor reputation with payment info
    (map-set debtor-reputation debtor
      (merge current-reputation {
        paid-invoices: (+ (get paid-invoices current-reputation) u1),
        late-payments: (if was-late 
                         (+ (get late-payments current-reputation) u1)
                         (get late-payments current-reputation)),
        last-payment: current-block
      }))
    
    (ok true)
  ))

;; Private function to update rating summary
(define-private (update-rating-summary (current-summary { one-star: uint, two-star: uint, three-star: uint, four-star: uint, five-star: uint, total-score: uint }) (rating uint))
  (let ((new-total-score (+ (get total-score current-summary) rating)))
    (if (is-eq rating u1)
      (merge current-summary { one-star: (+ (get one-star current-summary) u1), total-score: new-total-score })
      (if (is-eq rating u2)
        (merge current-summary { two-star: (+ (get two-star current-summary) u1), total-score: new-total-score })
        (if (is-eq rating u3)
          (merge current-summary { three-star: (+ (get three-star current-summary) u1), total-score: new-total-score })
          (if (is-eq rating u4)
            (merge current-summary { four-star: (+ (get four-star current-summary) u1), total-score: new-total-score })
            (merge current-summary { five-star: (+ (get five-star current-summary) u1), total-score: new-total-score })))))))

;; Private function to calculate reputation score
(define-private (calculate-reputation-score (average-rating uint) (rating-count uint) (paid-invoices uint) (late-payments uint))
  (let (
    (rating-score (* average-rating u20))
    (volume-bonus (if (< (* rating-count u2) u20) (* rating-count u2) u20))
    (payment-rate (if (> paid-invoices u0) 
                    (* (/ (* (- paid-invoices late-payments) u100) paid-invoices) u40) 
                    u0))
    (recency-bonus u20)
  )
    (+ rating-score volume-bonus payment-rate recency-bonus)
  ))

;; Read-only functions

;; Get debtor reputation summary
(define-read-only (get-debtor-reputation (debtor principal))
  (map-get? debtor-reputation debtor))

;; Get specific invoice rating
(define-read-only (get-invoice-rating (invoice-id uint) (rater principal))
  (map-get? invoice-ratings { invoice-id: invoice-id, rater: rater }))

;; Get payment record for specific invoice
(define-read-only (get-payment-record (debtor principal) (invoice-id uint))
  (map-get? payment-records { debtor: debtor, invoice-id: invoice-id }))

;; Get rating breakdown for debtor
(define-read-only (get-rating-summary (debtor principal))
  (map-get? rating-summary debtor))

;; Get debtor reputation score only
(define-read-only (get-reputation-score (debtor principal))
  (match (map-get? debtor-reputation debtor)
    reputation (get reputation-score reputation)
    u0))

;; Check if user can rate specific invoice
(define-read-only (can-rate-invoice (invoice-id uint) (debtor principal))
  (and 
    (not (is-eq tx-sender debtor))
    (is-none (map-get? invoice-ratings { invoice-id: invoice-id, rater: tx-sender }))))

;; Get payment reliability percentage
(define-read-only (get-payment-reliability (debtor principal))
  (match (map-get? debtor-reputation debtor)
    reputation 
    (let ((paid (get paid-invoices reputation))
          (late (get late-payments reputation)))
      (if (> paid u0)
        (/ (* (- paid late) u100) paid)
        u0))
    u0))

;; Get total system ratings
(define-read-only (get-total-ratings)
  (var-get total-ratings))

;; Get debtor risk assessment
(define-read-only (get-risk-assessment (debtor principal))
  (let ((reputation (get-debtor-reputation debtor)))
    (match reputation
      rep (let ((score (get reputation-score rep))
                (avg-rating (get average-rating rep))
                (reliability (get-payment-reliability debtor)))
        {
          risk-level: (if (>= score u80) "low" 
                        (if (>= score u60) "medium" 
                          (if (>= score u40) "high" "very-high"))),
          reputation-score: score,
          average-rating: avg-rating,
          payment-reliability: reliability,
          total-invoices: (get total-invoices rep),
          late-payment-rate: (if (> (get paid-invoices rep) u0)
                               (/ (* (get late-payments rep) u100) (get paid-invoices rep))
                               u0)
        })
      { risk-level: "unknown", reputation-score: u0, average-rating: u0, 
        payment-reliability: u0, total-invoices: u0, late-payment-rate: u0 })))
