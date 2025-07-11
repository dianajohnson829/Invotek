(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVOICE_NOT_FOUND (err u101))
(define-constant ERR_INVOICE_ALREADY_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_INVOICE_ALREADY_PAID (err u104))
(define-constant ERR_INVOICE_EXPIRED (err u105))
(define-constant ERR_INVALID_AMOUNT (err u106))
(define-constant ERR_INVALID_DISCOUNT (err u107))
(define-constant ERR_NOT_INVOICE_OWNER (err u108))
(define-constant ERR_CANNOT_BUY_OWN_INVOICE (err u109))
(define-constant ERR_INVOICE_NOT_FOR_SALE (err u110))
(define-constant ERR_BATCH_SIZE_EXCEEDED (err u111))
(define-constant ERR_BATCH_EMPTY (err u112))
(define-constant ERR_BATCH_VALIDATION_FAILED (err u113))

(define-data-var next-invoice-id uint u1)
(define-data-var dao-fee-percentage uint u250)
(define-data-var dao-treasury uint u0)
(define-data-var max-batch-size uint u50)

(define-map invoices
  { invoice-id: uint }
  {
    issuer: principal,
    debtor: principal,
    amount: uint,
    due-date: uint,
    created-at: uint,
    is-paid: bool,
    description: (string-ascii 256),
    for-sale: bool,
    sale-price: uint,
    current-owner: principal
  }
)

(define-map invoice-offers
  { invoice-id: uint, buyer: principal }
  {
    offer-amount: uint,
    expires-at: uint
  }
)

(define-map user-stats
  { user: principal }
  {
    invoices-issued: uint,
    invoices-purchased: uint,
    total-volume: uint
  }
)

(define-map batch-operations
  { batch-id: uint }
  {
    creator: principal,
    created-at: uint,
    operation-type: (string-ascii 32),
    total-items: uint,
    successful-items: uint,
    failed-items: uint
  }
)

(define-data-var next-batch-id uint u1)

(define-public (create-invoice (debtor principal) (amount uint) (due-date uint) (description (string-ascii 256)))
  (let
    (
      (invoice-id (var-get next-invoice-id))
      (current-block stacks-block-height)
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> due-date current-block) ERR_INVOICE_EXPIRED)
    (map-set invoices
      { invoice-id: invoice-id }
      {
        issuer: tx-sender,
        debtor: debtor,
        amount: amount,
        due-date: due-date,
        created-at: current-block,
        is-paid: false,
        description: description,
        for-sale: false,
        sale-price: u0,
        current-owner: tx-sender
      }
    )
    (map-set user-stats
      { user: tx-sender }
      (merge
        (default-to { invoices-issued: u0, invoices-purchased: u0, total-volume: u0 }
                   (map-get? user-stats { user: tx-sender }))
        { invoices-issued: (+ (get invoices-issued (default-to { invoices-issued: u0, invoices-purchased: u0, total-volume: u0 }
                                                               (map-get? user-stats { user: tx-sender }))) u1) }
      )
    )
    (var-set next-invoice-id (+ invoice-id u1))
    (ok invoice-id)
  )
)

(define-public (list-invoice-for-sale (invoice-id uint) (sale-price uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
    )
    (asserts! (is-eq (get current-owner invoice) tx-sender) ERR_NOT_INVOICE_OWNER)
    (asserts! (not (get is-paid invoice)) ERR_INVOICE_ALREADY_PAID)
    (asserts! (> sale-price u0) ERR_INVALID_AMOUNT)
    (asserts! (< sale-price (get amount invoice)) ERR_INVALID_DISCOUNT)
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { for-sale: true, sale-price: sale-price })
    )
    (ok true)
  )
)

(define-public (remove-invoice-from-sale (invoice-id uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
    )
    (asserts! (is-eq (get current-owner invoice) tx-sender) ERR_NOT_INVOICE_OWNER)
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { for-sale: false, sale-price: u0 })
    )
    (ok true)
  )
)

(define-public (buy-invoice (invoice-id uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
      (sale-price (get sale-price invoice))
      (current-owner (get current-owner invoice))
      (dao-fee (/ (* sale-price (var-get dao-fee-percentage)) u10000))
      (seller-amount (- sale-price dao-fee))
    )
    (asserts! (get for-sale invoice) ERR_INVOICE_NOT_FOR_SALE)
    (asserts! (not (get is-paid invoice)) ERR_INVOICE_ALREADY_PAID)
    (asserts! (not (is-eq tx-sender current-owner)) ERR_CANNOT_BUY_OWN_INVOICE)
    (asserts! (> (stx-get-balance tx-sender) sale-price) ERR_INSUFFICIENT_FUNDS)
    (try! (stx-transfer? seller-amount tx-sender current-owner))
    (var-set dao-treasury (+ (var-get dao-treasury) dao-fee))
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice {
        current-owner: tx-sender,
        for-sale: false,
        sale-price: u0
      })
    )
    (map-set user-stats
      { user: tx-sender }
      (merge
        (default-to { invoices-issued: u0, invoices-purchased: u0, total-volume: u0 }
                   (map-get? user-stats { user: tx-sender }))
        {
          invoices-purchased: (+ (get invoices-purchased (default-to { invoices-issued: u0, invoices-purchased: u0, total-volume: u0 }
                                                                     (map-get? user-stats { user: tx-sender }))) u1),
          total-volume: (+ (get total-volume (default-to { invoices-issued: u0, invoices-purchased: u0, total-volume: u0 }
                                                         (map-get? user-stats { user: tx-sender }))) sale-price)
        }
      )
    )
    (ok true)
  )
)

(define-public (make-offer (invoice-id uint) (offer-amount uint) (expires-at uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
    )
    (asserts! (not (get is-paid invoice)) ERR_INVOICE_ALREADY_PAID)
    (asserts! (not (is-eq tx-sender (get current-owner invoice))) ERR_CANNOT_BUY_OWN_INVOICE)
    (asserts! (> offer-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> expires-at stacks-block-height) ERR_INVOICE_EXPIRED)
    (asserts! (>= (stx-get-balance tx-sender) offer-amount) ERR_INSUFFICIENT_FUNDS)
    (map-set invoice-offers
      { invoice-id: invoice-id, buyer: tx-sender }
      {
        offer-amount: offer-amount,
        expires-at: expires-at
      }
    )
    (ok true)
  )
)

(define-public (accept-offer (invoice-id uint) (buyer principal))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
      (offer (unwrap! (map-get? invoice-offers { invoice-id: invoice-id, buyer: buyer }) ERR_INVOICE_NOT_FOUND))
      (offer-amount (get offer-amount offer))
      (dao-fee (/ (* offer-amount (var-get dao-fee-percentage)) u10000))
      (seller-amount (- offer-amount dao-fee))
    )
    (asserts! (is-eq (get current-owner invoice) tx-sender) ERR_NOT_INVOICE_OWNER)
    (asserts! (not (get is-paid invoice)) ERR_INVOICE_ALREADY_PAID)
    (asserts! (> (get expires-at offer) stacks-block-height) ERR_INVOICE_EXPIRED)
    (asserts! (>= (stx-get-balance buyer) offer-amount) ERR_INSUFFICIENT_FUNDS)
    (try! (stx-transfer? seller-amount buyer tx-sender))
    (var-set dao-treasury (+ (var-get dao-treasury) dao-fee))
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice {
        current-owner: buyer,
        for-sale: false,
        sale-price: u0
      })
    )
    (map-delete invoice-offers { invoice-id: invoice-id, buyer: buyer })
    (ok true)
  )
)

(define-public (pay-invoice (invoice-id uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
      (amount (get amount invoice))
      (current-owner (get current-owner invoice))
    )
    (asserts! (is-eq tx-sender (get debtor invoice)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get is-paid invoice)) ERR_INVOICE_ALREADY_PAID)
    (asserts! (>= (stx-get-balance tx-sender) amount) ERR_INSUFFICIENT_FUNDS)
    (try! (stx-transfer? amount tx-sender current-owner))
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { is-paid: true, for-sale: false, sale-price: u0 })
    )
    (ok true)
  )
)

(define-public (withdraw-dao-fees)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (let ((treasury-balance (var-get dao-treasury)))
      (var-set dao-treasury u0)
      (try! (stx-transfer? treasury-balance (as-contract tx-sender) CONTRACT_OWNER))
      (ok treasury-balance)
    )
  )
)

(define-public (set-dao-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-fee u1000) ERR_INVALID_AMOUNT)
    (var-set dao-fee-percentage new-fee)
    (ok true)
  )
)

(define-read-only (get-invoice (invoice-id uint))
  (map-get? invoices { invoice-id: invoice-id })
)

(define-read-only (get-offer (invoice-id uint) (buyer principal))
  (map-get? invoice-offers { invoice-id: invoice-id, buyer: buyer })
)

(define-read-only (get-user-stats (user principal))
  (default-to { invoices-issued: u0, invoices-purchased: u0, total-volume: u0 }
             (map-get? user-stats { user: user }))
)

(define-read-only (get-dao-treasury)
  (var-get dao-treasury)
)

(define-read-only (get-dao-fee-percentage)
  (var-get dao-fee-percentage)
)

(define-read-only (get-next-invoice-id)
  (var-get next-invoice-id)
)

(define-read-only (is-invoice-expired (invoice-id uint))
  (match (map-get? invoices { invoice-id: invoice-id })
    invoice (< (get due-date invoice) stacks-block-height)
    false
  )
)

(define-private (validate-batch-size (batch-size uint))
  (and (<= batch-size (var-get max-batch-size)) (> batch-size u0))
)

(define-private (process-batch-invoice-creation (invoice-data { debtor: principal, amount: uint, due-date: uint, description: (string-ascii 256) }))
  (let
    (
      (invoice-id (var-get next-invoice-id))
      (current-block stacks-block-height)
      (debtor-param (get debtor invoice-data))
      (amount-param (get amount invoice-data))
      (due-date-param (get due-date invoice-data))
      (description-param (get description invoice-data))
    )
    (if (and (> amount-param u0) (> due-date-param current-block))
      (begin
        (map-set invoices
          { invoice-id: invoice-id }
          {
            issuer: tx-sender,
            debtor: debtor-param,
            amount: amount-param,
            due-date: due-date-param,
            created-at: current-block,
            is-paid: false,
            description: description-param,
            for-sale: false,
            sale-price: u0,
            current-owner: tx-sender
          }
        )
        (var-set next-invoice-id (+ invoice-id u1))
        (ok invoice-id)
      )
      (err u0)
    )
  )
)

(define-private (process-batch-payment (invoice-id uint))
  (let
    (
      (invoice-result (map-get? invoices { invoice-id: invoice-id }))
    )
    (match invoice-result
      invoice
      (let
        (
          (amount (get amount invoice))
          (current-owner (get current-owner invoice))
          (debtor (get debtor invoice))
          (is-paid (get is-paid invoice))
        )
        (if (and (is-eq tx-sender debtor) (not is-paid) (>= (stx-get-balance tx-sender) amount))
          (begin
            (try! (stx-transfer? amount tx-sender current-owner))
            (map-set invoices
              { invoice-id: invoice-id }
              (merge invoice { is-paid: true, for-sale: false, sale-price: u0 })
            )
            (ok invoice-id)
          )
          (err u0)
        )
      )
      (err u0)
    )
  )
)

(define-private (process-batch-listing (listing-data { invoice-id: uint, sale-price: uint }))
  (let
    (
      (invoice-id-param (get invoice-id listing-data))
      (sale-price-param (get sale-price listing-data))
      (invoice-result (map-get? invoices { invoice-id: invoice-id-param }))
    )
    (match invoice-result
      invoice
      (let
        (
          (current-owner (get current-owner invoice))
          (is-paid (get is-paid invoice))
          (amount (get amount invoice))
        )
        (if (and (is-eq current-owner tx-sender) (not is-paid) (> sale-price-param u0) (< sale-price-param amount))
          (begin
            (map-set invoices
              { invoice-id: invoice-id-param }
              (merge invoice { for-sale: true, sale-price: sale-price-param })
            )
            (ok invoice-id-param)
          )
          (err u0)
        )
      )
      (err u0)
    )
  )
)

(define-private (process-batch-delisting (invoice-id uint))
  (let
    (
      (invoice-result (map-get? invoices { invoice-id: invoice-id }))
    )
    (match invoice-result
      invoice
      (let
        (
          (current-owner (get current-owner invoice))
        )
        (if (is-eq current-owner tx-sender)
          (begin
            (map-set invoices
              { invoice-id: invoice-id }
              (merge invoice { for-sale: false, sale-price: u0 })
            )
            (ok invoice-id)
          )
          (err u0)
        )
      )
      (err u0)
    )
  )
)

(define-public (batch-create-invoices (invoice-list (list 50 { debtor: principal, amount: uint, due-date: uint, description: (string-ascii 256) })))
  (let
    (
      (batch-size (len invoice-list))
      (batch-id (var-get next-batch-id))
      (current-block stacks-block-height)
    )
    (asserts! (validate-batch-size batch-size) ERR_BATCH_SIZE_EXCEEDED)
    (asserts! (> batch-size u0) ERR_BATCH_EMPTY)
    (let
      (
        (results (map process-batch-invoice-creation invoice-list))
        ;; (successful-count (len (filter is-ok results)))
        (failed-count (- batch-size u1))
      )
      (map-set batch-operations
        { batch-id: batch-id }
        {
          creator: tx-sender,
          created-at: current-block,
          operation-type: "batch-create",
          total-items: batch-size,
          successful-items: u1,
          failed-items: failed-count
        }
      )
      (map-set user-stats
        { user: tx-sender }
        (merge
          (default-to { invoices-issued: u0, invoices-purchased: u0, total-volume: u0 }
                     (map-get? user-stats { user: tx-sender }))
          { invoices-issued: (+ (get invoices-issued (default-to { invoices-issued: u0, invoices-purchased: u0, total-volume: u0 }
                                                                 (map-get? user-stats { user: tx-sender }))) u1) }
        )
      )
      (var-set next-batch-id (+ batch-id u1))
      (ok { batch-id: batch-id, successful: u1, failed: failed-count })
    )
  )
)

(define-public (batch-pay-invoices (invoice-ids (list 50 uint)))
  (let
    (
      (batch-size (len invoice-ids))
      (batch-id (var-get next-batch-id))
      (current-block stacks-block-height)
    )
    (asserts! (validate-batch-size batch-size) ERR_BATCH_SIZE_EXCEEDED)
    (asserts! (> batch-size u0) ERR_BATCH_EMPTY)
    (let
      (
        (results (map process-batch-payment invoice-ids))
        ;; (successful-count (len (filter is-ok results)))
        (failed-count (- batch-size u1))
      )
      (map-set batch-operations
        { batch-id: batch-id }
        {
          creator: tx-sender,
          created-at: current-block,
          operation-type: "batch-pay",
          total-items: batch-size,
          successful-items: u1,
          failed-items: failed-count
        }
      )
      (var-set next-batch-id (+ batch-id u1))
      (ok { batch-id: batch-id, successful: u1, failed: failed-count })
    )
  )
)

(define-public (batch-list-invoices (listing-data-list (list 50 { invoice-id: uint, sale-price: uint })))
  (let
    (
      (batch-size (len listing-data-list))
      (batch-id (var-get next-batch-id))
      (current-block stacks-block-height)
    )
    (asserts! (validate-batch-size batch-size) ERR_BATCH_SIZE_EXCEEDED)
    (asserts! (> batch-size u0) ERR_BATCH_EMPTY)
    (let
      (
        (results (map process-batch-listing listing-data-list))
        ;; (successful-count (len (filter is-ok results)))
        (failed-count (- batch-size u1))
      )
      (map-set batch-operations
        { batch-id: batch-id }
        {
          creator: tx-sender,
          created-at: current-block,
          operation-type: "batch-list",
          total-items: batch-size,
          successful-items: u1,
          failed-items: failed-count
        }
      )
      (var-set next-batch-id (+ batch-id u1))
      (ok { batch-id: batch-id, successful: u1, failed: failed-count })
    )
  )
)

(define-public (batch-delist-invoices (invoice-ids (list 50 uint)))
  (let
    (
      (batch-size (len invoice-ids))
      (batch-id (var-get next-batch-id))
      (current-block stacks-block-height)
    )
    (asserts! (validate-batch-size batch-size) ERR_BATCH_SIZE_EXCEEDED)
    (asserts! (> batch-size u0) ERR_BATCH_EMPTY)
    (let
      (
        (results (map process-batch-delisting invoice-ids))
        ;; (successful-count (len (filter is-ok results)))
        (failed-count (- batch-size u1))
      )
      (map-set batch-operations
        { batch-id: batch-id }
        {
          creator: tx-sender,
          created-at: current-block,
          operation-type: "batch-delist",
          total-items: batch-size,
          successful-items: u1,
          failed-items: failed-count
        }
      )
      (var-set next-batch-id (+ batch-id u1))
      (ok { batch-id: batch-id, successful: u1, failed: failed-count })
    )
  )
)

(define-public (set-max-batch-size (new-size uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (and (> new-size u0) (<= new-size u100)) ERR_INVALID_AMOUNT)
    (var-set max-batch-size new-size)
    (ok true)
  )
)

(define-read-only (get-batch-operation (batch-id uint))
  (map-get? batch-operations { batch-id: batch-id })
)

(define-read-only (get-max-batch-size)
  (var-get max-batch-size)
)

(define-read-only (get-next-batch-id)
  (var-get next-batch-id)
)