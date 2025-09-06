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
(define-constant ERR_AUCTION_NOT_FOUND (err u114))
(define-constant ERR_AUCTION_ENDED (err u115))
(define-constant ERR_AUCTION_NOT_ENDED (err u116))
(define-constant ERR_BID_TOO_LOW (err u117))
(define-constant ERR_AUCTION_ALREADY_EXISTS (err u118))
(define-constant ERR_INVALID_AUCTION_DURATION (err u119))
(define-constant ERR_CANNOT_BID_ON_OWN_AUCTION (err u120))

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

(define-map invoice-auctions
  { auction-id: uint }
  {
    invoice-id: uint,
    seller: principal,
    start-price: uint,
    reserve-price: uint,
    current-bid: uint,
    current-bidder: (optional principal),
    start-block: uint,
    end-block: uint,
    total-bids: uint,
    is-active: bool
  }
)

(define-map auction-bids
  { auction-id: uint, bidder: principal, bid-index: uint }
  {
    bid-amount: uint,
    bid-block: uint
  }
)

(define-data-var next-batch-id uint u1)
(define-data-var next-auction-id uint u1)
(define-data-var min-bid-increment uint u1000)
(define-data-var min-auction-duration uint u144)
(define-data-var max-auction-duration uint u4320)

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

(define-public (create-auction (invoice-id uint) (start-price uint) (reserve-price uint) (duration-blocks uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
      (auction-id (var-get next-auction-id))
      (current-block stacks-block-height)
      (end-block (+ current-block duration-blocks))
    )
    (asserts! (is-eq (get current-owner invoice) tx-sender) ERR_NOT_INVOICE_OWNER)
    (asserts! (not (get is-paid invoice)) ERR_INVOICE_ALREADY_PAID)
    (asserts! (not (get for-sale invoice)) ERR_INVOICE_ALREADY_EXISTS)
    (asserts! (> start-price u0) ERR_INVALID_AMOUNT)
    (asserts! (>= reserve-price start-price) ERR_INVALID_AMOUNT)
    (asserts! (>= duration-blocks (var-get min-auction-duration)) ERR_INVALID_AUCTION_DURATION)
    (asserts! (<= duration-blocks (var-get max-auction-duration)) ERR_INVALID_AUCTION_DURATION)

    (map-set invoice-auctions
      { auction-id: auction-id }
      {
        invoice-id: invoice-id,
        seller: tx-sender,
        start-price: start-price,
        reserve-price: reserve-price,
        current-bid: u0,
        current-bidder: none,
        start-block: current-block,
        end-block: end-block,
        total-bids: u0,
        is-active: true
      }
    )
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { for-sale: true, sale-price: start-price })
    )
    (var-set next-auction-id (+ auction-id u1))
    (ok auction-id)
  )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
  (let
    (
      (auction (unwrap! (map-get? invoice-auctions { auction-id: auction-id }) ERR_AUCTION_NOT_FOUND))
      (current-block stacks-block-height)
      (current-bid (get current-bid auction))
      (min-bid (if (is-eq current-bid u0) 
                   (get start-price auction)
                   (+ current-bid (var-get min-bid-increment))))
      (total-bids (get total-bids auction))
      (bid-index total-bids)
    )
    (asserts! (get is-active auction) ERR_AUCTION_ENDED)
    (asserts! (< current-block (get end-block auction)) ERR_AUCTION_ENDED)
    (asserts! (not (is-eq tx-sender (get seller auction))) ERR_CANNOT_BID_ON_OWN_AUCTION)
    (asserts! (>= bid-amount min-bid) ERR_BID_TOO_LOW)
    (asserts! (>= (stx-get-balance tx-sender) bid-amount) ERR_INSUFFICIENT_FUNDS)
    (map-set auction-bids
      { auction-id: auction-id, bidder: tx-sender, bid-index: bid-index }
      {
        bid-amount: bid-amount,
        bid-block: current-block
      }
    )
    (map-set invoice-auctions
      { auction-id: auction-id }
      (merge auction {
        current-bid: bid-amount,
        current-bidder: (some tx-sender),
        total-bids: (+ total-bids u1)
      })
    )
    (ok true)
  )
)

(define-public (end-auction (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? invoice-auctions { auction-id: auction-id }) ERR_AUCTION_NOT_FOUND))
      (current-block stacks-block-height)
      (invoice-id (get invoice-id auction))
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
      (current-bid (get current-bid auction))
      (reserve-price (get reserve-price auction))
      (seller (get seller auction))
    )
    (asserts! (get is-active auction) ERR_AUCTION_ENDED)
    (asserts! (>= current-block (get end-block auction)) ERR_AUCTION_NOT_ENDED)
    (if (and (> current-bid u0) (>= current-bid reserve-price))
      (let
        (
          (winner (unwrap! (get current-bidder auction) ERR_AUCTION_NOT_FOUND))
          (dao-fee (/ (* current-bid (var-get dao-fee-percentage)) u10000))
          (seller-amount (- current-bid dao-fee))
        )
        (try! (stx-transfer? seller-amount winner seller))
        (var-set dao-treasury (+ (var-get dao-treasury) dao-fee))
        (map-set invoices
          { invoice-id: invoice-id }
          (merge invoice {
            current-owner: winner,
            for-sale: false,
            sale-price: u0
          })
        )
        (map-set user-stats
          { user: winner }
          (merge
            (default-to { invoices-issued: u0, invoices-purchased: u0, total-volume: u0 }
                       (map-get? user-stats { user: winner }))
            {
              invoices-purchased: (+ (get invoices-purchased (default-to { invoices-issued: u0, invoices-purchased: u0, total-volume: u0 }
                                                                         (map-get? user-stats { user: winner }))) u1),
              total-volume: (+ (get total-volume (default-to { invoices-issued: u0, invoices-purchased: u0, total-volume: u0 }
                                                             (map-get? user-stats { user: winner }))) current-bid)
            }
          )
        )
        (map-set invoice-auctions
          { auction-id: auction-id }
          (merge auction { is-active: false })
        )
        (ok true)
      )
      (begin
        (map-set invoices
          { invoice-id: invoice-id }
          (merge invoice { for-sale: false, sale-price: u0 })
        )
        (map-set invoice-auctions
          { auction-id: auction-id }
          (merge auction { is-active: false })
        )
        (ok false)
      )
    )
  )
)

(define-public (cancel-auction (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? invoice-auctions { auction-id: auction-id }) ERR_AUCTION_NOT_FOUND))
      (invoice-id (get invoice-id auction))
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get seller auction)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active auction) ERR_AUCTION_ENDED)
    (asserts! (is-eq (get total-bids auction) u0) ERR_NOT_AUTHORIZED)
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { for-sale: false, sale-price: u0 })
    )
    (map-set invoice-auctions
      { auction-id: auction-id }
      (merge auction { is-active: false })
    )
    (ok true)
  )
)

(define-public (set-auction-parameters (new-min-increment uint) (new-min-duration uint) (new-max-duration uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> new-min-increment u0) ERR_INVALID_AMOUNT)
    (asserts! (> new-min-duration u0) ERR_INVALID_AUCTION_DURATION)
    (asserts! (> new-max-duration new-min-duration) ERR_INVALID_AUCTION_DURATION)
    (var-set min-bid-increment new-min-increment)
    (var-set min-auction-duration new-min-duration)
    (var-set max-auction-duration new-max-duration)
    (ok true)
  )
)

(define-read-only (get-auction (auction-id uint))
  (map-get? invoice-auctions { auction-id: auction-id })
)

(define-read-only (get-auction-bid (auction-id uint) (bidder principal) (bid-index uint))
  (map-get? auction-bids { auction-id: auction-id, bidder: bidder, bid-index: bid-index })
)

(define-read-only (get-auction-parameters)
  {
    min-bid-increment: (var-get min-bid-increment),
    min-auction-duration: (var-get min-auction-duration),
    max-auction-duration: (var-get max-auction-duration),
    next-auction-id: (var-get next-auction-id)
  }
)

(define-read-only (is-auction-active (auction-id uint))
  (match (map-get? invoice-auctions { auction-id: auction-id })
    auction (and (get is-active auction) (< stacks-block-height (get end-block auction)))
    false
  )
)




