;; CacheVibe - Decentralized Impact Investing Platform
;; Stacks Clarity Smart Contract
;; Version: 1.0.0

;; ===========================
;; CONSTANTS
;; ===========================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-POOL-NOT-FOUND (err u101))
(define-constant ERR-PROJECT-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-INVALID-MILESTONE (err u104))
(define-constant ERR-ALREADY-VALIDATED (err u105))
(define-constant ERR-NOT-VALIDATOR (err u106))
(define-constant ERR-POOL-CLOSED (err u107))
(define-constant ERR-INVALID-SCORE (err u108))
(define-constant ERR-INVALID-CATEGORY (err u109))
(define-constant ERR-MILESTONE-ALREADY-COMPLETE (err u110))
(define-constant ERR-BOND-NOT-FOUND (err u111))
(define-constant ERR-BOND-NOT-FOR-SALE (err u112))
(define-constant ERR-BELOW-MIN-STAKE (err u113))

;; Impact Categories
(define-constant CATEGORY-EDUCATION u1)
(define-constant CATEGORY-HEALTHCARE u2)
(define-constant CATEGORY-ENVIRONMENT u3)
(define-constant CATEGORY-ECONOMIC u4)

;; Pool / Project statuses
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-PAUSED u2)
(define-constant STATUS-COMPLETED u3)
(define-constant STATUS-CANCELLED u4)

;; Validation thresholds
(define-constant MIN-VALIDATOR-STAKE u1000000)     ;; 1 STX (in micro-STX)
(define-constant VALIDATION-QUORUM u3)             ;; min validators needed per milestone
(define-constant BASE-FEE-BPS u200)                ;; 2.00% base platform fee (basis points)
(define-constant MAX-VIBE-SCORE u1000)             ;; max vibe score (0-1000)
(define-constant GOVERNANCE-REWARD u500000)        ;; 0.5 STX reward per validation

;; ===========================
;; DATA VARS
;; ===========================

(define-data-var pool-nonce uint u0)
(define-data-var project-nonce uint u0)
(define-data-var bond-nonce uint u0)
(define-data-var platform-treasury uint u0)

;; ===========================
;; FUNGIBLE TOKENS
;; ===========================

;; Governance token earned by validators
(define-fungible-token VIBE-GOV)

;; Impact bond token (each unit = 1 micro-STX of pool share)
(define-fungible-token IMPACT-BOND)

;; ===========================
;; DATA MAPS
;; ===========================

;; Impact Pools
(define-map impact-pools
  { pool-id: uint }
  {
    creator: principal,
    name: (string-ascii 64),
    category: uint,
    total-capital: uint,          ;; total STX deposited (micro-STX)
    allocated-capital: uint,      ;; STX allocated to projects
    released-capital: uint,       ;; STX released to projects
    investor-count: uint,
    status: uint,
    vibe-score: uint,             ;; 0-1000
    created-at: uint              ;; block height
  }
)

;; Investor positions in pools
(define-map pool-investments
  { pool-id: uint, investor: principal }
  {
    amount: uint,
    bond-tokens: uint,
    invested-at: uint
  }
)

;; Projects funded by pools
(define-map projects
  { project-id: uint }
  {
    pool-id: uint,
    lead: principal,
    name: (string-ascii 64),
    category: uint,
    description: (string-utf8 256),
    total-budget: uint,
    disbursed: uint,
    milestone-count: uint,
    completed-milestones: uint,
    status: uint,
    vibe-score: uint,
    created-at: uint
  }
)

;; Milestones within a project
(define-map milestones
  { project-id: uint, milestone-id: uint }
  {
    description: (string-utf8 128),
    target-metric: (string-ascii 64),   ;; e.g. "students-enrolled"
    target-value: uint,                 ;; numeric target
    release-amount: uint,               ;; STX to release on completion
    validation-count: uint,             ;; how many validators approved
    rejection-count: uint,
    is-complete: bool,
    deadline: uint                      ;; block height deadline
  }
)

;; Validator registry (staking to participate in oracle network)
(define-map validators
  { validator: principal }
  {
    staked-amount: uint,
    validations-completed: uint,
    reputation-score: uint,    ;; 0-1000
    is-active: bool,
    joined-at: uint
  }
)

;; Per-milestone validation records (prevents double-voting)
(define-map milestone-validations
  { project-id: uint, milestone-id: uint, validator: principal }
  {
    approved: bool,
    reported-value: uint,       ;; actual measured value reported
    sentiment-score: uint,      ;; 0-100 community sentiment
    validated-at: uint
  }
)

;; Secondary market: impact bond listings
(define-map bond-listings
  { listing-id: uint }
  {
    seller: principal,
    pool-id: uint,
    bond-amount: uint,          ;; IMPACT-BOND tokens for sale
    ask-price: uint,            ;; STX price for total bond-amount
    listed-at: uint,
    is-active: bool
  }
)

;; ===========================
;; READ-ONLY HELPERS
;; ===========================

(define-read-only (get-pool (pool-id uint))
  (map-get? impact-pools { pool-id: pool-id })
)

(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

(define-read-only (get-milestone (project-id uint) (milestone-id uint))
  (map-get? milestones { project-id: project-id, milestone-id: milestone-id })
)

(define-read-only (get-validator (addr principal))
  (map-get? validators { validator: addr })
)

(define-read-only (get-investment (pool-id uint) (investor principal))
  (map-get? pool-investments { pool-id: pool-id, investor: investor })
)

(define-read-only (get-bond-listing (listing-id uint))
  (map-get? bond-listings { listing-id: listing-id })
)

(define-read-only (get-platform-treasury)
  (var-get platform-treasury)
)

;; Calculate the platform fee for a given pool (lower fee = higher vibe score)
;; Fee decays from BASE-FEE-BPS down to 25 bps as vibe-score approaches MAX-VIBE-SCORE
(define-read-only (calculate-fee-bps (pool-vibe-score uint))
  (let (
    (score (if (> pool-vibe-score MAX-VIBE-SCORE) MAX-VIBE-SCORE pool-vibe-score))
    (discount (/ (* score u175) MAX-VIBE-SCORE))  ;; up to 175bps discount
  )
    (- BASE-FEE-BPS discount)
  )
)

;; Check if a principal is a valid active validator
(define-read-only (is-active-validator (addr principal))
  (match (map-get? validators { validator: addr })
    v (get is-active v)
    false
  )
)

;; ===========================
;; VALIDATOR REGISTRY
;; ===========================

;; Stake STX to become a community validator (Impact Oracle)
(define-public (register-validator)
  (let (
    (stake (stx-get-balance tx-sender))
    (existing (map-get? validators { validator: tx-sender }))
  )
    (asserts! (is-none existing) ERR-ALREADY-VALIDATED)
    (asserts! (>= (stx-get-balance tx-sender) MIN-VALIDATOR-STAKE) ERR-BELOW-MIN-STAKE)
    (try! (stx-transfer? MIN-VALIDATOR-STAKE tx-sender (as-contract tx-sender)))
    (map-set validators
      { validator: tx-sender }
      {
        staked-amount: MIN-VALIDATOR-STAKE,
        validations-completed: u0,
        reputation-score: u500,
        is-active: true,
        joined-at: block-height
      }
    )
    (ok true)
  )
)

;; Validator withdraws stake and deregisters
(define-public (deregister-validator)
  (let (
    (v (unwrap! (map-get? validators { validator: tx-sender }) ERR-NOT-VALIDATOR))
    (stake (get staked-amount v))
  )
    (asserts! (get is-active v) ERR-NOT-VALIDATOR)
    (map-set validators { validator: tx-sender }
      (merge v { is-active: false, staked-amount: u0 })
    )
    (try! (as-contract (stx-transfer? stake tx-sender tx-sender)))
    (ok true)
  )
)

;; ===========================
;; IMPACT POOL MANAGEMENT
;; ===========================

;; Create a new Impact Pool
(define-public (create-impact-pool
    (name (string-ascii 64))
    (category uint)
  )
  (begin
    (asserts!
      (or
        (is-eq category CATEGORY-EDUCATION)
        (is-eq category CATEGORY-HEALTHCARE)
        (is-eq category CATEGORY-ENVIRONMENT)
        (is-eq category CATEGORY-ECONOMIC)
      )
      ERR-INVALID-CATEGORY
    )
    (let ((new-id (+ (var-get pool-nonce) u1)))
      (var-set pool-nonce new-id)
      (map-set impact-pools
        { pool-id: new-id }
        {
          creator: tx-sender,
          name: name,
          category: category,
          total-capital: u0,
          allocated-capital: u0,
          released-capital: u0,
          investor-count: u0,
          status: STATUS-ACTIVE,
          vibe-score: u0,
          created-at: block-height
        }
      )
      (ok new-id)
    )
  )
)

;; Invest STX into a pool; mints IMPACT-BOND tokens proportional to deposit
(define-public (invest-in-pool (pool-id uint) (amount uint))
  (let (
    (pool (unwrap! (map-get? impact-pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND))
    (existing-investment (map-get? pool-investments { pool-id: pool-id, investor: tx-sender }))
    (fee-bps (calculate-fee-bps (get vibe-score pool)))
    (fee-amount (/ (* amount fee-bps) u10000))
    (net-amount (- amount fee-amount))
  )
    (asserts! (is-eq (get status pool) STATUS-ACTIVE) ERR-POOL-CLOSED)
    (asserts! (> amount u0) ERR-INSUFFICIENT-FUNDS)

    ;; Transfer investor funds to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    ;; Accumulate platform fee
    (var-set platform-treasury (+ (var-get platform-treasury) fee-amount))

    ;; Mint IMPACT-BOND tokens 1:1 with net STX deposited
    (try! (ft-mint? IMPACT-BOND net-amount tx-sender))

    ;; Update or create investment record
    (match existing-investment
      inv (map-set pool-investments
            { pool-id: pool-id, investor: tx-sender }
            {
              amount: (+ (get amount inv) net-amount),
              bond-tokens: (+ (get bond-tokens inv) net-amount),
              invested-at: (get invested-at inv)
            }
          )
      (begin
        (map-set pool-investments
          { pool-id: pool-id, investor: tx-sender }
          {
            amount: net-amount,
            bond-tokens: net-amount,
            invested-at: block-height
          }
        )
        ;; Increment investor count only on first investment
        (map-set impact-pools { pool-id: pool-id }
          (merge pool {
            total-capital: (+ (get total-capital pool) net-amount),
            investor-count: (+ (get investor-count pool) u1)
          })
        )
      )
    )

    ;; If existing investor, just update capital total
    (match existing-investment
      inv (map-set impact-pools { pool-id: pool-id }
            (merge pool { total-capital: (+ (get total-capital pool) net-amount) })
          )
      false
    )

    (ok net-amount)
  )
)

;; ===========================
;; PROJECT MANAGEMENT
;; ===========================

;; Register a new project under a pool
(define-public (create-project
    (pool-id uint)
    (name (string-ascii 64))
    (description (string-utf8 256))
    (total-budget uint)
  )
  (let (
    (pool (unwrap! (map-get? impact-pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND))
    (new-id (+ (var-get project-nonce) u1))
  )
    (asserts! (is-eq (get status pool) STATUS-ACTIVE) ERR-POOL-CLOSED)
    ;; Only pool creator or contract owner can add projects
    (asserts!
      (or (is-eq tx-sender (get creator pool)) (is-eq tx-sender CONTRACT-OWNER))
      ERR-NOT-AUTHORIZED
    )
    (asserts! (<= total-budget
      (- (get total-capital pool) (get allocated-capital pool)))
      ERR-INSUFFICIENT-FUNDS
    )
    (var-set project-nonce new-id)
    (map-set projects
      { project-id: new-id }
      {
        pool-id: pool-id,
        lead: tx-sender,
        name: name,
        category: (get category pool),
        description: description,
        total-budget: total-budget,
        disbursed: u0,
        milestone-count: u0,
        completed-milestones: u0,
        status: STATUS-ACTIVE,
        vibe-score: u0,
        created-at: block-height
      }
    )
    ;; Mark capital as allocated (escrowed for this project)
    (map-set impact-pools { pool-id: pool-id }
      (merge pool { allocated-capital: (+ (get allocated-capital pool) total-budget) })
    )
    (ok new-id)
  )
)

;; Add a milestone to a project (project lead only)
(define-public (add-milestone
    (project-id uint)
    (description (string-utf8 128))
    (target-metric (string-ascii 64))
    (target-value uint)
    (release-amount uint)
    (deadline uint)
  )
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (milestone-id (+ (get milestone-count project) u1))
  )
    (asserts! (is-eq tx-sender (get lead project)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS-ACTIVE) ERR-POOL-CLOSED)
    (asserts! (> deadline block-height) ERR-INVALID-MILESTONE)
    (asserts! (<= release-amount
      (- (get total-budget project) (get disbursed project)))
      ERR-INSUFFICIENT-FUNDS
    )
    (map-set milestones
      { project-id: project-id, milestone-id: milestone-id }
      {
        description: description,
        target-metric: target-metric,
        target-value: target-value,
        release-amount: release-amount,
        validation-count: u0,
        rejection-count: u0,
        is-complete: false,
        deadline: deadline
      }
    )
    (map-set projects { project-id: project-id }
      (merge project { milestone-count: milestone-id })
    )
    (ok milestone-id)
  )
)

;; ===========================
;; IMPACT ORACLE NETWORK
;; ===========================

;; Validator submits outcome verification for a milestone
(define-public (validate-milestone
    (project-id uint)
    (milestone-id uint)
    (approved bool)
    (reported-value uint)
    (sentiment-score uint)
  )
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (milestone (unwrap! (map-get? milestones { project-id: project-id, milestone-id: milestone-id }) ERR-INVALID-MILESTONE))
    (v (unwrap! (map-get? validators { validator: tx-sender }) ERR-NOT-VALIDATOR))
    (existing-vote (map-get? milestone-validations
      { project-id: project-id, milestone-id: milestone-id, validator: tx-sender }))
  )
    (asserts! (get is-active v) ERR-NOT-VALIDATOR)
    (asserts! (not (get is-complete milestone)) ERR-MILESTONE-ALREADY-COMPLETE)
    (asserts! (is-none existing-vote) ERR-ALREADY-VALIDATED)
    (asserts! (<= sentiment-score u100) ERR-INVALID-SCORE)

    ;; Record this validator's vote
    (map-set milestone-validations
      { project-id: project-id, milestone-id: milestone-id, validator: tx-sender }
      {
        approved: approved,
        reported-value: reported-value,
        sentiment-score: sentiment-score,
        validated-at: block-height
      }
    )

    ;; Update milestone vote tally
    (let (
      (new-approval-count (if approved (+ (get validation-count milestone) u1) (get validation-count milestone)))
      (new-rejection-count (if approved (get rejection-count milestone) (+ (get rejection-count milestone) u1)))
    )
      (map-set milestones
        { project-id: project-id, milestone-id: milestone-id }
        (merge milestone {
          validation-count: new-approval-count,
          rejection-count: new-rejection-count
        })
      )

      ;; Reward validator with governance tokens
      (try! (ft-mint? VIBE-GOV GOVERNANCE-REWARD tx-sender))

      ;; Update validator stats
      (map-set validators { validator: tx-sender }
        (merge v { validations-completed: (+ (get validations-completed v) u1) })
      )

      ;; If quorum reached with net approval, finalize milestone
      (if (>= new-approval-count VALIDATION-QUORUM)
        (try! (finalize-milestone project-id milestone-id))
        true
      )
    )
    (ok true)
  )
)

;; Internal: release escrow funds when milestone is validated
(define-private (finalize-milestone (project-id uint) (milestone-id uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (milestone (unwrap! (map-get? milestones { project-id: project-id, milestone-id: milestone-id }) ERR-INVALID-MILESTONE))
    (pool (unwrap! (map-get? impact-pools { pool-id: (get pool-id project) }) ERR-POOL-NOT-FOUND))
    (release-amount (get release-amount milestone))
    (new-completed (+ (get completed-milestones project) u1))
  )
    ;; Mark milestone complete
    (map-set milestones
      { project-id: project-id, milestone-id: milestone-id }
      (merge milestone { is-complete: true })
    )

    ;; Transfer released capital to project lead
    (try! (as-contract (stx-transfer? release-amount tx-sender (get lead project))))

    ;; Update project disbursement tracking
    (let (
      (new-vibe (compute-project-vibe project milestone))
      (all-done (is-eq new-completed (get milestone-count project)))
    )
      (map-set projects { project-id: project-id }
        (merge project {
          disbursed: (+ (get disbursed project) release-amount),
          completed-milestones: new-completed,
          vibe-score: new-vibe,
          status: (if all-done STATUS-COMPLETED STATUS-ACTIVE)
        })
      )

      ;; Update pool vibe score (rolling average)
      (map-set impact-pools { pool-id: (get pool-id project) }
        (merge pool {
          released-capital: (+ (get released-capital pool) release-amount),
          vibe-score: (/ (+ (get vibe-score pool) new-vibe) u2)
        })
      )
    )
    (ok true)
  )
)

;; Vibe Score: blends milestone completion ratio, outcome accuracy, and sentiment
;; Returns 0-1000
(define-private (compute-project-vibe
    (project { pool-id: uint, lead: principal, name: (string-ascii 64), category: uint,
               description: (string-utf8 256), total-budget: uint, disbursed: uint,
               milestone-count: uint, completed-milestones: uint, status: uint,
               vibe-score: uint, created-at: uint })
    (milestone { description: (string-utf8 128), target-metric: (string-ascii 64),
                 target-value: uint, release-amount: uint, validation-count: uint,
                 rejection-count: uint, is-complete: bool, deadline: uint })
  )
  (let (
    (total (get milestone-count project))
    (completed (+ (get completed-milestones project) u1))
    ;; Completion ratio component (0-600)
    (completion-score (if (> total u0) (/ (* completed u600) total) u0))
    ;; Outcome accuracy: approvals vs rejections (0-300)
    (total-votes (+ (get validation-count milestone) (get rejection-count milestone)))
    (accuracy-score
      (if (> total-votes u0)
        (/ (* (get validation-count milestone) u300) total-votes)
        u150
      )
    )
    ;; Timeliness bonus: 100 if completed before deadline (0-100)
    (timeliness-score (if (<= block-height (get deadline milestone)) u100 u0))
  )
    (+ (+ completion-score accuracy-score) timeliness-score)
  )
)

;; ===========================
;; SECONDARY MARKET (IMPACT BONDS)
;; ===========================

;; List IMPACT-BOND tokens for sale
(define-public (list-bonds-for-sale (pool-id uint) (bond-amount uint) (ask-price uint))
  (let (
    (new-id (+ (var-get bond-nonce) u1))
  )
    (asserts! (> bond-amount u0) ERR-INSUFFICIENT-FUNDS)
    (asserts! (> ask-price u0) ERR-INSUFFICIENT-FUNDS)
    ;; Transfer bonds to escrow (contract holds them during listing)
    (try! (ft-transfer? IMPACT-BOND bond-amount tx-sender (as-contract tx-sender)))
    (var-set bond-nonce new-id)
    (map-set bond-listings
      { listing-id: new-id }
      {
        seller: tx-sender,
        pool-id: pool-id,
        bond-amount: bond-amount,
        ask-price: ask-price,
        listed-at: block-height,
        is-active: true
      }
    )
    (ok new-id)
  )
)

;; Cancel a bond listing and reclaim tokens
(define-public (cancel-bond-listing (listing-id uint))
  (let (
    (listing (unwrap! (map-get? bond-listings { listing-id: listing-id }) ERR-BOND-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active listing) ERR-BOND-NOT-FOR-SALE)
    (map-set bond-listings { listing-id: listing-id }
      (merge listing { is-active: false })
    )
    (try! (as-contract (ft-transfer? IMPACT-BOND (get bond-amount listing) tx-sender (get seller listing))))
    (ok true)
  )
)

;; Purchase bonds from secondary market
(define-public (purchase-bonds (listing-id uint))
  (let (
    (listing (unwrap! (map-get? bond-listings { listing-id: listing-id }) ERR-BOND-NOT-FOUND))
    (pool (unwrap! (map-get? impact-pools { pool-id: (get pool-id listing) }) ERR-POOL-NOT-FOUND))
    (fee-bps (calculate-fee-bps (get vibe-score pool)))
    (fee-amount (/ (* (get ask-price listing) fee-bps) u10000))
    (seller-proceeds (- (get ask-price listing) fee-amount))
  )
    (asserts! (get is-active listing) ERR-BOND-NOT-FOR-SALE)
    (asserts! (not (is-eq tx-sender (get seller listing))) ERR-NOT-AUTHORIZED)

    ;; Buyer pays ask price
    (try! (stx-transfer? (get ask-price listing) tx-sender (as-contract tx-sender)))

    ;; Pay seller (minus fee)
    (try! (as-contract (stx-transfer? seller-proceeds tx-sender (get seller listing))))

    ;; Collect platform fee
    (var-set platform-treasury (+ (var-get platform-treasury) fee-amount))

    ;; Transfer bonds to buyer
    (try! (as-contract (ft-transfer? IMPACT-BOND (get bond-amount listing) tx-sender tx-sender)))

    ;; Close listing
    (map-set bond-listings { listing-id: listing-id }
      (merge listing { is-active: false })
    )

    ;; Update buyer's pool-investment record
    (let (
      (existing (map-get? pool-investments { pool-id: (get pool-id listing), investor: tx-sender }))
    )
      (match existing
        inv (map-set pool-investments
              { pool-id: (get pool-id listing), investor: tx-sender }
              (merge inv { bond-tokens: (+ (get bond-tokens inv) (get bond-amount listing)) })
            )
        (map-set pool-investments
          { pool-id: (get pool-id listing), investor: tx-sender }
          {
            amount: u0,
            bond-tokens: (get bond-amount listing),
            invested-at: block-height
          }
        )
      )
    )
    (ok true)
  )
)

;; ===========================
;; ADMIN / GOVERNANCE
;; ===========================

;; Contract owner can withdraw accumulated platform fees
(define-public (withdraw-treasury (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= amount (var-get platform-treasury)) ERR-INSUFFICIENT-FUNDS)
    (var-set platform-treasury (- (var-get platform-treasury) amount))
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok true)
  )
)

;; Pool creator can pause/resume their pool
(define-public (set-pool-status (pool-id uint) (new-status uint))
  (let (
    (pool (unwrap! (map-get? impact-pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND))
  )
    (asserts!
      (or (is-eq tx-sender (get creator pool)) (is-eq tx-sender CONTRACT-OWNER))
      ERR-NOT-AUTHORIZED
    )
    (asserts!
      (or (is-eq new-status STATUS-ACTIVE)
          (is-eq new-status STATUS-PAUSED)
          (is-eq new-status STATUS-CANCELLED))
      ERR-NOT-AUTHORIZED
    )
    (map-set impact-pools { pool-id: pool-id }
      (merge pool { status: new-status })
    )
    (ok true)
  )
)

;; Slash a malicious validator (owner only); redistributes stake to treasury
(define-public (slash-validator (validator-addr principal))
  (let (
    (v (unwrap! (map-get? validators { validator: validator-addr }) ERR-NOT-VALIDATOR))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set platform-treasury (+ (var-get platform-treasury) (get staked-amount v)))
    (map-set validators { validator: validator-addr }
      (merge v { is-active: false, staked-amount: u0, reputation-score: u0 })
    )
    (ok true)
  )
)
