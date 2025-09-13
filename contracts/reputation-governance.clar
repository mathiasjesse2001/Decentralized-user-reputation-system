;; Dynamic Reputation Governance System
;; Enables reputation-weighted community governance for ecosystem parameters

;; Proposal data structure
(define-map governance-proposals
  { proposal-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposal-type: (string-ascii 30),
    target-parameter: (string-ascii 50),
    current-value: int,
    proposed-value: int,
    created-at: uint,
    voting-deadline: uint,
    execution-deadline: uint,
    status: (string-ascii 20),
    total-votes-for: uint,
    total-votes-against: uint,
    total-voting-power: uint,
    quorum-threshold: uint,
    approval-threshold: uint
  })

;; Vote tracking
(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  {
    vote-type: (string-ascii 10),
    voting-power: uint,
    reputation-at-vote: int,
    vote-timestamp: uint
  })

;; Governance parameters
(define-map system-parameters
  { parameter-name: (string-ascii 50) }
  { 
    current-value: int,
    last-updated: uint,
    update-count: uint,
    min-value: int,
    max-value: int
  })

;; Delegate voting system
(define-map governance-delegates
  { delegator: principal, proposal-id: uint }
  { delegate: principal, power-delegated: uint })

;; Emergency governance actions
(define-map emergency-proposals
  { emergency-id: uint }
  {
    creator: principal,
    action: (string-ascii 100),
    justification: (string-ascii 300),
    created-at: uint,
    execution-deadline: uint,
    council-votes: uint,
    status: (string-ascii 20)
  })

;; Governance council for emergency actions
(define-map council-members
  { member: principal }
  { appointed-at: uint, reputation-at-appointment: int, active: bool })

;; Data variables
(define-data-var next-proposal-id uint u1)
(define-data-var next-emergency-id uint u1)
(define-data-var governance-active bool true)
(define-data-var total-council-members uint u0)

;; Constants
(define-constant MIN_PROPOSAL_REPUTATION 100)
(define-constant MIN_VOTING_PERIOD u1440) ;; 10 days in blocks
(define-constant MAX_VOTING_PERIOD u4320) ;; 30 days in blocks
(define-constant EMERGENCY_COUNCIL_SIZE u5)
(define-constant QUADRATIC_SCALING_FACTOR u2)
(define-constant BASE_QUORUM_PERCENTAGE u20) ;; 20%
(define-constant APPROVAL_THRESHOLD_PERCENTAGE u60) ;; 60%

;; Error constants
(define-constant ERR_INSUFFICIENT_REPUTATION -30)
(define-constant ERR_GOVERNANCE_INACTIVE -31)
(define-constant ERR_INVALID_PROPOSAL -32)
(define-constant ERR_VOTING_ENDED -33)
(define-constant ERR_ALREADY_VOTED -34)
(define-constant ERR_PROPOSAL_NOT_READY -35)
(define-constant ERR_UNAUTHORIZED -36)
(define-constant ERR_INVALID_PARAMETER -37)
(define-constant ERR_EMERGENCY_ONLY -38)

;; Initialize governance parameters
(define-public (initialize-governance)
  (begin
    (map-set system-parameters
      { parameter-name: "min-circle-members" }
      { current-value: 3, last-updated: stacks-block-height, update-count: u0, min-value: 2, max-value: 10 })
    (map-set system-parameters
      { parameter-name: "max-circle-members" }
      { current-value: 5, last-updated: stacks-block-height, update-count: u0, min-value: 3, max-value: 20 })
    (map-set system-parameters
      { parameter-name: "approval-threshold" }
      { current-value: 75, last-updated: stacks-block-height, update-count: u0, min-value: 50, max-value: 95 })
    (map-set system-parameters
      { parameter-name: "daily-vote-limit" }
      { current-value: 5, last-updated: stacks-block-height, update-count: u0, min-value: 1, max-value: 20 })
    (ok true)))

;; Create a governance proposal
(define-public (create-proposal 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (proposal-type (string-ascii 30))
  (target-parameter (string-ascii 50))
  (proposed-value int)
  (voting-duration uint))
  (let ((proposal-id (var-get next-proposal-id))
        (creator-reputation (get score (contract-call? .user-reputation-system get-reputation tx-sender)))
        (current-param (map-get? system-parameters { parameter-name: target-parameter })))
    (if (and 
        (var-get governance-active)
        (>= creator-reputation MIN_PROPOSAL_REPUTATION)
        (>= voting-duration MIN_VOTING_PERIOD)
        (<= voting-duration MAX_VOTING_PERIOD)
        (is-some current-param))
      (let ((param-data (unwrap-panic current-param))
            (voting-deadline (+ stacks-block-height voting-duration))
            (execution-deadline (+ voting-deadline u720))) ;; 5 days after voting ends
        (begin
          (map-set governance-proposals
            { proposal-id: proposal-id }
            {
              creator: tx-sender,
              title: title,
              description: description,
              proposal-type: proposal-type,
              target-parameter: target-parameter,
              current-value: (get current-value param-data),
              proposed-value: proposed-value,
              created-at: stacks-block-height,
              voting-deadline: voting-deadline,
              execution-deadline: execution-deadline,
              status: "active",
              total-votes-for: u0,
              total-votes-against: u0,
              total-voting-power: u0,
              quorum-threshold: (calculate-quorum-threshold),
              approval-threshold: APPROVAL_THRESHOLD_PERCENTAGE
            })
          (var-set next-proposal-id (+ proposal-id u1))
          (ok proposal-id)))
      (err ERR_INSUFFICIENT_REPUTATION))))

;; Vote on a proposal with quadratic voting
(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let ((proposal (map-get? governance-proposals { proposal-id: proposal-id }))
        (existing-vote (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender }))
        (voter-reputation (get score (contract-call? .user-reputation-system get-reputation tx-sender))))
    (match proposal
      proposal-data
        (if (and
            (var-get governance-active)
            (<= stacks-block-height (get voting-deadline proposal-data))
            (is-eq (get status proposal-data) "active")
            (is-none existing-vote)
            (> voter-reputation 0))
          (let ((voting-power (calculate-quadratic-voting-power voter-reputation))
                (vote-type (if vote-for "for" "against")))
            (begin
              (map-set proposal-votes
                { proposal-id: proposal-id, voter: tx-sender }
                {
                  vote-type: vote-type,
                  voting-power: voting-power,
                  reputation-at-vote: voter-reputation,
                  vote-timestamp: stacks-block-height
                })
              (map-set governance-proposals
                { proposal-id: proposal-id }
                (merge proposal-data {
                  total-votes-for: (if vote-for 
                    (+ (get total-votes-for proposal-data) voting-power)
                    (get total-votes-for proposal-data)),
                  total-votes-against: (if (not vote-for)
                    (+ (get total-votes-against proposal-data) voting-power)
                    (get total-votes-against proposal-data)),
                  total-voting-power: (+ (get total-voting-power proposal-data) voting-power)
                }))
              (ok true)))
          (err ERR_ALREADY_VOTED))
      (err ERR_INVALID_PROPOSAL))))

;; Delegate voting power for specific proposal
(define-public (delegate-vote (proposal-id uint) (delegate principal) (power-percentage uint))
  (let ((delegator-reputation (get score (contract-call? .user-reputation-system get-reputation tx-sender)))
        (delegation-power (/ (* (to-uint delegator-reputation) power-percentage) u100)))
    (if (and 
        (<= power-percentage u100)
        (> delegator-reputation 0))
      (begin
        (map-set governance-delegates
          { delegator: tx-sender, proposal-id: proposal-id }
          { delegate: delegate, power-delegated: delegation-power })
        (ok delegation-power))
      (err ERR_INVALID_PARAMETER))))

;; Vote with delegated power
(define-public (vote-with-delegation (proposal-id uint) (vote-for bool) (delegators (list 5 principal)))
  (let ((delegate-reputation (get score (contract-call? .user-reputation-system get-reputation tx-sender)))
        (total-delegated-power (fold + (map get-delegated-power-for-proposal delegators) u0)))
    (if (> delegate-reputation 0)
      (let ((total-voting-power (+ (calculate-quadratic-voting-power delegate-reputation) total-delegated-power)))
        (vote-with-power proposal-id vote-for total-voting-power))
      (err ERR_INSUFFICIENT_REPUTATION))))

;; Execute approved proposal
(define-public (execute-proposal (proposal-id uint))
  (let ((proposal (map-get? governance-proposals { proposal-id: proposal-id })))
    (match proposal
      proposal-data
        (if (and
            (>= stacks-block-height (get voting-deadline proposal-data))
            (<= stacks-block-height (get execution-deadline proposal-data))
            (is-eq (get status proposal-data) "active")
            (>= (get total-voting-power proposal-data) (get quorum-threshold proposal-data))
            (>= (* (get total-votes-for proposal-data) u100) 
                (* (get total-voting-power proposal-data) (get approval-threshold proposal-data))))
          (begin
            (map-set system-parameters
              { parameter-name: (get target-parameter proposal-data) }
              {
                current-value: (get proposed-value proposal-data),
                last-updated: stacks-block-height,
                update-count: (+ u1 (get update-count (default-to 
                  { current-value: 0, last-updated: u0, update-count: u0, min-value: 0, max-value: 0 }
                  (map-get? system-parameters { parameter-name: (get target-parameter proposal-data) })))),
                min-value: (get min-value (unwrap-panic (map-get? system-parameters { parameter-name: (get target-parameter proposal-data) }))),
                max-value: (get max-value (unwrap-panic (map-get? system-parameters { parameter-name: (get target-parameter proposal-data) })))
              })
            (map-set governance-proposals
              { proposal-id: proposal-id }
              (merge proposal-data { status: "executed" }))
            (ok true))
          (err ERR_PROPOSAL_NOT_READY))
      (err ERR_INVALID_PROPOSAL))))

;; Create emergency proposal (council only)
(define-public (create-emergency-proposal 
  (action (string-ascii 100))
  (justification (string-ascii 300)))
  (let ((emergency-id (var-get next-emergency-id))
        (is-council-member (is-some (map-get? council-members { member: tx-sender }))))
    (if (and 
        (var-get governance-active)
        is-council-member
        (get active (default-to { appointed-at: u0, reputation-at-appointment: 0, active: false }
          (map-get? council-members { member: tx-sender }))))
      (begin
        (map-set emergency-proposals
          { emergency-id: emergency-id }
          {
            creator: tx-sender,
            action: action,
            justification: justification,
            created-at: stacks-block-height,
            execution-deadline: (+ stacks-block-height u288), ;; 2 days
            council-votes: u1,
            status: "pending"
          })
        (var-set next-emergency-id (+ emergency-id u1))
        (ok emergency-id))
      (err ERR_UNAUTHORIZED))))

;; Vote on emergency proposal (council only)
(define-public (vote-emergency-proposal (emergency-id uint))
  (let ((emergency (map-get? emergency-proposals { emergency-id: emergency-id }))
        (is-council-member (is-some (map-get? council-members { member: tx-sender }))))
    (match emergency
      emergency-data
        (if (and
            is-council-member
            (get active (default-to { appointed-at: u0, reputation-at-appointment: 0, active: false }
              (map-get? council-members { member: tx-sender })))
            (<= stacks-block-height (get execution-deadline emergency-data))
            (is-eq (get status emergency-data) "pending"))
          (let ((new-vote-count (+ (get council-votes emergency-data) u1)))
            (begin
              (map-set emergency-proposals
                { emergency-id: emergency-id }
                (merge emergency-data { 
                  council-votes: new-vote-count,
                  status: (if (>= new-vote-count (/ EMERGENCY_COUNCIL_SIZE u2))
                    "approved"
                    "pending")
                }))
              (ok new-vote-count)))
          (err ERR_UNAUTHORIZED))
      (err ERR_INVALID_PROPOSAL))))

;; Appoint council member (existing council only)
(define-public (appoint-council-member (new-member principal))
  (let ((is-council-member (is-some (map-get? council-members { member: tx-sender })))
        (member-reputation (get score (contract-call? .user-reputation-system get-reputation new-member))))
    (if (and
        is-council-member
        (>= member-reputation 200) ;; High reputation requirement
        (< (var-get total-council-members) EMERGENCY_COUNCIL_SIZE))
      (begin
        (map-set council-members
          { member: new-member }
          { 
            appointed-at: stacks-block-height,
            reputation-at-appointment: member-reputation,
            active: true
          })
        (var-set total-council-members (+ (var-get total-council-members) u1))
        (ok true))
      (err ERR_UNAUTHORIZED))))

;; Private helper functions
(define-private (calculate-quadratic-voting-power (reputation int))
  (let ((positive-rep (if (> reputation 0) (to-uint reputation) u0)))
    (+ u1 (/ positive-rep QUADRATIC_SCALING_FACTOR))))

(define-private (calculate-quorum-threshold)
  (let ((total-active-reputation (get-total-active-reputation)))
    (/ (* total-active-reputation BASE_QUORUM_PERCENTAGE) u100)))

(define-private (get-total-active-reputation)
  u1000) ;; Simplified - would calculate actual total in production

(define-private (get-delegated-power-for-proposal (delegator principal))
  (let ((delegation (map-get? governance-delegates { delegator: delegator, proposal-id: u0 })))
    (match delegation
      delegation-data (get power-delegated delegation-data)
      u0)))

(define-private (vote-with-power (proposal-id uint) (vote-for bool) (voting-power uint))
  (let ((proposal (unwrap! (map-get? governance-proposals { proposal-id: proposal-id }) (err ERR_INVALID_PROPOSAL))))
    (begin
      (map-set proposal-votes
        { proposal-id: proposal-id, voter: tx-sender }
        {
          vote-type: (if vote-for "for" "against"),
          voting-power: voting-power,
          reputation-at-vote: (get score (contract-call? .user-reputation-system get-reputation tx-sender)),
          vote-timestamp: stacks-block-height
        })
      (map-set governance-proposals
        { proposal-id: proposal-id }
        (merge proposal {
          total-votes-for: (if vote-for 
            (+ (get total-votes-for proposal) voting-power)
            (get total-votes-for proposal)),
          total-votes-against: (if (not vote-for)
            (+ (get total-votes-against proposal) voting-power)
            (get total-votes-against proposal)),
          total-voting-power: (+ (get total-voting-power proposal) voting-power)
        }))
      (ok true))))

;; Read-only functions
(define-read-only (get-proposal-details (proposal-id uint))
  (map-get? governance-proposals { proposal-id: proposal-id }))

(define-read-only (get-vote-details (proposal-id uint) (voter principal))
  (map-get? proposal-votes { proposal-id: proposal-id, voter: voter }))

(define-read-only (get-system-parameter (parameter-name (string-ascii 50)))
  (map-get? system-parameters { parameter-name: parameter-name }))

(define-read-only (get-emergency-proposal (emergency-id uint))
  (map-get? emergency-proposals { emergency-id: emergency-id }))

(define-read-only (is-council-member (member principal))
  (is-some (map-get? council-members { member: member })))

(define-read-only (get-governance-stats)
  {
    total-proposals: (- (var-get next-proposal-id) u1),
    total-emergencies: (- (var-get next-emergency-id) u1),
    governance-active: (var-get governance-active),
    council-size: (var-get total-council-members)
  })

(define-read-only (get-voting-power (user principal))
  (let ((user-reputation (get score (contract-call? .user-reputation-system get-reputation user))))
    (calculate-quadratic-voting-power user-reputation)))

;; Emergency governance controls
(define-public (pause-governance)
  (let ((is-council-member (is-some (map-get? council-members { member: tx-sender }))))
    (if is-council-member
      (begin
        (var-set governance-active false)
        (ok true))
      (err ERR_UNAUTHORIZED))))

(define-public (resume-governance)
  (let ((is-council-member (is-some (map-get? council-members { member: tx-sender }))))
    (if is-council-member
      (begin
        (var-set governance-active true)
        (ok true))
      (err ERR_UNAUTHORIZED))))

