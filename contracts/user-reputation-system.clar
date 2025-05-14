(define-map user-reputation
  { user: principal }
  { score: int })

(define-constant ERR_INVALID_ACTION -1)
(define-constant ERR_SELF_ACTION -2)

(define-public (upvote (target-user principal))
  (begin
    (if (is-eq tx-sender target-user)
        (err ERR_SELF_ACTION)
        (ok (update-user-reputation target-user 1)))
  ))

  
(define-public (downvote (target-user principal))
  (begin
    (if (is-eq tx-sender target-user)
        (err ERR_SELF_ACTION)
        (ok (update-user-reputation target-user -1)))
  ))

  (define-read-only (get-reputation (user principal))
    (default-to { score: 0 } (map-get? user-reputation { user: user })))

    (define-private (update-user-reputation (user principal) (delta int))
  (let ((current-score (default-to { score: 0 } (map-get? user-reputation { user: user }))))
    (map-set user-reputation
      { user: user }
      { score: (+ (get score current-score) delta) })))


;; Add these constants
(define-constant BRONZE 0)
(define-constant SILVER 50) 
(define-constant GOLD 100)
(define-constant PLATINUM 200)

(define-read-only (get-user-tier (user principal))
(let ((score (get score (get-reputation user))))
  (if (>= score PLATINUM)
      "Platinum"
      (if (>= score GOLD)
          "Gold"
          (if (>= score SILVER)
              "Silver"
              "Bronze")))))


(define-private (calculate-vote-weight (voter principal))
(let ((voter-score (get score (get-reputation voter))))
  (if (>= voter-score 100)
      2
      (if (>= voter-score 50)
          (to-int u1)
          1))))
(define-public (weighted-upvote (target-user principal))
  (let ((weight (calculate-vote-weight tx-sender)))
    (if (is-eq tx-sender target-user)
        (err ERR_SELF_ACTION)
        (ok (update-user-reputation target-user weight)))))



(define-map vote-history
  { voter: principal, target: principal }
  { vote-type: (string-ascii 8), timestamp: uint })

(define-public (upvote-with-history (target-user principal))
  (begin
    (if (is-eq tx-sender target-user)
        (err ERR_SELF_ACTION)
        (begin
          (map-set vote-history
            { voter: tx-sender, target: target-user }
            { vote-type: "upvote", timestamp: block-height })
          (ok (update-user-reputation target-user 1))))))



(define-constant DAILY_VOTE_LIMIT u5)
(define-constant ERR_DAILY_LIMIT_EXCEEDED -1)

(define-map daily-votes
  { user: principal, day: uint }
  { count: uint })

(define-public (limited-upvote (target-user principal))
  (let ((current-day (/ block-height u144))
        (vote-count (default-to { count: u0 } 
          (map-get? daily-votes { user: tx-sender, day: current-day }))))
    (if (>= (get count vote-count) DAILY_VOTE_LIMIT)
        (err ERR_DAILY_LIMIT_EXCEEDED)
        (begin
          (map-set daily-votes 
            { user: tx-sender, day: current-day }
            { count: (+ (get count vote-count) u1) })
          (ok (update-user-reputation target-user 1))))))



(define-constant DECAY_RATE 1)
(define-constant DECAY_PERIOD 144) ;; One day in blocks

(define-public (apply-reputation-decay (user principal))
  (let ((current-score (get score (get-reputation user))))
    (if (> current-score 0)
        (ok (update-user-reputation user (* -1 DECAY_RATE)))
        (ok true))))


(define-map user-achievements
  { user: principal }
  { first-upvote: bool, reach-100: bool })

(define-public (check-achievements (user principal))
  (let ((score (get score (get-reputation user))))
    (begin
      (if (>= score 100)
          (map-set user-achievements 
            { user: user }
            { first-upvote: true, reach-100: true })
          false)
      (ok true))))



(define-constant RECOVERY_COOLDOWN u1440) ;; 10 days in blocks
(define-map recovery-timestamps
  { user: principal }
  { last-recovery: uint })

(define-public (recover-reputation)
  (let ((last-recovery (default-to { last-recovery: u0 }
         (map-get? recovery-timestamps { user: tx-sender }))))
    (if (>= (- block-height (get last-recovery last-recovery)) RECOVERY_COOLDOWN)
        (begin
          (map-set recovery-timestamps
            { user: tx-sender }
            { last-recovery: block-height })
          (ok (update-user-reputation tx-sender 10)))
        (err u100))))



(define-map staked-reputation
  { user: principal }
  { amount: int, lock-period: uint })

(define-public (stake-reputation (amount int) (lock-blocks uint))
  (let ((current-score (get score (get-reputation tx-sender))))
    (if (>= current-score amount)
        (ok (map-set staked-reputation
          { user: tx-sender }
          { amount: amount, lock-period: (+ block-height lock-blocks) }))
        (err u1))))



(define-map category-reputation
  { user: principal, category: (string-ascii 20) }
  { score: int })

(define-public (category-upvote (target-user principal) (category (string-ascii 20)))
  (ok (map-set category-reputation
    { user: target-user, category: category }
    { score: (+ (get score (default-to { score: 0 } 
      (map-get? category-reputation { user: target-user, category: category }))) 1) })))




(define-constant TRANSFER_FEE 5)

(define-public (transfer-reputation (recipient principal) (amount int))
  (let ((sender-score (get score (get-reputation tx-sender))))
    (if (>= sender-score (+ amount TRANSFER_FEE))
        (begin
          (update-user-reputation tx-sender (* -1 (+ amount TRANSFER_FEE)))
          (ok (update-user-reputation recipient amount)))
        (err u2))))



(define-map daily-challenges
  { day: uint }
  { description: (string-ascii 50), reward: int })

(define-map user-challenge-completion
  { user: principal, day: uint }
  { completed: bool })

(define-public (complete-challenge (day uint))
  (let ((challenge (default-to { description: "", reward: 0 } 
        (map-get? daily-challenges { day: day }))))
    (ok (map-set user-challenge-completion
      { user: tx-sender, day: day }
      { completed: true }))))


(define-map reputation-insurance
  { user: principal }
  { protected-amount: int, expiry: uint })

(define-public (buy-insurance (amount int) (duration uint))
  (ok (map-set reputation-insurance
    { user: tx-sender }
    { protected-amount: amount, expiry: (+ block-height duration) })))


(define-map endorsements
  { endorser: principal, endorsed: principal }
  { weight: int, timestamp: uint })

(define-public (endorse-user (user principal) (weight int))
  (ok (map-set endorsements
    { endorser: tx-sender, endorsed: user }
    { weight: weight, timestamp: block-height })))


;; Define badge types
(define-map user-badges 
    { user: principal }
    { 
        influencer: bool,
        expert: bool,
        pioneer: bool,
        mentor: bool 
    })

(define-public (award-badge (user principal) (badge-type (string-ascii 20)))
    (let ((current-badges (default-to 
            { influencer: false, expert: false, pioneer: false, mentor: false }
            (map-get? user-badges { user: user }))))
        (ok (map-set user-badges
            { user: user }
            (if (is-eq badge-type "influencer")
                (merge current-badges { influencer: true })
                (if (is-eq badge-type "expert")
                    (merge current-badges { expert: true })
                    (if (is-eq badge-type "pioneer")
                        (merge current-badges { pioneer: true })
                        (if (is-eq badge-type "mentor")
                            (merge current-badges { mentor: true })
                            current-badges))))))))



(define-map event-multipliers
    { event-id: uint }
    { multiplier: uint, start-block: uint, end-block: uint })

(define-public (create-multiplier-event (event-id uint) (multiplier uint) (duration uint))
    (ok (map-set event-multipliers
        { event-id: event-id }
        { 
            multiplier: multiplier,
            start-block: block-height,
            end-block: (+ block-height duration)
        })))

(define-public (event-upvote (target-user principal) (event-id uint))
    (let ((event (default-to { multiplier: u1, start-block: u0, end-block: u0 }
            (map-get? event-multipliers { event-id: event-id }))))
        (if (and (>= block-height (get start-block event))
                (<= block-height (get end-block event)))
            (ok (update-user-reputation target-user 
                (* 1 (to-int (get multiplier event)))))
            (ok (update-user-reputation target-user 1)))))


(define-map leaderboard
    { rank: uint }
    { user: principal, score: int })

(define-public (update-leaderboard (user principal))
    (let ((user-score (get score (get-reputation user))))
        (ok (map-set leaderboard
            { rank: u1 }
            { user: user, score: user-score }))))

(define-read-only (get-top-user)
    (default-to 
        { user: tx-sender, score: 0 }
        (map-get? leaderboard { rank: u1 })))


(define-map active-boosters
    { user: principal }
    { multiplier: uint, expiry: uint })

(define-constant BOOSTER_DURATION u1440) ;; 10 days in blocks
(define-constant BOOSTER_MULTIPLIER u2)

(define-public (activate-booster)
    (ok (map-set active-boosters
        { user: tx-sender }
        { 
            multiplier: BOOSTER_MULTIPLIER,
            expiry: (+ block-height BOOSTER_DURATION)
        })))


(define-map milestone-rewards
    { level: uint }
    { reward: int, claimed: bool })

(define-constant MILESTONE-1 u50)
(define-constant MILESTONE-2 u100)
(define-constant MILESTONE-3 u200)

(define-public (claim-milestone-reward (level uint))
    (let ((user-score (get score (get-reputation tx-sender))))
        (if (and 
            (>= user-score (to-int level))
            (not (get claimed (default-to { reward: 0, claimed: false }
                (map-get? milestone-rewards { level: level })))))
            (ok (map-set milestone-rewards
                { level: level }
                { reward: 10, claimed: true }))
            (err u1))))



(define-map recovery-challenges
    { user: principal }
    { target-score: int, deadline: uint, completed: bool })

(define-public (start-recovery-challenge (target-score int))
    (ok (map-set recovery-challenges
        { user: tx-sender }
        { 
            target-score: target-score,
            deadline: (+ block-height u1440),
            completed: false
        })))


(define-map community-pools
    { pool-id: uint }
    { total-score: int, member-count: uint })

(define-map pool-membership
    { user: principal, pool-id: uint }
    { joined: bool })

(define-public (join-community-pool (pool-id uint))
    (begin
        (map-set pool-membership
            { user: tx-sender, pool-id: pool-id }
            { joined: true })
        (ok true)))


(define-map staking-rewards
    { user: principal }
    { staked-amount: int, reward-rate: uint, last-claim: uint })

(define-constant DAILY-REWARD-RATE u5)

(define-public (stake-for-rewards (amount int))
    (let ((user-score (get score (get-reputation tx-sender))))
        (if (>= user-score amount)
            (ok (map-set staking-rewards
                { user: tx-sender }
                { 
                    staked-amount: amount,
                    reward-rate: DAILY-REWARD-RATE,
                    last-claim: block-height
                }))
            (err u1))))



(define-map badge-powers
    { badge-type: (string-ascii 20) }
    { vote-multiplier: uint, daily-limit-bonus: uint })

(define-public (initialize-badge-powers)
    (begin
        (map-set badge-powers 
            { badge-type: "bronze" }
            { vote-multiplier: u1, daily-limit-bonus: u2 })
        (map-set badge-powers
            { badge-type: "silver" }
            { vote-multiplier: u2, daily-limit-bonus: u3 })
        (map-set badge-powers
            { badge-type: "gold" }
            { vote-multiplier: u3, daily-limit-bonus: u5 })
        (ok true)))

(define-public (apply-badge-power (action-type (string-ascii 10)))
    (let ((user-tier (get-user-tier tx-sender))
          (powers (default-to { vote-multiplier: u1, daily-limit-bonus: u0 }
                    (map-get? badge-powers { badge-type: user-tier }))))
        (ok powers)))



(define-map recovery-quests
    { quest-id: uint }
    { target: int, reward: int, timeframe: uint })

(define-map user-quests
    { user: principal, quest-id: uint }
    { completed: bool, start-time: uint })

(define-public (start-recovery-quest (quest-id uint))
    (ok (map-set user-quests
        { user: tx-sender, quest-id: quest-id }
        { completed: false, start-time: block-height })))

(define-public (complete-recovery-quest (quest-id uint))
    (let ((quest (default-to { target: 0, reward: 0, timeframe: u0 }
                    (map-get? recovery-quests { quest-id: quest-id }))))
        (ok (update-user-reputation tx-sender (get reward quest)))))



(define-map reputation-loans
    { borrower: principal, lender: principal }
    { amount: int, due-block: uint, returned: bool })

(define-public (lend-reputation (borrower principal) (amount int) (duration uint))
    (let ((lender-score (get score (get-reputation tx-sender))))
        (if (>= lender-score amount)
            (begin
                (update-user-reputation tx-sender (* -1 amount))
                (update-user-reputation borrower amount)
                (ok (map-set reputation-loans
                    { borrower: borrower, lender: tx-sender }
                    { amount: amount, due-block: (+ block-height duration), returned: false })))
            (err u1))))




(define-map staking-pools
    { pool-id: uint }
    { total-staked: int, reward-rate: uint, min-stake: int })

(define-map pool-stakes
    { user: principal, pool-id: uint }
    { amount: int, start-block: uint })

(define-public (create-staking-pool (pool-id uint) (min-stake int) (reward-rate uint))
    (ok (map-set staking-pools
        { pool-id: pool-id }
        { total-staked: 0, reward-rate: reward-rate, min-stake: min-stake })))

(define-public (stake-in-pool (pool-id uint) (amount int))
    (let ((pool (default-to { total-staked: 0, reward-rate: u0, min-stake: 0 }
                    (map-get? staking-pools { pool-id: pool-id }))))
        (if (>= amount (get min-stake pool))
            (ok (map-set pool-stakes
                { user: tx-sender, pool-id: pool-id }
                { amount: amount, start-block: block-height }))
            (err u1))))



(define-map bounties
    { bounty-id: uint }
    { reward: int, completed: bool, deadline: uint })

(define-map bounty-claims
    { user: principal, bounty-id: uint }
    { claimed: bool })

(define-public (create-bounty (bounty-id uint) (reward int) (duration uint))
    (ok (map-set bounties
        { bounty-id: bounty-id }
        { reward: reward, completed: false, deadline: (+ block-height duration) })))

(define-public (claim-bounty (bounty-id uint))
    (let ((bounty (default-to { reward: 0, completed: false, deadline: u0 }
                    (map-get? bounties { bounty-id: bounty-id }))))
        (ok (update-user-reputation tx-sender (get reward bounty)))))



(define-map boost-events
    { event-id: uint }
    { multiplier: uint, duration: uint, active: bool })

(define-map user-boosts
    { user: principal, event-id: uint }
    { participated: bool })

(define-public (create-boost-event (event-id uint) (multiplier uint) (duration uint))
    (ok (map-set boost-events
        { event-id: event-id }
        { multiplier: multiplier, duration: duration, active: true })))

(define-public (participate-in-boost (event-id uint))
    (let ((event (default-to { multiplier: u0, duration: u0, active: false }
                    (map-get? boost-events { event-id: event-id }))))
        (ok (map-set user-boosts
            { user: tx-sender, event-id: event-id }
            { participated: true }))))





(define-map reputation-offers
    { offer-id: uint }
    { seller: principal, amount: int, price: uint, active: bool })

(define-map user-trades
    { user: principal }
    { total-bought: int, total-sold: int })

(define-public (create-reputation-offer (offer-id uint) (amount int) (price uint))
    (let ((seller-score (get score (get-reputation tx-sender))))
        (if (>= seller-score amount)
            (ok (map-set reputation-offers
                { offer-id: offer-id }
                { seller: tx-sender, amount: amount, price: price, active: true }))
            (err u1))))



(define-map achievement-types
    { achievement-id: uint }
    { name: (string-ascii 50), required-score: int, reward: int })

(define-map user-achievements-new
    { user: principal, achievement-id: uint }
    { unlocked: bool, unlock-time: uint })

(define-public (create-achievement (achievement-id uint) (name (string-ascii 50)) (required-score int) (reward int))
    (ok (map-set achievement-types
        { achievement-id: achievement-id }
        { name: name, required-score: required-score, reward: reward })))

(define-public (check-achievement (achievement-id uint))
    (let ((achievement (default-to { name: "", required-score: 0, reward: 0 }
                        (map-get? achievement-types { achievement-id: achievement-id })))
          (user-score (get score (get-reputation tx-sender))))
        (if (>= user-score (get required-score achievement))
            (ok (map-set user-achievements-new
                { user: tx-sender, achievement-id: achievement-id }
                { unlocked: true, unlock-time: block-height }))
            (err u1))))


(define-map delegation-registry
    { delegator: principal, delegate: principal }
    { power: int, expiry: uint })

(define-constant ERR_INSUFFICIENT_POWER -3)
(define-constant ERR_EXPIRED_DELEGATION -4)

(define-public (delegate-power (delegate principal) (amount int) (duration uint))
    (let ((user-score (get score (get-reputation tx-sender))))
        (if (>= user-score amount)
            (ok (map-set delegation-registry
                { delegator: tx-sender, delegate: delegate }
                { power: amount, expiry: (+ block-height duration) }))
            (err ERR_INSUFFICIENT_POWER))))

(define-public (vote-with-delegation (target-user principal))
    (let ((delegation (default-to { power: 0, expiry: u0 }
            (map-get? delegation-registry { delegator: tx-sender, delegate: target-user }))))
        (if (>= block-height (get expiry delegation))
            (err ERR_EXPIRED_DELEGATION)
            (ok (update-user-reputation target-user (get power delegation))))))




(define-map insurance-pool
    { pool-id: uint }
    { total-staked: uint, coverage-ratio: uint, min-stake: uint })

(define-map insured-users
    { user: principal }
    { coverage-amount: int, premium-paid: uint, expiry: uint })

(define-constant COVERAGE_PERIOD u14400)
(define-constant MIN_PREMIUM u10)


(define-public (claim-insurance (lost-amount int))
    (let ((insurance (default-to 
            { coverage-amount: 0, premium-paid: u0, expiry: u0 }
            (map-get? insured-users { user: tx-sender }))))
        (if (and 
            (<= lost-amount (get coverage-amount insurance))
            (< block-height (get expiry insurance)))
            (ok (update-user-reputation tx-sender lost-amount))
            (err u3))))