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
