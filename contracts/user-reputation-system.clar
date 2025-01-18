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
