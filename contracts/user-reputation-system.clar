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