(define-map review-circles
  { circle-id: uint }
  { 
    creator: principal,
    members: (list 5 principal),
    min-reviews: uint,
    active: bool
  })

(define-map circle-membership
  { user: principal, circle-id: uint }
  { joined-at: uint, reviews-given: uint, reviews-received: uint })

(define-map submissions
  { submission-id: uint }
  {
    submitter: principal,
    circle-id: uint,
    content-hash: (string-ascii 64),
    reviews-needed: uint,
    reviews-received: uint,
    status: (string-ascii 20),
    created-at: uint
  })

(define-map submission-reviews
  { submission-id: uint, reviewer: principal }
  {
    score: int,
    feedback: (string-ascii 200),
    submitted-at: uint
  })

(define-map circle-stats
  { circle-id: uint }
  {
    total-submissions: uint,
    approved-submissions: uint,
    average-quality: uint
  })

(define-data-var next-circle-id uint u1)
(define-data-var next-submission-id uint u1)

(define-constant MIN_CIRCLE_MEMBERS u3)
(define-constant MAX_CIRCLE_MEMBERS u5)
(define-constant MIN_REVIEWS u2)
(define-constant APPROVAL_THRESHOLD 75)
(define-constant REVIEW_TIMEOUT u1440)

(define-constant ERR_CIRCLE_FULL -20)
(define-constant ERR_NOT_MEMBER -21)
(define-constant ERR_ALREADY_REVIEWED -22)
(define-constant ERR_SUBMISSION_CLOSED -23)
(define-constant ERR_INVALID_CIRCLE -24)

(define-public (create-review-circle (min-reviews uint))
  (let ((circle-id (var-get next-circle-id)))
    (begin
      (map-set review-circles
        { circle-id: circle-id }
        {
          creator: tx-sender,
          members: (list tx-sender),
          min-reviews: min-reviews,
          active: true
        })
      (map-set circle-membership
        { user: tx-sender, circle-id: circle-id }
        { joined-at: stacks-block-height, reviews-given: u0, reviews-received: u0 })
      (map-set circle-stats
        { circle-id: circle-id }
        { total-submissions: u0, approved-submissions: u0, average-quality: u0 })
      (var-set next-circle-id (+ circle-id u1))
      (ok circle-id))))

(define-public (join-circle (circle-id uint))
  (let ((circle (map-get? review-circles { circle-id: circle-id })))
    (match circle
      circle-data
        (if (and 
          (get active circle-data)
          (< (len (get members circle-data)) MAX_CIRCLE_MEMBERS)
          (is-none (index-of (get members circle-data) tx-sender)))
          (begin
            (map-set review-circles
              { circle-id: circle-id }
              (merge circle-data { 
                members: (unwrap! (as-max-len? (append (get members circle-data) tx-sender) u5) (err ERR_CIRCLE_FULL))
              }))
            (map-set circle-membership
              { user: tx-sender, circle-id: circle-id }
              { joined-at: stacks-block-height, reviews-given: u0, reviews-received: u0 })
            (ok true))
          (err ERR_CIRCLE_FULL))
      (err ERR_INVALID_CIRCLE))))

(define-public (submit-for-review (circle-id uint) (content-hash (string-ascii 64)))
  (let ((circle (map-get? review-circles { circle-id: circle-id }))
        (membership (map-get? circle-membership { user: tx-sender, circle-id: circle-id }))
        (submission-id (var-get next-submission-id)))
    (match circle
      circle-data
        (match membership
          member-data
            (if (get active circle-data)
              (let ((reviews-needed (get min-reviews circle-data)))
                (begin
                  (map-set submissions
                    { submission-id: submission-id }
                    {
                      submitter: tx-sender,
                      circle-id: circle-id,
                      content-hash: content-hash,
                      reviews-needed: reviews-needed,
                      reviews-received: u0,
                      status: "pending",
                      created-at: stacks-block-height
                    })
                  (var-set next-submission-id (+ submission-id u1))
                  (ok submission-id)))
              (err ERR_INVALID_CIRCLE))
          (err ERR_NOT_MEMBER))
      (err ERR_INVALID_CIRCLE))))

(define-private (finalize-submission (submission-id uint))
  (let ((submission (unwrap! (map-get? submissions { submission-id: submission-id }) (err u1))))
    (let ((average-score (calculate-average-score submission-id))
          (circle-id (get circle-id submission)))
      (begin
        (if (>= average-score APPROVAL_THRESHOLD)
          (begin
            (map-set submissions
              { submission-id: submission-id }
              (merge submission { status: "approved" }))
            (update-user-reputation (get submitter submission) 5)
            (update-circle-stats circle-id true))
          (begin
            (map-set submissions
              { submission-id: submission-id }
              (merge submission { status: "rejected" }))
            (update-circle-stats circle-id false)))
        (let ((submitter-membership (unwrap! (map-get? circle-membership 
                { user: (get submitter submission), circle-id: circle-id }) (err u1))))
          (map-set circle-membership
            { user: (get submitter submission), circle-id: circle-id }
            (merge submitter-membership { 
              reviews-received: (+ (get reviews-received submitter-membership) u1) 
            })))
        (ok true)))))

(define-private (calculate-average-score (submission-id uint))
  (let ((submission (map-get? submissions { submission-id: submission-id })))
    (match submission
      submission-data
        (let ((total-score (fold + (get-all-review-scores submission-id (get reviews-received submission-data)) 0))
              (review-count (to-int (get reviews-received submission-data))))
          (if (> review-count 0)
            (/ total-score review-count)
            0))
      0)))

(define-private (get-all-review-scores (submission-id uint) (review-count uint))
  (list 75))

(define-private (update-circle-stats (circle-id uint) (approved bool))
  (let ((stats (default-to 
          { total-submissions: u0, approved-submissions: u0, average-quality: u0 }
          (map-get? circle-stats { circle-id: circle-id }))))
    (map-set circle-stats
      { circle-id: circle-id }
      {
        total-submissions: (+ (get total-submissions stats) u1),
        approved-submissions: (if approved 
          (+ (get approved-submissions stats) u1)
          (get approved-submissions stats)),
        average-quality: (if (> (get total-submissions stats) u0)
          (/ (* (get approved-submissions stats) u100) (get total-submissions stats))
          u0)
      })))

(define-read-only (get-circle-info (circle-id uint))
  (map-get? review-circles { circle-id: circle-id }))

(define-read-only (get-submission-status (submission-id uint))
  (map-get? submissions { submission-id: submission-id }))

(define-read-only (get-user-circle-stats (user principal) (circle-id uint))
  (map-get? circle-membership { user: user, circle-id: circle-id }))

(define-read-only (get-circle-performance (circle-id uint))
  (map-get? circle-stats { circle-id: circle-id }))

(define-public (leave-circle (circle-id uint))
  (let ((circle (map-get? review-circles { circle-id: circle-id }))
        (membership (map-get? circle-membership { user: tx-sender, circle-id: circle-id })))
    (match membership
      member-data
        (match circle
          circle-data
            (if (not (is-eq tx-sender (get creator circle-data)))
              (let ((updated-members (filter-member (get members circle-data) tx-sender)))
                (begin
                  (map-delete circle-membership { user: tx-sender, circle-id: circle-id })
                  (map-set review-circles
                    { circle-id: circle-id }
                    (merge circle-data { members: updated-members }))
                  (ok true)))
              (err ERR_NOT_MEMBER))
          (err ERR_INVALID_CIRCLE))
      (err ERR_NOT_MEMBER))))

(define-private (filter-member (members (list 5 principal)) (target principal))
  (filter is-not-target members))

(define-private (is-not-target (member principal))
  (not (is-eq member tx-sender)))

(define-private (update-user-reputation (user principal) (delta int))
  (let ((current-score (default-to { score: 0 } (map-get? user-reputation { user: user }))))
    (map-set user-reputation
      { user: user }
      { score: (+ (get score current-score) delta) })))

(define-map user-reputation
  { user: principal }
  { score: int })

(define-read-only (get-reputation (user principal))
  (default-to { score: 0 } (map-get? user-reputation { user: user })))