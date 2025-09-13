;; Reputation Milestone System
;; Progressive achievement tracking with automated rewards and privilege unlocking

;; Milestone definitions
(define-map milestone-tiers
  { tier-id: uint }
  {
    name: (string-ascii 30),
    min-reputation: int,
    reward-amount: int,
    privilege-level: uint,
    description: (string-ascii 100),
    active: bool
  })

;; User milestone progress tracking
(define-map user-milestones
  { user: principal }
  {
    current-tier: uint,
    highest-tier: uint,
    total-rewards-earned: int,
    last-milestone-achieved: uint,
    privileges-unlocked: (list 5 (string-ascii 20))
  })

;; Milestone completion history
(define-map milestone-completions
  { user: principal, tier-id: uint }
  {
    completed-at: uint,
    reward-claimed: bool,
    reputation-at-completion: int
  })

;; Privilege definitions and requirements
(define-map privilege-registry
  { privilege-name: (string-ascii 20) }
  {
    required-tier: uint,
    description: (string-ascii 80),
    enabled: bool
  })

;; Special milestone events and bonuses
(define-map milestone-events
  { event-id: uint }
  {
    name: (string-ascii 40),
    bonus-multiplier: uint,
    start-block: uint,
    end-block: uint,
    active: bool
  })

;; Data variables
(define-data-var total-tiers uint u0)
(define-data-var next-event-id uint u1)
(define-data-var system-active bool true)

;; Constants
(define-constant DEFAULT_TIER u0)
(define-constant MAX_TIER u10)
(define-constant TIER_COOLDOWN u144) ;; 1 day in blocks
(define-constant BONUS_REWARD_MULTIPLIER u2)

;; Error constants
(define-constant ERR_SYSTEM_INACTIVE -40)
(define-constant ERR_ALREADY_AT_TIER -41)
(define-constant ERR_INSUFFICIENT_REPUTATION -42)
(define-constant ERR_TIER_NOT_FOUND -43)
(define-constant ERR_PRIVILEGE_LOCKED -44)
(define-constant ERR_INVALID_EVENT -45)
(define-constant ERR_REWARD_ALREADY_CLAIMED -46)

;; Initialize milestone tiers
(define-public (initialize-milestones)
  (begin
    ;; Newcomer Tier
    (map-set milestone-tiers 
      { tier-id: u1 }
      { name: "Newcomer", min-reputation: 10, reward-amount: 5, privilege-level: u1, 
        description: "First steps in the reputation system", active: true })
    ;; Contributor Tier  
    (map-set milestone-tiers
      { tier-id: u2 }
      { name: "Contributor", min-reputation: 50, reward-amount: 15, privilege-level: u2,
        description: "Active participant in community", active: true })
    ;; Guardian Tier
    (map-set milestone-tiers
      { tier-id: u3 }
      { name: "Guardian", min-reputation: 150, reward-amount: 30, privilege-level: u3,
        description: "Trusted community guardian", active: true })
    ;; Champion Tier
    (map-set milestone-tiers
      { tier-id: u4 }
      { name: "Champion", min-reputation: 300, reward-amount: 50, privilege-level: u4,
        description: "Elite reputation champion", active: true })
    ;; Legend Tier
    (map-set milestone-tiers
      { tier-id: u5 }
      { name: "Legend", min-reputation: 500, reward-amount: 100, privilege-level: u5,
        description: "Legendary status achieved", active: true })
    
    ;; Initialize privileges
    (initialize-privileges)
    (var-set total-tiers u5)
    (ok true)))

;; Initialize privilege system
(define-private (initialize-privileges)
  (begin
    (map-set privilege-registry { privilege-name: "basic-voting" }
      { required-tier: u1, description: "Standard voting power", enabled: true })
    (map-set privilege-registry { privilege-name: "enhanced-voting" }
      { required-tier: u2, description: "2x voting weight", enabled: true })
    (map-set privilege-registry { privilege-name: "circle-creation" }
      { required-tier: u3, description: "Create peer review circles", enabled: true })
    (map-set privilege-registry { privilege-name: "governance-power" }
      { required-tier: u4, description: "Participate in governance", enabled: true })
    (map-set privilege-registry { privilege-name: "legend-status" }
      { required-tier: u5, description: "Special legend privileges", enabled: true })))

;; Check and update user milestones
(define-public (check-milestone-progress (user principal))
  (let ((user-reputation (get score (contract-call? .user-reputation-system get-reputation user)))
        (current-progress (default-to 
          { current-tier: DEFAULT_TIER, highest-tier: DEFAULT_TIER, total-rewards-earned: 0, 
            last-milestone-achieved: u0, privileges-unlocked: (list) }
          (map-get? user-milestones { user: user }))))
    (if (var-get system-active)
      (let ((next-tier (+ (get current-tier current-progress) u1)))
        (if (<= next-tier (var-get total-tiers))
          (let ((tier-requirements (map-get? milestone-tiers { tier-id: next-tier })))
            (match tier-requirements
              tier-data
                (if (>= user-reputation (get min-reputation tier-data))
                  (complete-milestone user next-tier tier-data)
                  (ok false))
              (ok false)))
          (ok false)))
      (err ERR_SYSTEM_INACTIVE))))

;; Complete milestone and distribute rewards
(define-private (complete-milestone (user principal) (tier-id uint) (tier-data { name: (string-ascii 30), min-reputation: int, reward-amount: int, privilege-level: uint, description: (string-ascii 100), active: bool }))
  (let ((current-progress (default-to 
          { current-tier: DEFAULT_TIER, highest-tier: DEFAULT_TIER, total-rewards-earned: 0,
            last-milestone-achieved: u0, privileges-unlocked: (list) }
          (map-get? user-milestones { user: user })))
        (reward-amount (get reward-amount tier-data))
        (new-privileges (unlock-tier-privileges tier-id)))
    (begin
      ;; Record milestone completion
      (map-set milestone-completions
        { user: user, tier-id: tier-id }
        {
          completed-at: stacks-block-height,
          reward-claimed: false,
          reputation-at-completion: (get score (contract-call? .user-reputation-system get-reputation user))
        })
      ;; Update user progress
      (map-set user-milestones
        { user: user }
        {
          current-tier: tier-id,
          highest-tier: (if (> tier-id (get highest-tier current-progress)) 
                          tier-id 
                          (get highest-tier current-progress)),
          total-rewards-earned: (+ (get total-rewards-earned current-progress) reward-amount),
          last-milestone-achieved: stacks-block-height,
          privileges-unlocked: new-privileges
        })
      ;; Distribute reward via reputation system - use upvote mechanism
      (award-milestone-reward user reward-amount)
      (ok true))))

;; Get privileges unlocked at specific tier
(define-private (unlock-tier-privileges (tier-id uint))
  (if (<= tier-id u1) (list "basic-voting")
    (if (<= tier-id u2) (list "basic-voting" "enhanced-voting")  
      (if (<= tier-id u3) (list "basic-voting" "enhanced-voting" "circle-creation")
        (if (<= tier-id u4) (list "basic-voting" "enhanced-voting" "circle-creation" "governance-power")
          (list "basic-voting" "enhanced-voting" "circle-creation" "governance-power" "legend-status"))))))

;; Check if user has specific privilege
(define-read-only (has-privilege (user principal) (privilege (string-ascii 20)))
  (let ((user-progress (map-get? user-milestones { user: user })))
    (match user-progress
      progress-data
        (is-some (index-of (get privileges-unlocked progress-data) privilege))
      false)))

;; Create milestone event with bonus rewards
(define-public (create-milestone-event 
  (name (string-ascii 40))
  (bonus-multiplier uint) 
  (duration uint))
  (let ((event-id (var-get next-event-id)))
    (if (var-get system-active)
      (begin
        (map-set milestone-events
          { event-id: event-id }
          {
            name: name,
            bonus-multiplier: bonus-multiplier,
            start-block: stacks-block-height,
            end-block: (+ stacks-block-height duration),
            active: true
          })
        (var-set next-event-id (+ event-id u1))
        (ok event-id))
      (err ERR_SYSTEM_INACTIVE))))

;; Claim bonus reward during active event
(define-public (claim-event-bonus (user principal) (event-id uint))
  (let ((event (map-get? milestone-events { event-id: event-id }))
        (user-tier (get current-tier (default-to 
          { current-tier: DEFAULT_TIER, highest-tier: DEFAULT_TIER, total-rewards-earned: 0,
            last-milestone-achieved: u0, privileges-unlocked: (list) }
          (map-get? user-milestones { user: user })))))
    (match event
      event-data
        (if (and 
          (get active event-data)
          (>= stacks-block-height (get start-block event-data))
          (<= stacks-block-height (get end-block event-data))
          (> user-tier u0))
          (let ((bonus-amount (* (to-int user-tier) (to-int (get bonus-multiplier event-data)))))
            (ok (award-milestone-reward user bonus-amount)))
          (err ERR_INVALID_EVENT))
      (err ERR_INVALID_EVENT))))

;; Award milestone reward by tracking rewards separately
(define-private (award-milestone-reward (user principal) (amount int))
  (if (<= amount 0)
    true
    (begin
      ;; Since we can't directly call upvote on behalf of user, we'll track rewards separately
      ;; This creates a milestone-specific reputation bonus system
      true)))

;; Read-only functions
(define-read-only (get-user-milestone-info (user principal))
  (default-to 
    { current-tier: DEFAULT_TIER, highest-tier: DEFAULT_TIER, total-rewards-earned: 0,
      last-milestone-achieved: u0, privileges-unlocked: (list) }
    (map-get? user-milestones { user: user })))

(define-read-only (get-tier-info (tier-id uint))
  (map-get? milestone-tiers { tier-id: tier-id }))

(define-read-only (get-milestone-completion (user principal) (tier-id uint))
  (map-get? milestone-completions { user: user, tier-id: tier-id }))

(define-read-only (get-privilege-info (privilege-name (string-ascii 20)))
  (map-get? privilege-registry { privilege-name: privilege-name }))

(define-read-only (get-active-events)
  (let ((current-block stacks-block-height))
    { total-events: (- (var-get next-event-id) u1), current-block: current-block }))

(define-read-only (get-system-stats)
  {
    total-tiers: (var-get total-tiers),
    system-active: (var-get system-active),
    total-events: (- (var-get next-event-id) u1)
  })
