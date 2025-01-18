import { describe, it, beforeEach, expect } from 'vitest';
let reputationScores: { [x: string]: any; };
let voteHistory: { voter: any; target: any; voteType: string; timestamp: any; }[];


beforeEach(() => {
  // Reset state before each test.
  reputationScores = {};
  voteHistory = [];
});




// Helper function to simulate contract methods.
function setReputation(user: string | number, score: any) {
  reputationScores[user] = score;
}

function getUserTier(user: string | number) {
  const score = reputationScores[user] || 0;

  if (score >= 200) return "Platinum";
  if (score >= 100) return "Gold";
  if (score >= 50) return "Silver";
  return "Bronze";
}

function calculateVoteWeight(user: string | number) {
  const score = reputationScores[user] || 0;

  if (score >= 100) return 2;
  if (score >= 50) return 1;
  return 1;
}

function weightedUpvote(targetUser: string | number, sender: any) {
  if (sender === targetUser) return { ok: false, error: "ERR_SELF_ACTION" };

  const weight = calculateVoteWeight(sender);
  reputationScores[targetUser] = (reputationScores[targetUser] || 0) + weight;

  return { ok: true };
}

function upvoteWithHistory(targetUser: any, sender: any, blockHeight: any) {
  if (sender === targetUser) return { ok: false, error: "ERR_SELF_ACTION" };

  const vote = {
    voter: sender,
    target: targetUser,
    voteType: "upvote",
    timestamp: blockHeight,
  };

  voteHistory.push(vote);
  return weightedUpvote(targetUser, sender);
}


const mockReputationSystem = {
  state: {
    users: {} as Record<string, number>, // Maps user principals to reputation scores
  },
  upvote: (targetUser: string, sender: string) => {
    if (targetUser === sender) {
      return { error: -2 }; // ERR_SELF_ACTION
    }
    mockReputationSystem.state.users[targetUser] =
      (mockReputationSystem.state.users[targetUser] || 0) + 1;
    return { value: true };
  },
  downvote: (targetUser: string, sender: string) => {
    if (targetUser === sender) {
      return { error: -2 }; // ERR_SELF_ACTION
    }
    mockReputationSystem.state.users[targetUser] =
      (mockReputationSystem.state.users[targetUser] || 0) - 1;
    return { value: true };
  },
  getReputation: (user: string) => {
    return mockReputationSystem.state.users[user] || 0;
  },
};

describe('User Reputation System', () => {
  let user1: string, user2: string;

  beforeEach(() => {
    user1 = 'ST1234...';
    user2 = 'ST5678...';
    mockReputationSystem.state = { users: {} };
  });

  it('should allow a user to upvote another user', () => {
    const result = mockReputationSystem.upvote(user2, user1);
    expect(result).toEqual({ value: true });
    expect(mockReputationSystem.getReputation(user2)).toBe(1);
  });

  it('should prevent a user from upvoting themselves', () => {
    const result = mockReputationSystem.upvote(user1, user1);
    expect(result).toEqual({ error: -2 });
    expect(mockReputationSystem.getReputation(user1)).toBe(0);
  });

  it('should allow a user to downvote another user', () => {
    const result = mockReputationSystem.downvote(user2, user1);
    expect(result).toEqual({ value: true });
    expect(mockReputationSystem.getReputation(user2)).toBe(-1);
  });

  it('should prevent a user from downvoting themselves', () => {
    const result = mockReputationSystem.downvote(user1, user1);
    expect(result).toEqual({ error: -2 });
    expect(mockReputationSystem.getReputation(user1)).toBe(0);
  });

  it('should return the correct reputation score for a user', () => {
    mockReputationSystem.upvote(user2, user1);
    mockReputationSystem.upvote(user2, user1);
    const reputation = mockReputationSystem.getReputation(user2);
    expect(reputation).toBe(2);
  });

  it('should return 0 for a user with no reputation score', () => {
    const reputation = mockReputationSystem.getReputation('ST9999...');
    expect(reputation).toBe(0);
  });
});



describe("User Tier Tests", () => {
  it("should assign Bronze tier for users with score < 50", () => {
    setReputation("user1", 30);
    const tier = getUserTier("user1");
    expect(tier).toBe("Bronze");
  });

  it("should assign Silver tier for users with score >= 50", () => {
    setReputation("user1", 60);
    const tier = getUserTier("user1");
    expect(tier).toBe("Silver");
  });

  it("should assign Gold tier for users with score >= 100", () => {
    setReputation("user1", 120);
    const tier = getUserTier("user1");
    expect(tier).toBe("Gold");
  });

  it("should assign Platinum tier for users with score >= 200", () => {
    setReputation("user1", 250);
    const tier = getUserTier("user1");
    expect(tier).toBe("Platinum");
  });
});

describe("Vote Weight Tests", () => {
  it("should assign weight 1 for users with score < 50", () => {
    setReputation("user1", 30);
    const weight = calculateVoteWeight("user1");
    expect(weight).toBe(1);
  });

  it("should assign weight 1 for users with score >= 50 but < 100", () => {
    setReputation("user1", 60);
    const weight = calculateVoteWeight("user1");
    expect(weight).toBe(1);
  });

  it("should assign weight 2 for users with score >= 100", () => {
    setReputation("user1", 120);
    const weight = calculateVoteWeight("user1");
    expect(weight).toBe(2);
  });
});

describe("Weighted Upvote Tests", () => {
  it("should increase target user's reputation based on sender's weight", () => {
    setReputation("user1", 120); // Gold user
    setReputation("user2", 50); // Silver user

    const result = weightedUpvote("user2", "user1");
    expect(result.ok).toBe(true);
    expect(reputationScores["user2"]).toBe(52); // user1's weight is 2
  });

  it("should prevent self-upvotes", () => {
    setReputation("user1", 120); // Gold user

    const result = weightedUpvote("user1", "user1");
    expect(result.ok).toBe(false);
    expect(result.error).toBe("ERR_SELF_ACTION");
  });
});

describe("Vote History Tests", () => {
  it("should record an upvote in vote history", () => {
    const blockHeight = 100;
    const result = upvoteWithHistory("user2", "user1", blockHeight);

    expect(result.ok).toBe(true);
    expect(voteHistory).toContainEqual({
      voter: "user1",
      target: "user2",
      voteType: "upvote",
      timestamp: blockHeight,
    });
  });

  it("should prevent self-upvotes and not record in history", () => {
    const blockHeight = 100;
    const result = upvoteWithHistory("user1", "user1", blockHeight);

    expect(result.ok).toBe(false);
    expect(result.error).toBe("ERR_SELF_ACTION");
    expect(voteHistory.length).toBe(0);
  });
});