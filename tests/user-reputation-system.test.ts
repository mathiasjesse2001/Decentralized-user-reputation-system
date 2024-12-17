import { describe, it, beforeEach, expect } from 'vitest';

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
