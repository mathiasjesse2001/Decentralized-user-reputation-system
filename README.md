
# User Reputation System Smart Contract 🌟

**Description:**  
The User Reputation System Smart Contract enables a decentralized reputation management system where users can upvote or downvote other users. The reputation score of each user is stored on the blockchain, and users can view their own and others' scores. This system allows for interaction and feedback among users while preventing self-voting.

---

## Features 🚀

- **Upvote:**  
  Users can upvote other users, increasing their reputation score by 1 point.
  
- **Downvote:**  
  Users can downvote other users, decreasing their reputation score by 1 point.

- **Prevent Self-voting:**  
  Users cannot upvote or downvote themselves. Attempts to do so will return an error.

- **Reputation Lookup:**  
  Users can view the reputation score of any user on the platform.

---

## Contract Functions 📜

### Public Functions  

#### `upvote`  
**Parameters:**  
- `target-user (principal)`: The user to be upvoted.

**Behavior:**  
- Increases the reputation score of the target user by 1.
- Prevents self-voting (a user cannot upvote themselves).

**Returns:**  
- `ok true`: If the upvote was successful.
- `err ERR_SELF_ACTION`: If the user tries to upvote themselves.

---

#### `downvote`  
**Parameters:**  
- `target-user (principal)`: The user to be downvoted.

**Behavior:**  
- Decreases the reputation score of the target user by 1.
- Prevents self-voting (a user cannot downvote themselves).

**Returns:**  
- `ok true`: If the downvote was successful.
- `err ERR_SELF_ACTION`: If the user tries to downvote themselves.

---

#### `get-reputation`  
**Parameters:**  
- `user (principal)`: The user whose reputation score is being queried.

**Returns:**  
- `score (int)`: The reputation score of the requested user. If the user has no reputation score, it defaults to 0.

---

## Unit Tests 🧪

The following unit tests ensure that the contract functions correctly:

### Test Cases:

1. **Upvote Other User:**  
   Users can successfully upvote another user, increasing their reputation score by 1.

2. **Prevent Self-upvoting:**  
   The contract prevents users from upvoting themselves and returns the appropriate error.

3. **Downvote Other User:**  
   Users can successfully downvote another user, decreasing their reputation score by 1.

4. **Prevent Self-downvoting:**  
   The contract prevents users from downvoting themselves and returns the appropriate error.

5. **Get Reputation:**  
   Users can retrieve the correct reputation score for any user. If the user has no score, the system returns 0.

---

## Example Usage 📝

### Upvote a User:
```clarity
(upvote target-user)
```

### Downvote a User:
```clarity
(downvote target-user)
```

### Get User Reputation:
```clarity
(get-reputation user)
```

---

## Deployment 🚀  

To deploy the contract:
1. Deploy the contract on the desired blockchain network.  
2. Enable interaction via a suitable front-end or interface, allowing users to upvote, downvote, and view reputation scores.  
3. Run the unit tests to ensure all functions are working correctly.

---

## License 📄  

This project is open-source and available under the MIT License.
