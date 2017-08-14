#### Motivation

Users want to use blockchains to gamble on future events. They want to gamble on the price of goods, for example.

An oracle consensus mechanism is a game on a blockchain designed so that the players are incentivized to reveal true facts about the world to the blockchain. Once the blockchain learns facts about the world, it can resolve the bets. Winning gamblers get their money, losing gamblers lose their money.

The oracle consensus mechanism's rewards and punishments are all measured in tokens.

#### Difficulty

There are limitations of using betting as an oracle.

If the oracle lied, then the users could do a hard-fork to fix the oracle's answer. So the attackers would lose all the money in the attack, and that money would reward the users who participated in defense.

If the miners prefer the honest chain, then the difficulty of finding blocks on the dishonest chain will go very low. So the attackers would lose their money on both forks, and defenders would be rewarded on both forks.

The code is focused on punishing the _dishonest fork_ so if we find ourselves with low (lower?) difficulty then we won’t pay out shares, etc. Hence the requirement to get the difficulty oracle going first and have people bet on expected difficulty, then have subsequent oracles refer to the difficulty oracle.

We focus on the blockchain difficulty to determine if our fork is _honest_ and then punish users by not converting their shares into tokens, etc. We also require users to start the difficulty oracle, bet on it and wait for the outcome, all of it before starting a regular oracles. 

There is still one case where an attacker can make the oracle lie. The attacker commits to buying up enough coins on the lying fork, so that the difficulty of the lying fork is higher than the difficulty of the honest fork. A blockchain with higher difficulty is more valuable. So the result of this attack is a blockchain that is more valuable.

Making the blockchain valuable is more important than making the oracle honest. So in this situation, it is acceptable for the oracle to lie.

#### Storing oracles on the blockchain

For questions that are in the process of being answered, we store a market in the on-chain state.

The market remembers how many shares of each type have been sold, and it remembers what it's initial liquidity was, and it should have an order book.

The market has 4 possible outcomes:

1. difficulty goes up, and oracle's outcome is true
2. difficulty goes up, and oracle's outcome is false
3. difficultyzd goes down, and oracle's outcome is true
4. difficulty goes down, and oracle's outcome is false

The initial liquidity will be collected using an off-chain dominant assurance contract.

the result of the oracle is determined by which side of the order book has open orders. If one side has open orders for a long enough period of time, then that side wins.

Since we use an order book, it is expensive for attackers to DDOS us by moving the price past 50% every time the oracle is almost done.

#### Development notes

Difficulty or governance oracles are started with an empty question (`<<"">>`).

Each oracle lets us bet on a Yes/No/Bad question outcome, where bad question is one that can't be answered with a yes or no.

We are supposed to start a difficulty oracle before any others, close it and then use the outcome as difficulty in the question oracle. The outcome is supposed to be "bad" for the difficulty guess to be acceptable. Also, we are supposed to set the difficulty of the question oracle to 1/2 of the difficulty oracle we are referring to.

Oracle initial liquidity is used in `orders:significant_volume`. 

Also, in `oracle_bet_tx:doit2` when inserting one last bet. It's done for an assertion, though, to crash if the bet amount is greater than initial liquidity adjusted for the number of orders.

The account starting the oracle gets debited the initial liquidity, plus fee. 

Q: What does initial liquidity affect? Is it necessary?

Q: Why is the final bet in `oracle_close_tx` created using `constants:oracle_initial_liquidity` and not the governance value?

Q: Shares are converted to tokens at a ratio. Doesn't this potentially create new tokens or burn some?

Q: How do I know if there are unmatched orders for me to get back via `api:oracle_unmatched`? Also, where do I get required order id?
