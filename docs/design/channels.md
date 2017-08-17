#### Scalability 

The channels in Aeternity are more scalable for 2 reasons.

1. Channels move computation off-chain, so we can parallelize the smart contracts. So there is no limit to the amount of smart contract computation that can happen per second. Ethereum channels also have this capability.

2. The channels don't share state, so we can process all the channel-transactions in a block in parallel. So there is no limit to the volume of channels that can be closed and opened per second. Ethereum channels do not have this capability.

Note: We seem to be limited to 1 channel per account, i.e. can't open more than one channel to another node.

#### How to use channels

Channels are a relationship between 2 participants.

Channels allow for the exchange of value dependent upon the outcome of turing complete contracts.
Channel contracts can be updated without recording anything to the blockchain.

Problem scenario:

I received several payments, but my partner closed the channel at the state from before I had received the payments. My channel partner stole my money.

Solution:

Each channel has a `delay` programmed in it.

If your partner tries to close the channel, you have at least this much time to provide data to the blockchain to make the channel close at the correct final state.

Make sure that the `delay` you and your partner agree to is set big enough so that you have time to stop them from stealing from you.

Additionally, you can hire an untrusted third party to provide evidence to the blockchain and stop your partner from stealing from you. You can pay them in a trust-free way by making another channel.

Use a `channel_slash` transaction.

Problem scenario:

I made a bet in my channel that wont settle for a long time, and my partner wont cooperate for me to make any more bets, and he wont cooperate for me to withdraw any money. My money is trapped in the channel. Maybe I have some other short term bets that settle, and the money from them is trapped too.

Solution:

Have a different channel for short term contracts and for long term contracts.

Don't put money into the long term channel until the channel state is already signed.

Remember: all the money in the channel can be trapped for as long as the longest delayed bet in the channel state with the highest nonce.

#### Transaction types

There are 5 transaction types:

2 that need to be signed by both: `new_channel`, `channel_team_close`,
and 3 that are signed by one: `channel_solo_close`, `channel_slash`, `channel_timeout`.

If your partner disappears, and you want to close the channel, then you first publish a solo close, which gives the current state of the channel, which is defined by a turing complete contract that outputs a nonce.

The contract is split into 2 parts, the scriptpubkey part, which both participants signed, and the scripsig part, which only one signed.

There is a third part that goes in the middle. The blockchain looks things up in the merkle tree, and provides that data to the smart contract too. This is useful if you are betting on the outcome of something in the oracle.

If your partner sees you publish a solo close that doesn't output the highest nonce possible, then they can do a channel slash transaction that outputs a higher nonce, and this becomes the final state that the channel gets closed at.

If your partner doesn't slash you, then eventually you can do a channel timeout transaction, which closes the channel at the state from the solo close.

The data that gets recorded on-chain for each channel is:
- How much money is in the channel. 
- The 2 accounts ids that control the channel.

#### Development notes

#### Issues

`api:market_match` 

Market making in a channel is implemented in `market.fs` (e.g. the order book) whereas betting against the oracle is implemented in the code. The Forth code is difficult to understand and will be even harder to test. We should rewrite market-making through an agent written in Erlang. 

`market:market_smart_contract` combines `market.fs` with `oracle_bet.fs`.

Betting and markets are heavily dependent on Chalang and Forth code.
