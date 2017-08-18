Governance decides a different minimum fee size for each transaction type.
The miner profits by including transactions that pay above the minimum fee for that transaction type.

These are the 18 types of transaction that can be in blocks.

4 transactions for accounts:
* create_account
* spend
* delete_account
* account_recycle -- not implemented yet

7 transactions for channels:
* new_channel
* grow_channel
* channel_team_close
* channel_solo_close
* channel_slash
* channel_timeout
* channel_recycle -- not implemented yet

5 transactions for the oracle:
* oracle_new
* oracle_bet
* oracle_close
* oracle_unmatched
* oracle_shares

2 bonus transactions:
* existence
* burn

1 transaction for the miner:
* coinbase

# create_account

This creates a new account on the blockchain and gives it some tokens and possibly some shares.
The account loses a little money every block. This stops attackers from making tons of accounts to spam the network.

# spend

Spends AE tokens and/or shares to a different account.

# delete_account

This deletes an account on the blockchain and sends all of it's AE tokens to a different account.
Warning, this deletes all your share. Make sure to use the spend transaction to move all your shares somewhere else before deleting your account.

# account_recycle

If an account runs out of money anyone can do this transaction to delete the account. The user who deletes the empty account recieves a reward which is smaller than the cost of creating an account.

# new_channel

This creates a new channel on the blockchain.
It needs to be signed by both participants in the channel.
It takes money from both participant's accounts to put into the channel.
Channels can only hold AE tokens.
There is a record of recently closed channels. You can't reuse an id from a recently closed channel. You can reuse an id from a channel closed not recently, i.e. modified at most `channel_closed_time` - as per governance - blocks ago.
The channel loses a little money every block. This is to stop attackers from making lots of channels to spam the network.

# grow_channel

This adds more money to an existing channel.
Both parties need to sign.
It can take AE tokens from both accounts.
This transaction has a hashlock, so it can be connected to a channel payment or a spend transaction.
This transaction can update the minimum nonce accepted when a channel is closed. (important if you ever increase the delay period for closing the channel)

# channel_team_close

Both parties need to sign.
This closes the channel.
The AE tokens in the channel are distributed to the 2 account owners in the form of AE tokens and shares of any type.

# channel_solo_close

If your partner disappears, or refuses to close the channel, this is how you can start the process of closing the channel without your partner's help.

# channel_slash

If channel participant does a channel_solo_close at the wrong state, this is how you stop them.
Anyone is allowed to publish this tx, it doesn't have to be one of the two channel participants.
The blockchain records who closed a channel most recently in a tree.
channel ids can't be reused for a long time, specifically for `channel_closed_time` - as per governance - blocks since the last modification, so the record will be good.
The channel can be slashed many times, but each time it is slashed the evidence needs to be for a higher nonce.

# channel_timeout

If you did a channel_solo_close, and then waited the delay number of blocks after the final channel_slash, now you can do this transaction to close the channel.

# channel_recycle

If a channel has no money in it, then anyone can use this transaction to delete the channel.
The maker of this transaction gets a reward which is smaller than the cost of making the channel.

# oracle_new

This asks the oracle a question.
The oracle can only answer true/false questions.
Running the oracle costs a fee which is used as a reward to get people to use the oracle.
The fact that an oracle exists is recorded on the blockchain in a way that is accessible to the VM. So we can use channels to make smart contracts to raise funds to run the oracle.
The entire text of the question is written into the transaction, but only the hash of the text is stored into a consensus state merkel tree.
The oracle has a start-date written in it. Trading doesn't start until the start-date.
The oracle can be published before we know the outcome of the question, that way the oracle id can be used to make channel contracts that bet on the eventual outcome of the oracle.

# oracle_bet

This is how you can participate in an existing oracle.
The market is an order book with 3 types of shares: "true", "false", "bad_question"
All trades are matched into the order book in pairs at even odds.
So the order book only stores 1 kind of order at a time.
If you want your order to be held in the order book, it needs to be bigger than a minimum size.
The minimum size gets bigger as the order book gets bigger. So if there is C number of coins in the order book, the number of orders is smaller than `log2(C/(minimum for first order))`
If your order isn't big enough to be in the order book, you cannot buy shares of the type that are stored in the order book.

# oracle_close

If there is a lot of open orders for one type of share in an oracle for a long enough period of time, then this transaction can be done.
This ends betting in the market.
The fee that was used to start the oracle is the final bet included. It bets against the winning outcome.

# oracle_unmatched

If you had money in orders in the oracle order book when the oracle_close transaction happened, this is how you get the money out.

# oracle_shares

If you bet in an oracle, and the oracle has closed, this is how you get your shares out.
If you bet on the winning outcome, then you get positive shares. If you bet on one of the losing outcomes, then you get negative shares.
[you can read about shares here](shares.md)
The difficulty of the shares was announced when the oracle was launched.

# existence

This transaction adds 256 bits of data to the existence tree. This is done to prove that certain data existed at a certain time.
The virtual machine can verify that data exists in the existence tree.

# burn

This destroys some coins in a provable way.
There will be a merkel tree that stores by address. It stores the number of coins that that address has burned.
Anyone holding the private key for that address can prove how big of a burn they did.

# coinbase

This transaction contains the identifier of the account of the miner, so that it can be rewarded.
It is included in the block by the miner.
This transaction has no fee.
