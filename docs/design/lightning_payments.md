# Lightning payments


Send money to someone on the network that you are not sharing a state channel with.
If you have a path of channels to the recipient you can use the nodes in between as 
proxies in order to trustlessly send money to the recipient.

Lightning payments can be accomplished by deploying the same contract along the path (see the 
[Aeternity whitepaper](https://blockchain.aeternity.com/%C3%A6ternity-blockchain-whitepaper.pdf),
section II-B.1.a) that contains a hashlock. The payment is activated by revealing the secret of the 
hashlock.

(For terminology used, see the glossary at the end.)

## Workflow

We assume a channel is already opened to the proxy node with sufficient funds for the transaction.

### Deploying contracts
    `api:lightning_spend/5` inputs: proxy node IP, port, receiver's pubkey, amount, fee
    
#### Create new lightning
    `secrets:new_lightning/0`
1.  generate random (32) bytes, this will be used as a secret
2.  calculate the hash of it
3.  generate a compiled chalang **code** referring to the hash of the random bytes
create new bet with
    *   the code,
    *   code as key
    *   amount of payment and
    *   an empty proof
3.  apply the bet (i.e., add it to bets of the SPK)
    `spk:apply_bet/5` with
    *   0 (additional) amount,
    *   to SPK
    *   fixed 1000 time-gas and
    *   fixed 1000 space-gas
4.  (sanity check: match trees in `tx_pool:data()`)
5.  sign the new SPK

#### Send it to server via talker (locked_payment message)
    We are giving money conditionally, and asking us to forward it with a similar
    conditions to the receiver. The message passes on parameters of the payment,
    including the bet code and the code/secret pair encrypted with the pubkey of
    the recipient.
    The talker message processed on the proxy node:
1.  `channel_feeder:lock_spend/7`
    1.  First check that this channel is in the on-chain state with sufficient depth
        *   fail if amount <= 0
        *   fail if fee <= lightning fee
    3.  create signed return payment SPK (sanity check: test that same as the
        original one without the signature)
    4.  update channel data on the channel manager to
        * me: received (unsigned) SPK
        * them: received signed SPK
        * add empty binary to ScriptSig of both sides
    5.  add code, sender, recipient to arbitrage
    6.  create locked transaction to recipient with negative amount
2.  Return the signed SPK for return payment to sender.

#### ask channel_manager to update data of the channel to proxy node
    with the received SPK on both sides and adding empty signatures to ScriptSigs.


### Activating payments on the path: pulling the channel
    `api:pull_channel_state/1`, takes IP, port
1.   Fetch the pubkey of the proxy node
2.   Increment the entropy of the channel by 1
3.   Make sure the channel is live.
4.   Fetch channel data and its signed SPK from the proxy server
5.   Simplify these
     TODO: elaborate!
6.   Sync the result with the proxy node
7.   Decrypt messages in CD, learn secrets in them
8.   Unlock the secret.


## Glossary


### account
    A pubkey identifying a participant

### channel
    Connection between two participants

### CID
    Channel ID. A unique number assigned to the channel on the node

### bet
    Contains:
    *   code to execute
    *   amount that goes to one of the participant depending on the outcome
    *   prove
    *   key: used to pattern-match that use the same contract. aiding channel updating.
        The code itself is used as key for lightning payments.

### SPK, script pubkey
    Script pubkey(SPK) as Satoshi named it
    There is a ScriptSig(SS) in addition.
    Both participants sign the SPK, only one of them signs the SS.
    A record that contains
    *   CID
    *   accounts of the two endpoints
    *   account 2
    *   bets to be played
    *   space and time gas (fees to be paid for storage and execution)
    *   amount: money paid deposited into the channel by participants when the channel is
        created and used during execution
    *   nonce
    *   entropy
    *   delay: the maximum time to get the money out of the channel. Used in order to have
        a chance to act when the other party tries to close the channel dishonestly, 
        taking its money. 

### SS, script sig
    List of items to be signed by one of the participants.

### channel data
    *   me
        the highest-nonced SPK signed by me
    *   them
        the highest-nonced SPK signed by them
    *   ssme
        the highest nonced ScriptSig that works with me
    *   ssthem
        the highest nonced ScriptSig that works with them
    *   emsg
        encoded message? And why encoded for the proxy??
    *   live
        setting it to false marks channel as being closed, no more data changes
    *   entropy
    *   cid
        channel id

### gas
    Fee for using resources: space gas for storage and time-gas for execution.
