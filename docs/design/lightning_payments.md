# Lightning payments


Send money to someone on the network that you are not sharing a state channel with. If you have a path of channels to the recipient you can use the nodes in between as proxies in order to trustlessly send money to the recipient.

Lightning payments transfer funds between two participants connected through a single mediator node. It is accomplished by deploying the same contract on the two channels involved(see the [Aeternity whitepaper](https://blockchain.aeternity.com/%C3%A6ternity-blockchain-whitepaper.pdf), section II-B.1.a) that contains a hashlock. The payment is activated by revealing the secret of the hashlock.

(For terminology used, see the glossary at the end.)

## Workflow

We assume a channel is already opened to the proxy node with sufficient funds for the transaction.

### Deploying contracts
    Create payment:
    Call `api:lightning_spend/5` with inputs: proxy node IP, port, receiver's pubkey, amount, fee

#### Create new lightning
We first grab the [scriptpubkey (SPK) and scriptsig (SS) scripts](smart_contracts.md) from `secrets:new_lightning/0`:
1.  generate random (32) bytes, this will be used as a secret
2.  calculate the hash of it
3.  generate a compiled chalang **code** referring to the hash of the random bytes. It returns a stack of 4 values: 
        *   shared root: always []
        *   amount 0 or 1 times channel granularity
        *   nonce
        *   delay
    code:
    *   if the stack is empty: [[], 0, 1, 50]
    *   otherwise calculate hash of its top
        *   if matches the hash wired-in: [[], granularity, 2, 0]
        *   otherwise [[], 0, 1, 49]
4.  generate **secret**: a compiled chalang declaration containing the base64 value of
    the random bytes
5.  a sanity check is performed that creates and runs a new bet with arbitrary amount, accounts, CID and space/time gases
6.  return code, secret to `api:lightning_spend/7`

#### Encrypt code and secret with pubkey of the proxy node (immediate receiver, fetch key)

#### Make a locked payment

We call `channel_feeder:make_locked_payment` to create a new bet for the given amount and add it to the list of bets in the existing channel SPK (for the pubkey for this IP and port) and return the updated SPK signed by us.

`channel_feeder:make_locked_payment/4` with input: pubkey of the proxy node, amount + fee, the lightning code and an empty proof.
1.  take the highest-nonced SPK of our node from channel data (#cd.me)
2.  create new bet with
    *   the code,
    *   code as key
    *   amount of payment and
    *   an empty proof
3.  apply the bet (i.e., add it to the bets of the SPK)
    `spk:apply_bet/5` with
    *   0 (additional) amount,
    *   to SPK
    *   fixed 1000 time-gas and
    *   fixed 1000 space-gas
4.  (sanity check: match trees in `tx_pool:data()`)
5.  sign the new SPK and return it to `api:lightning_spend/7`.

#### Send it to server via talker (locked_payment message)

    We are giving money conditionally, and asking us to forward it with similar conditions to the receiver. The message passes on parameters of the payment, including the bet code and the code/secret pair encrypted with the pubkey of the recipient.

    We tell the proxy node to make a locked payment, too, via `talker:talk`. On that node it goes to `ext_handler:doit({locked_payment` and then to `channel_feeder:handle_call({lock_spend`. The node cretaes two locked payments: one debiting the sender and one crediting the receiver:
1.  `channel_feeder:handle_call({lock_spend`
    1.  First check that this channel is in the on-chain state with sufficient funds
        *   fail if amount <= 0
        *   fail if fee <= lightning fee
    3.  create signed payment SPK debiting the sender with amount + fee: `channel_feeder:make_locked_payment/4`
        (sanity check: test that same as the
        original one without the signature)
    4.  update channel data on the channel manager to
        * me: received (unsigned) SPK
        * them: received signed SPK
        * add empty binary to ScriptSig of both sides
    5.  add code, sender, recipient to arbitrage
    6.  create locked crediting transaction towards the recipient for -amount: `channel_feeder:make_locked_payment/4`
2.  Return the signed SPK for debit payment to sender.

#### sender updates channel data

`api:lightning_spend/5` asks channel_manager to update data of the channel to proxy node with the received SPK on both sides and adding empty signatures to ScriptSigs.


### Activating payments on the path: pulling the channel

Revealing the secret and updating of channels takes place when the channel is pulled.

`api:pull_channel_state/1`, takes IP, port
1.  Fetch the pubkey of the proxy node
2.  Increment the entropy of the channel by 1
3.  Make sure the channel is live.
4.  Fetch from the proxy server his view of
    *   the channel data and
    *   the SPK in it on our side, signed by him
5.  Simplify these
    `channel_feeder:they_simplify/3`, takes the public key of the proxy node,
    and the data data we fetched from the proxy node above
    1.  test if
        *   the channel id is live in the view of both sides
        *   the signature on the SPK fetched is valid
        *   the the fetched SPK and channel data are consistent
    2.  forced update of our SPK using our SS and their one from the fetched CD
        `spk:force_update/3`
        *   run with with both SSes
            *   `spk:run/6`:
                *   create Chalang state with block height we got from `tx_pool:data/0`
                    and slash = 0 (i.e., not running as a solo_stop or slash transaction)
                *   `spk:run2/5` fast mode (i.e., not crash safe/time limited)
                    `spk:run/8` SS, bets (=codes), gases and delay from SPK, fun/var limits from
                    governance
                    `spk:run/11`:
                    *   `spk:run3/7` with the first SS and code
                        *   crash if SS is ?crash
                        *   `spk:prove_facts/2`: nothing to prove as field `prove` is empty
                        *   `chalang:run5/2` both for SS and code
                        *   return {Amount * Bet#bet.amount div granularity, <<<< amount is just 0/1 * granularity
                            nonce, shareRoot, delay, time_gas}
                        *   `spk:run/11` for the remainder
                *   return {Amount + SPK#spk.amount, NewNonce + SPK#spk.nonce, Shares, Delay}
        *   Check nonces:
            *   unless new one is lower, to run all bets with the new SS'es and return
                the new bets, SS'es amount and nonce
            *   if higher,
                `spk:simplify_helper/2`
                    try to unlock bets in in the SPK of the CD in my side using their SS
                    `spk:bet_unlock/2`
                    *   `spk:bet_unlock/8`
                        *   if no secret can be read (`secret:read/1`) check next bet2
                        *   if secret available, try to run bet in chalang
                            *   if fails, try to run with 1st SS instead of secret
                                *   if that fails, too move on to next bet2
                                *   success: as next point
                            *   if succeeds, check delay in chalang stack
                                *   >50:  failure, move on
                                *   success, add SS to secrets, accumulate amount, move on
                    *   return remaining SS, updated SPK, learned secrets, accumulated SS
            *   return remaining SS, new SPK (theirs) signed
    3.  Check if the update returns the SPK and SS of other side.
        *   If it does, accept it on our side: update our CD with these on our side.
            Sign and return the received SPK. This means tha if the other party found
            a way to unlock funds, we agree to give them.
        *   If the update does not match it, accept if it is an improvement on our side.
            (i.e., accept if they want to give us money.) (@@TODO: elaborate improvement)
            Otherwise, try to unlock bets with the new SS received from the proxy node in
            the CD (their side).
6.  Sync the result with the proxy node (call `channel_feeder:update_to_me/2` on other node)
7.  Decrypt messages in CD, learn secrets in them
8.  Unlock the secret:
    `api:bet_unlock/2` (IP, port)
    *   `channel_feeder:bets_unlock/1` for their pubkey
        *   call `spk:bet_unlock/2` (as above):
            *   tries to unlock each bet with SS, SPK on our side
            *   returns new SS, SPK, secrets, their SPK signed
            *   update channel data with these
        *   returm learned secrets and our updated SPK
    *   teach the secrets found to proxy node
        `api:teach_secrets/4`: call learn_secret on proxy node for each secret found via talker
        *   `ext_handler:doit({learn_secret, ...)`
            *   `secrets:add/2`
            *   unlock bets (SS, SPK on the own side of the proxy node)
            *   if SS changed
                *   update channel data
                *   fetch pubkeys of other connected nodes referring to this code
                *   update bets on these channels, too
    *   update my side of channel data with the new SPK 
        `channel_feeder:update_to_me/2`

## Problems

1.  bet evaluation: uses granularity from constants but the chalang code in secrets.erl has it wired-in.

2.  chalang: duplication of opcodes, error-prone

3.  almost all tests rely on badmatch/crash

4.  code duplication: running chalang for bets in several places

## Chalang

### run
    expected to return [ShareRoot, Amount, Nonce, Delay | _] (`spk:run3`)

1. run5
   *   check well-formedness
   *   run2

2. run2
   functions parsed
   run4

3. run4
   basic constructs parsed


## Glossary


### account
    A pubkey identifying a participant.

### channel
    Connection between two participants, created in order to perform some transctions.

### CID
    Channel ID. A network-wide unique number assigned to the channel.

### bet
    Contains:
    *   code to execute
    *   amount that goes to one of the participant depending on the outcome
    *   prove
    *   key: used to pattern-match that use the same contract. aiding channel updating.
        The code itself is used as key for lightning payments.

### SPK, script pubkey
    [SPK, script pubkey](https://bitcoin.org/en/glossary/pubkey-script) is a Bitcoin term.
    There is a ScriptSig(SS) in addition, storing data for the SPK.
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
    *   delay: a delay value the two participants agreed upon when the channel was created.
        When one of them tries to close the channel unilaterally, the other has this much
        time to intervene. It is advisable to agree on a large enough value in order to
        protect ourselves from the other party's possible dishonesty.

### SS, script sig
    List of items to be signed by one of the participants.

### CD, channel data
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

### Chalang
    [Chalang](https://github.com/zack-bitcoin/chalang) a stack-based language implementing smart contracts.
