Shares can be found in 2 forms, positive and negative, and are needed to bet on the yes/no oracle outcomes.

If you own positive shares of form X, and receive negative shares of the same form, they cancel out and become normal AE tokens.

Similarly, AE tokens can be transformed into equal amounts of positive and negative forms of the same share.

Each kind of share has the same difficulty written on it.

#### Development notes

Shares are deserialized by `spk:run`. 

`shares:to_code` is only used by `test_txs:test(10)`.

Share id is the difficulty at the time the share was issued.

`modified` is the height of the block when the share was issued.

Shares are created from an oracle bet (`oracle_bets:to_shares`) as two-element list of positive shares, followed by negative shares. This happens when you create an oracle shares transaction (`oracle_shares_tx`). 

This is normally done by calling `api:oracle_shares` but also happens when creating a transaction digest (`txs:digest`), e.g. from `block:new_trees`.

Shares pay out (`shares:get_paid`) only if the block difficulty at the time the share was issued is greater than the share difficulty.

The conversion ratio of shares to tokens is controlled by the `shares_conversion` governance value.

Shares are converted to tokens by `shares:receive_shares`, called from `accounts:receive_shares`.

#### Issues

The share id is supposed to be the difficulty. It doesn't look like this implemented, though. The bet id is used as the share id in `oracle_bets:to_shares`. The bet id is set to be the oracle id in `oracle_bet_tx:give_bets_main` and `oracle_bet_tx:give_bets`. Share difficulty is compared to the block difficulty in `shares:get_paid2` but, again, the share id is not its difficulty so this will never work.

The sole purpose of shares seems to be to depend on difficulty within the system. It doesn't look like the user is aware of the fact that their shares may not pay out. Also, shares get converted back to tokens at a ratio in `shares:get_paid`. It looks like this may introduce new tokens into the system in addition to mining. 

It should be enough to bet with tokens. Dispensing with the shares and dropping the dependency on difficulty should simplify the code a lot. Any tokens from lost bets should be moved to the winning bets, keeping the supply of tokens constant. 
