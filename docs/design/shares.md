Shares can be found in 2 forms, positive and negative, and are needed to bet on the yes/no oracle outcomes.

If you own positive shares of form X, and receive negative shares of the same form, they cancel out and become normal AE tokens.

Similarly, AE tokens can be transformed into equal amounts of positive and negative forms of the same share.

Each kind of share has the same difficulty written on it.

Shares are deserialized by `spk:run`. 

`shares:to_code` is only used by `test_txs:test(10)`.

Share id is the difficulty at the time the share was issued. This doesn't look like it's implemented, though.

`modified` is the height of the block when the share was issued.

Shares are created from an oracle bet (`oracle_bets:to_shares`) as two-element list of positive shares, followed by negative shares. This happens when you create an oracle shares transaction (`oracle_shares_tx`). 

This is normally done by calling `api:oracle_shares` but also happens when creating a transaction digest (`txs:digest`), e.g. from `block:new_trees`.

Shares pay out (`shares:get_paid`) only if the block difficulty at the time the share was issued is greater than the share difficulty.

The conversion ratio of shares to tokens is controlled by the `shares_conversion` governance value.

Shares are converted to tokens by `shares:receive_shares`, called from `accounts:receive_shares`.

