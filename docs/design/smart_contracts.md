[Chalang](https://github.com/zack-bitcoin/chalang) implements the smart contract (script) language.

[ScriptPubKey (SPK)](https://bitcoin.org/en/glossary/pubkey-script) is a Bitcoin term.

Scripts are run by `api:channel_balance`, `api:channel_close`, `channel_feeder:handle_cast` (close), `channel_feeder:handle_call` (trade), etc.

Scripts are also run indirectly by other functions in the `spk` module, e.g. `bet_unlock`.

Channel slash and solo close transactions run channel scripts. 

The `market` module uses the `market.fs` Chalang script to run markets.

Lightning payments are implemented by running a script built into `secrets:new_lightning`.

A regular channel close runs the channel script locally and asks the remote to run it.

#### Oracle trades

Trading is implemented by the `oracle_bet.fs` script, supplied by `constants:oracle_bet` and used by `api:trade` and `channel_feeder:handle_call` (trade).


