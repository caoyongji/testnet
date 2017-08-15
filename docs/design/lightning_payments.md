We call `api:lightning_spend` to create a payment. 

We first grab the [scriptpubkey (SPK) and scriptsig (SS) scripts](smart_contracts.md) from `secrets:new_lightning`.

We then call `channel_feeder:make_locked_payment` to create a new bet for the given amount and add it to the list of bets in the existing channel SPK (for the pubkey for this IP and port) and return the updated SPK signed by us.

and also tells the other node to do the same via `talker:talk`. On the other node it goes to `ext_handler:doit({locked_payment` and then to `channel_feeder:handle_call({lock_spend` where `channel_feeder:make_locked_payment` is called once again (on the other node). Here it creates the offsetting transaction for `-Amount`, also by calling `make_locked_payment`. (edited)


[11:58] 
Note that lightning payments are implemented as _immediate bets_ of sorts. Where `make_locked_payment` creates a new bet and immediately applies it.


[11:59] 
@akorosmezey Answering your question #2… Two transactions are posted, one that credits the recipient and another one that debits the sender.
