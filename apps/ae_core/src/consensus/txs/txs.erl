-module(txs).

%% API
-export([digest/3,
         fees/1]).

%% API functions

digest([], Trees, _) ->
    Trees;
digest([SignedTx | Txs], Trees, Height) ->
    true = testnet_sign:verify(SignedTx),
    Tx = testnet_sign:data(SignedTx),
    NewTrees = digest2(Tx, Trees, Height),
    digest(Txs, NewTrees, Height).
digest2(Tx, Trees, H) ->
    case element(1, Tx) of
        create_acc_tx -> create_account_tx:doit(Tx, Trees, H);
        spend -> spend_tx:doit(Tx, Trees, H);
        delete_acc_tx -> delete_account_tx:doit(Tx, Trees, H);
        %repo -> repo_tx:doit(Tx, Trees, H);
        nc -> new_channel_tx:doit(Tx, Trees, H);
        gc -> grow_channel_tx:doit(Tx, Trees, H);
        ctc -> channel_team_close_tx:doit(Tx, Trees, H);
        %cr -> channel_repo_tx:doit(Tx, Trees, H);
        csc -> channel_solo_close_tx:doit(Tx, Trees, H);
        timeout -> channel_timeout_tx:doit(Tx, Trees, H);
        cs -> channel_slash_tx:doit(Tx, Trees, H);
        ex -> existence_tx:doit(Tx, Trees, H);
        oracle_new -> oracle_new_tx:doit(Tx, Trees, H);
        oracle_bet -> oracle_bet_tx:doit(Tx, Trees, H);
        oracle_close -> oracle_close_tx:doit(Tx, Trees, H);
        unmatched -> oracle_unmatched_tx:doit(Tx, Trees,H);
        oracle_shares -> oracle_shares_tx:doit(Tx,Trees,H);
        coinbase -> coinbase_tx:doit(Tx, Trees, H);
        X -> X = 2
    end.

fees([]) -> 0;
fees([H | T]) ->
    element(4, element(2, H)) + fees(T).
