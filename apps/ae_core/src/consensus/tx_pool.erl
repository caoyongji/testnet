-module(tx_pool).
-behaviour(gen_server).

%% This module holds the txs ready for the next block.
%% It needs to use txs:digest to keep track of the Accounts and Channels dicts.
%% This module needs to be ready to share either of those dicts.

%% API
-export([data/0,
         dump/0,
         absorb_tx/2,
         absorb/3]).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(f, {txs = [],
            trees,
            height = 0}).

%% API functions

data() ->
    {Trees, Height, Txs} = gen_server:call(?MODULE, data),
    {Trees, Height, Txs}.

dump() ->
    gen_server:call(?MODULE, dump).

absorb_tx(Trees, Tx) ->
    gen_server:call(?MODULE, {absorb_tx, Trees, Tx}).

absorb(Trees, Txs, Height) ->
    gen_server:call(?MODULE, {absorb, Trees, Txs, Height}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).


%% gen_server callbacks

init(ok) ->
    lager:info("~p started", [?MODULE]),
    State = initial_state(),
    {ok, State}.

handle_call(dump, _From, _OldState) ->
    State = current_state(),
    {reply, 0, State};
handle_call({absorb_tx, NewTrees, Tx}, _From, F) ->
    NewTxs = [Tx | F#f.txs],
    BlockSize = size(term_to_binary(NewTxs)),
    Governance = trees:governance(NewTrees),
    MaxBlockSize = governance:get_value(max_block_size, Governance),
    FinalTxs =
        case BlockSize > MaxBlockSize of
            true ->
                lager:warning("Cannot absorb tx - block is already full"),
                F#f.txs;
            false ->
                NewTxs
        end,
    {reply, 0, F#f{txs = FinalTxs, trees = NewTrees}};
handle_call({absorb, NewTrees, Txs, _}, _From, _) ->
    {reply, 0, #f{txs = Txs, trees = NewTrees}};
handle_call(data, _From, F) ->
    {ok, Header} = headers:read(block:hash(headers:top())),
    H = headers:height(Header), %% Q. Why not using height in state? Change introduced in commit bbde8de
    {reply, {F#f.trees, H, lists:reverse(F#f.txs)}, F}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok = lager:warning("~p died!", [?MODULE]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internals

initial_state() ->
    Header = block:initialize_chain(),
    state_from_block(block:get_by_hash(Header)).

current_state() ->
    B = block:top(),
    state_from_block(B).

state_from_block(B) ->
    #f{trees = block:trees(B),
       height = block:height(B)}.
