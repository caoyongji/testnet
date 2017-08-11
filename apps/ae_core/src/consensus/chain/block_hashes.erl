%%% Persists hashes of block headers, keeping copy in memory.
%%%
%%% Sizing consideration:
%%% Each blockhash is about 12 bytes. We need to prepare for about 10000000 blocks. So that would be 12 megabytes of data. We can keep this all in ram.

-module(block_hashes).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([save/1,
         is_known/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(LOC, constants:block_hashes()).

-record(state, {bh_hashes :: gb_sets:set(headers:block_header_hash())}).

%%% API"

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, ok, []).

-spec save(headers:block_header_hash()) -> ok.
save(H) ->
    true = size(H) == constants:hash_size(),
    gen_server:cast(?MODULE, {save, H}).

-spec is_known(headers:block_header_hash()) -> boolean().
is_known(H) ->
    true = size(H) == constants:hash_size(),
    gen_server:call(?MODULE, {is_known, H}).

%%% gen_server callbacks

init(ok) ->
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    K = if
            X == "" ->
                gb_sets:new();
            true -> X
        end,
    {ok, #state{bh_hashes = K}}.

handle_call({is_known, H}, _From, State) ->
    Reply = gb_sets:is_member(H, State#state.bh_hashes),
    {reply, Reply, State};
handle_call(Request, From, State) ->
    lager:warning("Ignoring unknown call request from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast({save, H}, State) ->
    NewState = State#state{bh_hashes = gb_sets:add(H, State#state.bh_hashes)},
    save_state(NewState), %This line is only necessary for power failures
    {noreply, NewState};
handle_cast(Msg, State) ->
    lager:warning("Ignoring unknown cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:warning("Ignoring unknown info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    save_state(State),
    lager:warning("~p died", [?SERVER]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

save_state(State) ->
    db:save(?LOC, State#state.bh_hashes).
