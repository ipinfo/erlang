-module(ipinfo_cache).
-include_lib("kernel/include/logger.hrl").

-export([
    create/1,
    start_link/1,
    add/3,
    get/2
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(NOW, erlang:system_time(seconds)).
-ifdef (TEST).
-define(TIMER_INTERVAL, timer:seconds(1)).
-else.
-define(TIMER_INTERVAL, timer:minutes(1)).
-endif.

-record(state, {
    ttl :: non_neg_integer(),
    tid :: ets:tid()
}).

-record(item, {
    key        :: '_'  | binary() | nil,
    value      :: '_'  | term(),
    created_at :: '$1' | integer()
}).

-type state() :: #state{}.
-opaque t() :: pid().

-export_type([
    t/0
]).

-spec create(non_neg_integer()) -> {ok, t()}.
create(Ttl) ->
    supervisor:start_child(ipinfo_sup, #{
        id      => make_ref(),
        start   => {?MODULE, start_link, [Ttl]},
        restart => transient,
        type    => worker
    }).

start_link(Ttl) ->
    gen_server:start_link(?MODULE, #{ttl => Ttl}, []).

add(Cache, Key, Value) ->
    gen_server:call(Cache, #item{key = Key, value = Value, created_at = ?NOW}).

get(Cache, Key) ->
    gen_server:call(Cache, {get, Key}).

-spec init(map()) -> {ok, state()}.
%% @private
init(#{ttl := Ttl}) ->
    start_timer(),
    {ok, #state{
        ttl = Ttl,
        tid = ets:new(?MODULE, [{keypos, #item.key}])
    }}.

%% @private
handle_call({get, Key}, _From, #state{tid = Tid} = State) ->
    Reply = case ets:lookup(Tid, Key) of
        [#item{key = Key, value = Value}] ->
            {ok, Value};
        [] ->
            error
    end,
    {reply, Reply, State};
handle_call(#item{} = Item, _From, #state{tid = Tid} = State) ->
    true = ets:insert(Tid, Item),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(evict_obsolete, State) ->
    start_timer(),
    evict_obsolete(State),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

evict_obsolete(#state{tid = Tid, ttl = Ttl}) ->
    Now = ?NOW,
    ets:select_delete(Tid, [{
        _MatchHead = #item{created_at = '$1', _ = '_'},
        _Guards    = [{'<', {'+', '$1', Ttl}, Now}],
        _Result    = [true]
    }]).

start_timer() ->
    erlang:send_after(?TIMER_INTERVAL, self(), evict_obsolete).
