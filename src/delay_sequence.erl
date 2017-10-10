-module(delay_sequence).

-behaviour(gen_server).

%% API
-export([start_link/2,
        has_next/0,
        next/0,
        get_next_delay_index/0,
        spend_current_delay_index/0,
        is_end_current_delay_seq/0,
        print_sequence/0, stop/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {delay_seq=[], delay_bound, max_delay_indx, current_delay_seq_indx, count}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link(DelayBound::non_neg_integer(), MaxDelayIndex::pos_integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(DelayBound, MaxDelayIndex) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [DelayBound, MaxDelayIndex], []).

has_next() ->
  gen_server:call(?SERVER, has_next).

next() ->
  gen_server:call(?SERVER, next).

get_next_delay_index() ->
  gen_server:call(?SERVER, get_next_delay_index).

spend_current_delay_index() ->
  gen_server:call(?SERVER, spend_current_delay_index).

is_end_current_delay_seq() ->
  gen_server:call(?SERVER, is_end_current_delay_seq).

print_sequence() ->
  gen_server:call(?SERVER, print_sequence).

stop() ->
  gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([DelayBound, MaxDelayIndex]) ->
  InitState =
    #state{delay_seq = [],
      delay_bound = DelayBound,
      max_delay_indx = MaxDelayIndex,
      current_delay_seq_indx = 0,
      count = 0},
  {ok, InitState}.

handle_call(has_next, _From, State) ->
  DelayBound = State#state.delay_bound,
  MaxDelayIndex = State#state.max_delay_indx,
  Reply =
    case State#state.delay_seq of
     [] when DelayBound > 0 -> true;
     [_H | _T] -> lists:any(fun(E) ->
                              E < MaxDelayIndex
                            end, State#state.delay_seq);
      _Else -> false
    end,
  {reply, Reply, State};

handle_call(next, _From, State) ->
  DelaySeq = State#state.delay_seq,
  DelayBound = State#state.delay_bound,
  MaxDelayIndex = State#state.max_delay_indx,

  NewDelaySeq =
    case DelaySeq of
      [] ->
        lists:seq(1, DelayBound);
      [_|_] ->
        {L1, L2} =
          lists:splitwith(fun(N) ->
                            N < MaxDelayIndex
                          end, DelaySeq),
        case L1 of
          [] ->
            L2;
          _Else ->
            Last = lists:last(L1),
            L1NewLast = Last + 1,
            L1_1 = lists:delete(Last, L1),
            NewL1 = L1_1 ++ [L1NewLast],
            {NewL2, _} = lists:mapfoldl(fun(_E, I) ->
                                          %% Sanity check
                                          true = I =< length(L2),
                                          {min(L1NewLast + I, MaxDelayIndex), I+1}
                                        end, 1, L2),
            NewL1 ++ NewL2
        end
    end,
  Count = State#state.count,
  NewState = State#state{delay_seq = NewDelaySeq, current_delay_seq_indx = 1, count = Count+1},
  {reply, ok, NewState};

handle_call(get_next_delay_index, _From, State) ->
  DelaySeq = State#state.delay_seq,
  CurrDSIndex = State#state.current_delay_seq_indx,
  DelayBound = State#state.delay_bound,
  MaxDelayIndex = State#state.max_delay_indx,

  {L1, _L2} =
    lists:splitwith(fun(N) ->
                      N < MaxDelayIndex
                    end, DelaySeq),

  {NewState, DelayIndex} =
    if
      L1 == [] ->
        {State, 0};
      CurrDSIndex =< length(L1) ->
        if
          (CurrDSIndex >= 1) and (CurrDSIndex =< DelayBound) ->
            DelIdx = lists:nth(CurrDSIndex, DelaySeq),
            {State, DelIdx};
          true ->
            {State, 0}
        end;
      CurrDSIndex > length(L1) ->
        {State, 0}
    end,
  {reply, DelayIndex, NewState};

handle_call(spend_current_delay_index, _From, State) ->
  CurrDelSeqIndex = State#state.current_delay_seq_indx,
  NewDSIndex = CurrDelSeqIndex+1,
  NewState = State#state{current_delay_seq_indx = NewDSIndex},
  {reply, ok, NewState};

handle_call(is_end_current_delay_seq, _From, State = #state{delay_bound = DelayBound, current_delay_seq_indx = CurrentDSIndex}) ->
  Reply =
    if
      CurrentDSIndex >= DelayBound ->
        true;
      true ->
        false
    end,
  {reply, Reply, State};

handle_call(print_sequence, _From, State) ->
  io:format("~p, ~p", [State#state.delay_seq, State#state.count]),
  {reply, ok ,State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(stop, State) ->
  {stop, normal, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================