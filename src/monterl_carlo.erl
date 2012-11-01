%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @copyright (C) 2012, nisbus
%%% @doc
%%%
%%% @end
%%% Created : 31 Oct 2012 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(monterl_carlo).

-behaviour(gen_server).

%% API
-export([start_link/1,start_link/6, subscribe/2,unsubscribe/1,stop/1,graph/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, 
	{
	  symbol,
	  bid,
	  ask,
	  precision = 4,
	  annual_vol,
	  annual_exp_return,
	  interval=0,
	  return_mean,
	  daily_vol,
	  daily_return,
	  max = 0.0,
	  min = 0.0,
	  bp_diff =0.0,
	  average_bp_change =0.0,
	  last_bp_change=0.0,
	  max_bp_change=0.0,
	  min_bp_change=0.0,
	  loop_count=0,
	  updatefun,
	  timer
	}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Symbol) ->
    start_link(Symbol,25.5,5,0.1,0.1,1000).

start_link(Symbol,Px,Precision, AnnualVol, AnnualExpRet,Interval) when is_binary(Symbol) ->
    start_link(list_to_atom(binary_to_list(Symbol)),Px,Precision, AnnualVol, AnnualExpRet,Interval);

start_link(Symbol,Px,Precision, AnnualVol, AnnualExpRet,Interval) when is_list(Symbol) ->
    start_link(list_to_atom(Symbol),Px,Precision, AnnualVol, AnnualExpRet,Interval);

start_link(Symbol,Px,Precision, AnnualVol, AnnualExpRet,Interval) when is_atom(Symbol) ->
    DailyRet = (AnnualExpRet/100)/252,
    DailyVol = (AnnualVol/100)/math:sqrt(252),
    RetM = DailyRet-0.5*math:pow(DailyVol,2),
    gen_server:start_link({local, Symbol}, ?MODULE, [#state{
								      symbol=Symbol,
								      annual_vol=AnnualVol,
								      annual_exp_return=AnnualExpRet,
								      interval=Interval,
								      precision = Precision,
								      bid=Px,
								      return_mean=RetM,
								      daily_return=DailyRet,
								      daily_vol=DailyVol
								     }], []).

subscribe(Symbol,UpdateFun) when is_atom(Symbol) ->
    gen_server:cast(Symbol,{subscribe,UpdateFun});
subscribe(Symbol,UpdateFun) when is_binary(Symbol) ->
    subscribe(list_to_atom(binary_to_list(Symbol)),UpdateFun);
subscribe(Symbol,UpdateFun) when is_list(Symbol) ->
    subscribe(list_to_atom(Symbol),UpdateFun).

graph(Symbol,Points,Type) when is_atom(Symbol) and is_atom(Type) ->
    gen_server:call(Symbol, {graph,Type,Points});
graph(Symbol,Points,Type) when is_list(Symbol) and is_list(Type) ->
    graph(list_to_atom(Symbol),Points,list_to_atom(Type));
graph(Symbol,Points,Type) when is_binary(Symbol) and is_binary(Type) ->
    graph(list_to_atom(binary_to_list(Symbol)),Points,list_to_atom(binary_to_list(Type))).


unsubscribe(Symbol) when is_atom(Symbol) ->
    gen_server:cast(Symbol,unsubscribe);
unsubscribe(Symbol) when is_list(Symbol) ->
    unsubscribe(list_to_atom(Symbol));
unsubscribe(Symbol) when is_binary(Symbol) ->
    unsubscribe(list_to_atom(binary_to_list(Symbol))).

stop(Symbol) when is_atom(Symbol) ->
    gen_server:cast(Symbol,stop);
stop(Symbol) when is_list(Symbol) ->
    stop(list_to_atom(Symbol));
stop(Symbol) when is_binary(Symbol) ->
    stop(list_to_atom(binary_to_list(Symbol))).

init([State]) ->
    {ok, State}.
    
handle_call({graph,Type,Points}, _From, State) ->
    P = lists:seq(1,Points),
    {Result,NewState} = lists:foldl(fun(X,Acc) ->
					    {L,S} = Acc,
					    NS = calculate(S),
					    case Type of
						ask -> {[
							 [{<<"point">>,
							   [{<<"counter">>,X},
							    {<<"ask">>,NS#state.ask}
							   ]
							  }]|L],NS};
						both -> {[
							  [{<<"point">>,
							    [{<<"counter">>,X},
							     {<<"ask">>,NS#state.ask},
							     {<<"bid">>,NS#state.ask}
							    ]
							   }]|L],NS};
						statistics -> {[
							       [{<<"point">>,
								 [{<<"counter">>,X},
								  {<<"bid">>,NS#state.bid},
								  {<<"ask">>,NS#state.ask},
								  {<<"max">>,NS#state.max},
								  {<<"min">>,NS#state.min},
								  {<<"average_bp_change">>,NS#state.average_bp_change},
								  {<<"last_bp_change">>,NS#state.last_bp_change},
								  {<<"max_bp_change">>,NS#state.max_bp_change},
								  {<<"min_bp_change">>,NS#state.min_bp_change}
								 ]
								}]
							       |L], NS};
						_ -> {[
						       [{<<"point">>,
							 [{<<"counter">>,X},
							  {<<"bid">>,NS#state.bid}
							 ]
							}]|L],NS}
					    end
				    end,{[],State},P),
    {reply, [{<<"graph">>,[Result]}], NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({subscribe,UpdateFun}, #state{interval=I} = State) ->
    {ok,TRef} = timer:send_interval(I, interval),
    {noreply,State#state{updatefun=UpdateFun, timer= TRef}};
handle_cast(unsubscribe, #state{timer = TRef} = State) ->
    timer:cancel(TRef),
    {noreply, State#state{loop_count = 0}};
handle_cast(stop,State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(interval,#state{updatefun=Update} = State) ->
    NS = calculate(State),
    Reply = [{<<"counter">>,NS#state.loop_count},
	     {<<"bid">>,NS#state.bid},
	     {<<"ask">>,NS#state.ask},
	     {<<"max">>,NS#state.max},
	     {<<"min">>,NS#state.min},
	     {<<"average_bp_change">>,NS#state.average_bp_change},
	     {<<"last_bp_change">>,NS#state.last_bp_change},
	     {<<"max_bp_change">>,NS#state.max_bp_change},
	     {<<"min_bp_change">>,NS#state.min_bp_change},
	     {<<"bp_diff">>,NS#state.bp_diff}],
    Update([{<<"point">>,Reply}]),
    {noreply, NS}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
non_zero_rand() ->
    case random:uniform() of
	0.0 ->
	    non_zero_rand();
	R -> R
    end.

multiplier(P) ->
    case P of
	0 -> 1;
	1 -> 10;
	2 -> 100;
	3 -> 1000;
	4 -> 10000;
	5 -> 100000;
	6 -> 1000000
    end.
	    
calculate(#state{return_mean=RM,daily_vol = DV, 
			    bid=PX, max=Mx, min=Mn, average_bp_change=AV,
			    loop_count=C,max_bp_change=MaxBpChange,
			    min_bp_change=MinBpChange,precision=Precision} = State) ->
    R = non_zero_rand(),
    RandVol = maths:inv_normal_dist(R),
    LogRet = (RM+DV)*RandVol,
    BidPrice = PX*(math:exp(LogRet)),
    NewP = multiplier(Precision),
    Spread = R/NewP,
    Offer = case BidPrice+Spread == BidPrice of
		true -> BidPrice+Spread+1/Precision;
		false -> BidPrice+Spread
	    end,
    Max= case BidPrice > Mx of
	     true -> BidPrice;
	     false -> Mx
	 end,
    Min = case (BidPrice < Mn) or (Mn =:= 0.0) of
	      true -> BidPrice;
	      false -> Mn
	  end,
    BPDiff = (Max-Min)*NewP,
    AveBpChange = case AV of
		      0.0 -> abs(BidPrice-PX)*NewP;
		      _ -> ((AV*C+abs(BidPrice-PX)*NewP))/(C+1)
		  end,			   
    LastBpChange = (BidPrice-PX)*NewP,
    MaxBp = case LastBpChange > MaxBpChange of
		true -> LastBpChange;		     
		false -> MaxBpChange
	    end,
    MinBp = case LastBpChange < MinBpChange of
		true -> LastBpChange;
		false -> MinBpChange
	    end,
    State#state{bid=BidPrice,ask=Offer, max=Max, min=Min, bp_diff=BPDiff,loop_count=C+1,
			   average_bp_change=AveBpChange, last_bp_change=LastBpChange,
			   max_bp_change=MaxBp,min_bp_change=MinBp}.
