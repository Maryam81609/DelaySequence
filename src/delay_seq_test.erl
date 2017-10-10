%%%-------------------------------------------------------------------
%%% @author maryam
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2016 12:33 AM
%%%-------------------------------------------------------------------
-module(delay_seq_test).
-author("maryam").

%% API
-export([generate_next_schedule/1, explore/1]).


generate_next_schedule(first) ->
  io:format("~n Delay Sequence: "),
  delay_sequence:print_sequence(),
  explore(sch),
  generate_next_schedule(delay_sequence:has_next());

generate_next_schedule(true) ->
  delay_sequence:next(),
  io:format("~n Delay Sequence: "),
  delay_sequence:print_sequence(),
  explore(sch),
  generate_next_schedule(delay_sequence:has_next());

generate_next_schedule(false) ->
  io:format("~nEnd.").

explore(sch) ->
  io:format("~n Current event to delay: ~p", [delay_sequence:get_next_delay_index()]),
  explore(delay_sequence:is_end_current_delay_seq());

explore(false) ->
  delay_sequence:spend_current_delay_index(),
  io:format("~n Current event to delay: ~p", [delay_sequence:get_next_delay_index()]),
  explore(delay_sequence:is_end_current_delay_seq());

explore(true) ->
  io:format("~n--------------------------").