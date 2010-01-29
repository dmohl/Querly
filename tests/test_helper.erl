-module(test_helper).
-author('Dan Mohl').

-export([display_message/1]).

display_message(Message) ->
    case Message of
		{FunctionName, true, _Result} ->
			io:format("~p - ~p~n", [FunctionName, passed]);
		{FunctionName, true, _Result, Message} ->	
			io:format("~p - ~p; Information: ~p~n", [FunctionName, passed, Message]);
		{FunctionName, false, Result} ->		
			io:format("~p - ~p; Actual Result = ~p~n~n", [FunctionName, failed, Result]);
		{FunctionName, false, Result, Message} -> 
			io:format("~p - ~p; Information: ~p; Actual Result = ~p~n~n", [FunctionName, failed, Message, Result]);
		_ -> 
			io:format("~p~n~n", Message)	
	end.
