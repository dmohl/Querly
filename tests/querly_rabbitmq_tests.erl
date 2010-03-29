-module(querly_rabbitmq_tests).
-author('Dan Mohl').

-compile(export_all).

-include("../include/amqp_client.hrl").

initialize_test_suite() ->
	ok.

finalize_test_suite() ->
    ok.

run_all() ->
	% initialize tests
	initialize_test_suite(),
	% all tests
	test_rabbitmq_subscription(),
	test_get_queue_length(),
	% cleanup
	finalize_test_suite().
	
test_rabbitmq_subscription() ->
	Exchange = <<"test_rabbitmq">>,
	Queue = <<"test_rabbitmq">>,
	Type = <<"topic">>,
	querly_rabbitmq:setup_exchange(Queue, Exchange, Type),
	PublisherPid = querly_rabbitmq:start_publisher(),
	SubscriberPid = querly_rabbitmq:start_subscriber(),
	PublisherPid ! {publish, Exchange, <<"test">>},
	SubscriberPid ! {self(), get_all_messages, Queue},
	receive
		{messages_received, Content} ->
			Result = Content;
		_Received -> 
			Result = ""
	end,
	test_helper:display_message({"querly_rabbitmq_tests/test_rabbitmq_subscription", length(Result) > 0, length(Result)}).
	
test_get_queue_length() ->
	Exchange = <<"test_rabbitmq">>,
	Queue = <<"test_rabbitmq">>,
	Type = <<"topic">>,
	querly_rabbitmq:setup_exchange(Queue, Exchange, Type),
	PublisherPid = querly_rabbitmq:start_publisher(),
	SubscriberPid = querly_rabbitmq:start_subscriber(),
	PublisherPid ! {publish, Exchange, <<"test">>},
	SubscriberPid ! {self(), get_queue_length, Queue},
	receive
		{queue_length, Length} ->
			Result = Length;
		_Received -> 
			io:format("~p~n~n", [_Received]),
			Result = 0
	end,
	SubscriberPid ! {self(), get_message, Queue},
	receive
		_ -> ok
	end,
	test_helper:display_message({"querly_rabbitmq_tests/test_rabbitmq_subscription", Result == 0, Result}).

