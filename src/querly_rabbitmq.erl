-module(querly_rabbitmq).

% based on blog from http://jkvor.com/rabbitmq-erlang-client

-export([setup_exchange/3, setup_exchange/4, start_publisher/0, start_subscriber/0, loop_publisher/3, loop_subscriber/3]).
 
-include("../include/amqp_client.hrl").
 
stop(Channel, Connection) -> 
	amqp_channel:close(Channel),
    amqp_connection:close(Connection),
	ok.

% sets up the queue and topic exchange, and ties the exchange to the queue
setup_exchange(Queue, Exchange, Type) ->
	Connection = amqp_connection:start_network(),
	setup_exchange(Connection, Queue, Exchange, Type).
setup_exchange(Connection, Queue, Exchange, Type) ->
	Channel = amqp_connection:open_channel(Connection),
    amqp_channel:call(Channel, #'exchange.declare'{exchange = Exchange, type = Type}),
    amqp_channel:call(Channel, #'queue.declare'{queue = Queue}),
    Route = #'queue.bind'{queue = Queue, exchange = Exchange},
    amqp_channel:call(Channel, Route),
	stop(Channel, Connection),
    ok.
  
%% -------------------------------------
%% PUBLISHER
%% -------------------------------------
start_publisher() ->
	Connection = amqp_connection:start_network(),
	start_publisher(Connection).
start_publisher(Connection) ->
	Channel = amqp_connection:open_channel(Connection),
    spawn_link(?MODULE, loop_publisher, [self(), Channel, Connection]).
  
loop_publisher(Parent, Channel, Connection) ->
    receive 
		{publish, Exchange, Payload} ->
			Publish = #'basic.publish'{exchange = Exchange},
			ok = publish_message(Channel, Publish, Payload),			
			loop_publisher(Parent, Channel, Connection),
			ok;
		{publish, Exchange, Payload, RoutingKey} ->
			Publish = #'basic.publish'{exchange = Exchange, routing_key = RoutingKey},
			ok = publish_message(Channel, Publish, Payload),			
			loop_publisher(Parent, Channel, Connection),
			ok;
		{stop_publisher_and_subscriber} ->
			stop(Channel, Connection)
	end. 

publish_message(Channel, Publish, Payload) ->
    amqp_channel:call(Channel, Publish, #amqp_msg{payload = Payload}).
	
%% -------------------------------------
%% SUBSCRIBER
%% -------------------------------------
start_subscriber() ->
	start_subscriber(amqp_connection:start_network()).
start_subscriber(Connection) ->
    Channel = amqp_connection:open_channel(Connection),
	spawn_link(?MODULE, loop_subscriber, [self(), Channel, Connection]).
 
loop_subscriber(Parent, Channel, Connection) ->
    receive 
		{From, get_message, Queue} ->
			case recieve_message(Channel, Queue) of
				{#'basic.get_ok'{}, #amqp_msg{payload = Payload}} -> 
					From ! {message_received, Payload};
				#'basic.get_empty'{} -> 
					ok;
				Other -> 
					From ! {error, Other}
			end,
			loop_subscriber(Parent, Channel, Connection);
		{From, get_queue_length, Queue} ->
			Length = get_queue_count(Channel, Queue),
			From ! {queue_length, Length},
			loop_subscriber(Parent, Channel, Connection);	
		{From, get_all_messages, Queue} ->			
			MessageList = get_all_messages(Channel, Queue, []),
			From ! {messages_received, MessageList},
			loop_subscriber(Parent, Channel, Connection);	
		{stop_publisher_and_subscriber} ->
			stop(Channel, Connection)
	end. 

get_all_messages(Channel, Queue, MessageList) ->
	case (get_queue_count(Channel, Queue)) of
		0 ->
			MessageList;
		_ ->
			NewMessageList = lists:merge([recieve_message(Channel, Queue)], MessageList),
			get_all_messages(Channel, Queue, NewMessageList)
	end.
	
recieve_message(Channel, Queue) ->
	amqp_channel:call(Channel, #'basic.get'{queue = Queue, no_ack = false}).

get_queue_count(Channel, Queue) ->
	{'queue.declare_ok', _, Length, _} = amqp_channel:call(Channel, #'queue.declare'{queue = Queue}),
	Length.