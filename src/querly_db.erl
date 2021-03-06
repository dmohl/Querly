-module(querly_db).
-author('Dan Mohl').

-export([start/0, build_json/2, build_record/3, doc_create/3, doc_create/4, doc_get_all/1, doc_get/2,
		tables_service/1, get_database_names/0, db_exists/1, get_table_by_name/2, handle_ets_update/0,
		chop_nonprintable_characters/1]).

-include_lib("record_definitions.hrl").

start() ->
	inets:start(),
	application:start(ecouch),
	start_ets_updater().
	
start_ets_updater() ->
	case whereis(ets_updater) of
		undefined ->
			register(ets_updater, spawn_link(fun() -> handle_ets_update() end));
		EtsUpdaterPid ->
			EtsUpdaterPid
	end.		

build_json(RecordToTransform, RecordFieldNames) ->
	Record = element(1, RecordToTransform),
    rfc4627:from_record(RecordToTransform, Record, RecordFieldNames).

build_record(Json, DefaultRecord, RecordFieldNames) ->
    rfc4627:to_record(Json, DefaultRecord, RecordFieldNames).

tables_service(DatabaseInformationTuple) ->
	receive
		{From, get_table, DatabaseName, PrimaryKeyPosition} ->
			process_get_table_request(From, DatabaseName, DatabaseInformationTuple, PrimaryKeyPosition);
		{From, reset_table, TableName} ->
			remove_table_from_table_list(From, DatabaseInformationTuple, TableName)
	end.

remove_table_from_table_list(From, DatabaseInformationTuple, TableName) ->
	TableList = element(2, DatabaseInformationTuple),
	DatabaseNamingFormat = element(1, DatabaseInformationTuple),
	FormattedDatabaseName = binary_to_list(list_to_binary(io_lib:format(DatabaseNamingFormat, [TableName]))),
	case get_table_element_from_list(FormattedDatabaseName, TableList) of
		[] ->
			From ! {table_reset_results, TableList},
			tables_service({DatabaseNamingFormat, TableList});
		TableElement -> 
			NewTableList = lists:delete(lists:nth(1, TableElement), TableList),
			From ! {table_reset_results, NewTableList},
			tables_service({DatabaseNamingFormat, NewTableList})
	end.

process_get_table_request(From, TableName, DatabaseInformationTuple, PrimaryKeyPosition) ->
	TableList = element(2, DatabaseInformationTuple),
	DatabaseNamingFormat = element(1, DatabaseInformationTuple),
	FormattedDatabaseName = binary_to_list(list_to_binary(io_lib:format(DatabaseNamingFormat, [TableName]))),
	case get_table_by_name(FormattedDatabaseName, TableList) of
		undefined -> 
			Table = ets:new(table, [public, {keypos, PrimaryKeyPosition}]),
			build_table_from_couch(FormattedDatabaseName, Table, TableName),
			NewTableList = lists:append([{FormattedDatabaseName, Table}], TableList),
			From ! {table_results, Table},
			tables_service({DatabaseNamingFormat, NewTableList});
		Table -> 
			From ! {table_results, Table},
			tables_service({DatabaseNamingFormat, TableList})
	end.

build_table_from_couch(DatabaseName, Table, TableName) ->
	db_create_if_needed(DatabaseName),
	Docs = doc_get_all(DatabaseName),
	case element(2, element(2, Docs)) of
		[] ->
			Table;
		_ ->
			Rows = lists:nth(3, element(2, element(2, Docs))),
			ResultList = element(2, Rows),	
			lists:foreach(
				fun(RowJson) -> 
					DocumentJson = element(2, lists:nth(4, element(2, RowJson))), 
					add_json_to_ets_table(DocumentJson, Table, TableName) 
				end, ResultList)
	end,			
	Table.

add_json_to_ets_table(Json, TableName) ->
	Table = case whereis(querly_db) of
		undefined -> 
			io:format("Something went wrong in add_record_to_ets_table ~n"),
			ets:new(table, [{keypos, ?PRIMARY_KEY_INDEX}]);
		Pid ->
			Pid ! {self(), get_table, TableName, ?PRIMARY_KEY_INDEX},
			receive
				{table_results, TableResult} -> 
					TableResult;
				_ -> 
					io:format("Something went wrong in add_record_to_ets_table ~n"),
					ets:new(table, [{keypos, ?PRIMARY_KEY_INDEX}])	
			end
	end,		
	add_json_to_ets_table(Json, Table, TableName).
add_json_to_ets_table(Json, Table, TableName) ->
	RecordFieldNames = querly_helper:get_record_fields(TableName),
	DefaultRecord = querly_helper:transform_to_record([], TableName, undefined, RecordFieldNames),
	Record = build_record(Json, DefaultRecord, RecordFieldNames),
	%?INFO("Here: ~p; ~p; ~p; ~p; ~p~n", [Json, DefaultRecord, RecordFieldNames, Record, Table]),
	ets:insert(Table, Record).
				
update_ets_table(DatabaseName, PrimaryKey, Action) ->
	JsonDoc = doc_get(DatabaseName, PrimaryKey),
	case Action of
		_ when Action == "add"; Action == "update" ->
			Json = element(2, JsonDoc), 
			add_json_to_ets_table(Json, DatabaseName);
		_ ->
			ok
	end.

handle_ets_update() ->
	SubscriberPid = get_rabbitmq_subscriber_pid(?RABBITMQ_EXCHANGE, ?RABBITMQ_QUEUE, ?RABBITMQ_TYPE),
	SubscriberPid ! {self(), get_all_messages, ?RABBITMQ_QUEUE},
	receive
		{messages_received, MessageList} ->
			process_messages(MessageList)
	end,
	timer:sleep(?RABBITMQ_SUBSCRIPTION_TIMER),
	handle_ets_update().

process_messages(MessageList) ->
	lists:foreach(
		fun(Message) -> 
			{_QueueInformation, MessageContent} = Message,
			{amqp_msg, _PayloadInformation, Payload} = MessageContent,
			PayloadToChop = re:replace(bitstring_to_list(Payload), "[\\\"\\\\\"]", "", [{return,list}, global]),
			ChoppedMessagePayload = chop_nonprintable_characters(PayloadToChop),
			ContentStartIndex = string:str(ChoppedMessagePayload, "{Body:{") + string:len("{Body:{"),
			ContentEndIndex = string:len(ChoppedMessagePayload) - 1,
			Content = string:sub_string(ChoppedMessagePayload, ContentStartIndex, ContentEndIndex),
			Tokens = string:tokens(Content, ","),
			DatabaseName = find_token_value(Tokens, ":", "DatabaseName"),
			PrimaryKey = find_token_value(Tokens, ":", "PrimaryKey"),
			Action = find_token_value(Tokens, ":", "Action"),
			update_ets_table(DatabaseName, PrimaryKey, Action)
		end, MessageList).	

find_token_value(Tokens, Delimiter, KeyToFind) ->
	FilteredList = lists:filter(
		fun(Token) -> 
			Key = lists:nth(1, get_tokens_from_string(Token, Delimiter)),
			Key == KeyToFind		
		end, Tokens),
	case FilteredList of
		[] -> 
			"";
		TokenList ->
			StringOfTokens = lists:nth(1, TokenList),
			Tokenized = get_tokens_from_string(StringOfTokens, Delimiter),
			case length(Tokenized) of
				2 -> 
					lists:nth(2, Tokenized);
				_ ->
					""
			end
	end.		

get_tokens_from_string(StringToTokenize, Delimiter) ->
	string:tokens(StringToTokenize, Delimiter).

get_rabbitmq_subscriber_pid(Queue, Exchange, Type) ->
	case whereis(rabbitmq_subscriber) of
		undefined ->
			querly_rabbitmq:setup_exchange(Queue, Exchange, Type),
			SubscriberPid = querly_rabbitmq:start_subscriber(),
			register(rabbitmq_subscriber, SubscriberPid),
			SubscriberPid;
		SubscriberPid ->
			SubscriberPid
	end.		

chop_nonprintable_characters(CharacterList) ->
    lists:filter(fun(Char) -> is_printable_character(Char) == true end, CharacterList).		

is_printable_character(Character) ->
	case Character of
		%_ when Character >= 8, Character =< 14 -> 
		%	true;
		_ when Character >= 32, Character =< 127 -> 
			true;
		_ -> 
			false
	end.    

db_create_if_needed(DatabaseName) ->
    case db_exists(DatabaseName) of
		false -> ecouch:db_create(DatabaseName);
		_ -> ok
	end.	

db_exists(DatabaseName) ->
    lists:any(fun(DbName) -> bitstring_to_list(DbName) == DatabaseName end, get_database_names()).

doc_create(DocName, RecordToTransform, RecordFieldNames) ->
	DatabaseName = atom_to_list(element(1, RecordToTransform)),
	doc_create(DatabaseName, DocName, RecordToTransform, RecordFieldNames).
doc_create(DatabaseName, DocName, RecordToTransform, RecordFieldNames) ->
	Json = querly_db:build_json(RecordToTransform, RecordFieldNames),
	db_create_if_needed(DatabaseName),
    ecouch:doc_create(DatabaseName, DocName, Json).	
    
doc_get_all(DatabaseName) ->    
	ecouch:doc_get_all(DatabaseName, [{include_docs,true}]).
	
doc_get(DatabaseName, DocName) ->    
	ecouch:doc_get(DatabaseName, DocName).	
	
get_database_names() ->
	element(2, ecouch:db_list()).

get_table_element_from_list(Name, TableList) ->
    lists:filter(fun(TableTuple) -> element(1, TableTuple) == Name end, TableList).
	
get_table_by_name(Name, TableList) ->
    TableResult = get_table_element_from_list(Name, TableList),
	case TableResult of
	    [] -> undefined;
		_ -> element(2, lists:nth(1, TableResult))
	end.	
