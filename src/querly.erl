-module(querly).
-author('Dan Mohl').

-export([start/0, start/1, start/2, stop/0, select/4, reset_ets_table/1]).

-include_lib("record_definitions.hrl").

start() ->
    start("~s", []).
start(DatabaseNamingFormat)	->
    start(DatabaseNamingFormat, []).
start(DatabaseNamingFormat, TableList) ->	
	case whereis(querly_db) of
		undefined ->
			querly_db:start(),
			register(querly_db, spawn(querly_db, tables_service, [{DatabaseNamingFormat, TableList}]));
		_ -> 
			querly_db_already_running
	end.

stop() ->
    unregister(querly_db).	

reset_ets_table(TableName) ->
	querly_db ! {self(), reset_table, TableName},
	receive
		{table_reset_results, _TableList} ->
			%io:format("~p~n", [_TableList]),
			ok
	end.
	
select(SearchCriteria, DefaultRecord, RecordFieldNames, PrimaryKeyPosition) ->
	querly_db ! {self(), get_table, PrimaryKeyPosition, DefaultRecord, RecordFieldNames},
	receive
		{table_results, Table} ->
			ets:select(Table, [{SearchCriteria, [], ['$_']}])
	end.

