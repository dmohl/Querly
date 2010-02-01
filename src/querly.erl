-module(querly).
-author('Dan Mohl').

-export([start/0, start/1, stop/0, select/4]).

-include_lib("record_definitions.hrl").

start() ->
    start([]).
start(TableList) ->	
	case whereis(querly_db) of
		undefined ->
			querly_db:start(),
			register(querly_db, spawn(querly_db, tables_service, [TableList]));
		_ -> 
			querly_db_already_running
	end.
	
stop() ->
    unregister(querly_db).	

select(SearchCriteria, DefaultRecord, RecordFieldNames, PrimaryKeyPosition) ->
	querly_db ! {self(), get_table, PrimaryKeyPosition, DefaultRecord, RecordFieldNames},
	receive
		{table_results, Table} ->
			ets:select(Table, [{SearchCriteria, [], ['$_']}])
	end.

