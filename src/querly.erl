-module(querly).
-author('Dan Mohl').

-export([start/0, start/1, select/4]).

-include_lib("record_definitions.hrl").

start() ->
	querly_db:start(),
	case whereis(querly_db) of
		undefined -> 
			register(querly_db, spawn(querly_db, tables_service, [[]]));
		_ -> 
			querly_db_already_running
	end.		

start(TestTableList) ->	
	querly_db:start(),
	register(querly_db, spawn(querly_db, tables_service, [TestTableList])).

select(SearchCriteria, DefaultRecord, RecordFieldNames, PrimaryKeyPosition) ->
	querly_db ! {self(), get_table, PrimaryKeyPosition, DefaultRecord, RecordFieldNames},
	receive
		{table_results, Table} ->
			ets:select(Table, [{SearchCriteria, [], ['$_']}])
	end.

