-module(querly).
-author('Dan Mohl').

-export([start/0, start/1, select/1]).

-include_lib("record_definitions.hrl").

start() ->
	querly_db:start(),
	case whereis(querly_db) of
		undefined -> 
			register(querly_db, spawn(querly_db, get_person_table, [undefined]));
		_ -> 
			querly_db_already_running
	end.		

start(TestPersonTable) ->	
	querly_db:start(),
	register(querly_db, spawn(querly_db, get_person_table, [TestPersonTable])).

select(SearchCriteria) ->
	DefaultRecord = #person{},
	RecordFieldNames = record_info(fields, person),
	querly_db ! {self(), get_person_table, #person.idno, DefaultRecord, RecordFieldNames},
	receive
		{people_table, People} ->
			ets:select(People, [{SearchCriteria, [], ['$_']}])
	end.

