-module(querly_load_tests).
-author('Dan Mohl').

-export([run_all/0, load_10000_people/0]).
		 
-include_lib("../src/record_definitions.hrl").

get_test_db_name_format() ->
	"test_~s_db_load".

initialize_test_suite() ->
    querly:start(get_test_db_name_format()).

run_all() ->
	% initialize tests
	initialize_test_suite(),
	% all tests
	load_10000_people().

load_10000_people() ->	
	Iterations = 10000, 
	create_person(1, Iterations).

create_person(Index, Iterations) ->
	FirstName = io_lib:format("~p~p", ["TestFirst", Index]),
	LastName = io_lib:format("~p~p", ["TestLast", Index]),
	PersonRecord = #person{idno=Index, firstName=FirstName, lastName=LastName, dob="08/28/1977", ssn="123-45-9876"},			
	RecordFieldNames = record_info(fields, person),
	querly:doc_create(io_lib:format("~p", [Index]), PersonRecord, RecordFieldNames),
	case (Index =< Iterations) of
		true -> 
			create_person(Index+1, Iterations),
			io:format("Created ~p of ~p~n", [Index, Iterations]);
		_ -> done
	end.

