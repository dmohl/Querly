-module(querly_load_tests).
-author('Dan Mohl').

-export([run_all/0, load_10000_people/0, load_10000_employers/0]).
		 
-include_lib("../src/record_definitions.hrl").

get_test_db_name_format() ->
	"test_~s_db_load".

initialize_test_suite() ->
    %querly:start(get_test_db_name_format()).
	querly:start().

run_all() ->
	% initialize tests
	initialize_test_suite(),
	% all tests
	load_10000_people(),
	load_10000_employers().

load_10000_people() ->	
	Iterations = 9999, 
	create_person(1, Iterations).

load_10000_employers() ->	
	Iterations = 9999, 
	create_employer(1, Iterations).

create_person(Index, Iterations) ->
	FirstName = binary_to_list(list_to_binary(io_lib:format("TestFirst~p", [Index]))),
	LastName = binary_to_list(list_to_binary(io_lib:format("TestLast~p", [Index]))),
	PersonRecord = #person{idno=Index, firstName=FirstName, lastName=LastName, dob="08/28/1977", ssn="123-45-9876"},			
	RecordFieldNames = record_info(fields, person),
	querly_db:doc_create(io_lib:format("~p", [Index]), PersonRecord, RecordFieldNames),
	case (Index =< Iterations) of
		true -> 
			create_person(Index+1, Iterations),
			io:format("Created ~p of ~p People~n", [(Iterations - Index) + 2, Iterations + 1]);
		_ -> done
	end.

create_employer(Index, Iterations) ->
	EmployerName = binary_to_list(list_to_binary(io_lib:format("TestEmployer~p", [Index]))), 
	Address = binary_to_list(list_to_binary(io_lib:format("~p Main Street", [Index]))), 
	EmployerRecord = #employer{id=Index, name=EmployerName, address=Address},			
	RecordFieldNames = record_info(fields, employer),
	querly_db:doc_create(io_lib:format("~p", [Index]), EmployerRecord, RecordFieldNames),
	case (Index =< Iterations) of
		true -> 
			create_employer(Index+1, Iterations),
			io:format("Created ~p of ~p Employers~n", [(Iterations - Index) + 2, Iterations + 1]);
		_ -> done
	end.
