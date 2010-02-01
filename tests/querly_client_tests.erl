-module(querly_client_tests).
-author('Dan Mohl').

-export([run_all/0, test_get_record_metadata/0, test_parse_where_clause/0]).
		 
-include_lib("../src/record_definitions.hrl").

initialize_test_suite() ->
	NewPeopleTable = ets:new(people, [{keypos, #person.idno}]),
	ets:insert(NewPeopleTable, 
	   [#person{idno=99999996, firstName="Dan", lastName="Mohl", dob="08/28/1977", ssn="123-45-9876"},
		#person{idno=99999997, firstName="Jimmy", lastName="John", dob="08/28/1967", ssn="123-45-5555"},
		#person{idno=99999998, firstName="Jimmy", lastName="Smith", dob="08/28/1957", ssn="123-45-4444"},
		#person{idno=99999999, firstName="Sally", lastName="Smith", dob="08/28/1947", ssn="123-45-3333"}]),	
	NewEmployerTable = ets:new(employer, [{keypos, #employer.id}]),
	ets:insert(NewEmployerTable, 
	   [#employer{id=999996, name="ABC Corp.", address="789 Main"},
		#employer{id=999997, name="123 Inc.", address="456 Main"},
		#employer{id=999998, name="XYZ Corp.", address="123 Main"}]),	
    querly:start([{"person", NewPeopleTable}, {"employer", NewEmployerTable}]).

finalize_test_suite() ->
	querly:stop().

run_all() ->
	% initialize tests
	initialize_test_suite(),
	% all tests
	test_get_record_metadata(),
	test_parse_where_clause(),
	% finalize 
	finalize_test_suite().		
	

test_get_record_metadata() ->
	Result = erlang:length(?recordMetadata),
    test_helper:display_message({"querly_client_tests/test_get_list_of_tables_and_fields", Result == 2, Result}).
	
test_parse_where_clause() ->
    %querly_client:parse_where_
	%DefaultRecord = #person{},
	%RecordFieldNames = record_info(fields, person),
	%ResultSet = querly:select(#person{_ = '_'}, DefaultRecord, RecordFieldNames, #person.idno),
	%Result = erlang:length(ResultSet),
	Result = 0,
	test_helper:display_message({"querly_client_tests/test_parse_where_clause", Result == 1, Result}).
