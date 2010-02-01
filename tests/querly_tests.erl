-module(querly_tests).
-author('Dan Mohl').

-export([run_all/0, test_select_all/0, test_select_all_jimmy/0, test_select_all_smith/0, 
        test_select_jimmy_smith/0]).
		 
-include_lib("../src/record_definitions.hrl").

get_test_db_name() ->
	"test_person_db".
	
delete_test_db() ->
    ecouch:db_delete(get_test_db_name()).

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
    querly:start([{"person", NewPeopleTable}, {"employer", NewEmployerTable}]),
	delete_test_db(),
	ecouch:db_create(get_test_db_name()).

finalize_test_suite() ->
	querly:stop(),
    delete_test_db().

run_all() ->
	% initialize tests
	initialize_test_suite(),
	% all tests
	test_select_all(),
	test_select_all_jimmy(),
	test_select_all_smith(),
	test_select_jimmy_smith(),
	% cleanup
	finalize_test_suite().
	
test_select_all() ->
	DefaultRecord = #person{},
	ResultSet = querly:select(#person{_ = '_'}, DefaultRecord, ?personFields, #person.idno),
	Result = erlang:length(ResultSet),
	test_helper:display_message({"querly_tests/test_select_all", Result == 4, Result}).
	
test_select_all_jimmy() ->
	DefaultRecord = #person{},
	ResultSet = querly:select(#person{firstName="Jimmy",  _ = '_'}, DefaultRecord, ?personFields, #person.idno),
	Result = erlang:length(ResultSet),
    test_helper:display_message({"querly_tests/test_select_all_jimmy", Result == 2, Result}).

test_select_all_smith() ->
	DefaultRecord = #person{},
	ResultSet = querly:select(#person{lastName="Smith",  _ = '_'}, DefaultRecord, ?personFields, #person.idno),
	Result = erlang:length(ResultSet),
    test_helper:display_message({"querly_tests/test_select_all_smith", Result == 2, Result}).

test_select_jimmy_smith() ->
	DefaultRecord = #person{},
	ResultSet = querly:select(#person{firstName="Jimmy", lastName="Smith",  _ = '_'}, DefaultRecord, ?personFields, #person.idno),
	Result = erlang:length(ResultSet),
    test_helper:display_message({"querly_tests/test_select_jimmy_smith", Result == 1, Result}).
