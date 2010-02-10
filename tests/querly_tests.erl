-module(querly_tests).
-author('Dan Mohl').

-export([run_all/0, test_select_all/0, test_select_all_jimmy/0, test_select_all_smith/0, 
        test_select_jimmy_smith/0, test_select_person_with_id_99999996/0, test_reset_specified_ets_table/0]).
		 
-include_lib("../src/record_definitions.hrl").

initialize_test_suite() ->
	NewPeopleTable = ets:new(people, [{keypos, #person.'Idno'}]),
	ets:insert(NewPeopleTable, 
	   [#person{'Idno'=99999996, 'FirstName'="Dan", 'LastName'="Mohl", 'Dob'="08/28/1977", 'Ssn'="123-45-9876"},
		#person{'Idno'=99999997, 'FirstName'="Jimmy", 'LastName'="John", 'Dob'="08/28/1967", 'Ssn'="123-45-5555"},
		#person{'Idno'=99999998, 'FirstName'="Jimmy", 'LastName'="Smith", 'Dob'="08/28/1957", 'Ssn'="123-45-4444"},
		#person{'Idno'=99999999, 'FirstName'="Sally", 'LastName'="Smith", 'Dob'="08/28/1947", 'Ssn'="123-45-3333"}]),	
	NewEmployerTable = ets:new(employer, [{keypos, #employer.'Id'}]),
	ets:insert(NewEmployerTable, 
	   [#employer{'Id'=999996, 'Name'="ABC Corp.", 'Address'="789 Main"},
		#employer{'Id'=999997, 'Name'="123 Inc.", 'Address'="456 Main"},
		#employer{'Id'=999998, 'Name'="XYZ Corp.", 'Address'="123 Main"}]),	
    querly:start("~s", [{"person", NewPeopleTable}, {"employer", NewEmployerTable}]).

initialize_test_suite_with_no_persons() ->
	NewPeopleTable = ets:new(people, [{keypos, #person.'Idno'}]),
	ets:insert(NewPeopleTable, []),	
	NewEmployerTable = ets:new(employer, [{keypos, #employer.'Id'}]),
	ets:insert(NewEmployerTable, 
	   [#employer{'Id'=999996, 'Name'="ABC Corp.", 'Address'="789 Main"},
		#employer{'Id'=999997, 'Name'="123 Inc.", 'Address'="456 Main"},
		#employer{'Id'=999998, 'Name'="XYZ Corp.", 'Address'="123 Main"}]),	
    querly:start("~s", [{"person", NewPeopleTable}, {"employer", NewEmployerTable}]).

finalize_test_suite() ->
	querly:stop().

run_all() ->
	% initialize tests
	initialize_test_suite(),
	% all tests
	test_select_all(),
	test_select_all_jimmy(),
	test_select_all_smith(),
	test_select_jimmy_smith(),
	test_select_person_with_id_99999996(),
	test_reset_specified_ets_table(),
	% cleanup
	finalize_test_suite().
	
test_select_all() ->
	DefaultRecord = #person{},
	ResultSet = querly:select(#person{_ = '_'}, DefaultRecord, ?personFields, #person.'Idno'),
	Result = erlang:length(ResultSet),
	test_helper:display_message({"querly_tests/test_select_all", Result == 4, Result}).
	
test_select_all_jimmy() ->
	DefaultRecord = #person{},
	ResultSet = querly:select(#person{'FirstName'="Jimmy",  _ = '_'}, DefaultRecord, ?personFields, #person.'Idno'),
	Result = erlang:length(ResultSet),
    test_helper:display_message({"querly_tests/test_select_all_jimmy", Result == 2, Result}).

test_select_all_smith() ->
	DefaultRecord = #person{},
	ResultSet = querly:select(#person{'LastName'="Smith",  _ = '_'}, DefaultRecord, ?personFields, #person.'Idno'),
	Result = erlang:length(ResultSet),
    test_helper:display_message({"querly_tests/test_select_all_smith", Result == 2, Result}).

test_select_jimmy_smith() ->
	DefaultRecord = #person{},
	ResultSet = querly:select(#person{'FirstName'="Jimmy", 'LastName'="Smith",  _ = '_'}, DefaultRecord, ?personFields, #person.'Idno'),
	Result = erlang:length(ResultSet),
    test_helper:display_message({"querly_tests/test_select_jimmy_smith", Result == 1, Result}).

test_select_person_with_id_99999996() ->
	DefaultRecord = #person{},
	ResultSet = querly:select(#person{'Idno'=99999996,  _ = '_'}, DefaultRecord, ?personFields, #person.'Idno'),
	Result = erlang:length(ResultSet),
    test_helper:display_message({"querly_tests/test_select_person_with_id_99999996", Result == 1, Result}).

test_reset_specified_ets_table() ->
	DefaultRecord = #person{},
	querly:reset_ets_table("person"),
	querly:stop(),
	initialize_test_suite_with_no_persons(),
	ResultSet = querly:select(#person{'Idno'=99999996,  _ = '_'}, DefaultRecord, ?personFields, #person.'Idno'),
	io:format("here"),
	Result = erlang:length(ResultSet),
    test_helper:display_message({"querly_tests/test_reset_specified_ets_table", Result == 0, Result}),
	querly:stop(),
	initialize_test_suite().
