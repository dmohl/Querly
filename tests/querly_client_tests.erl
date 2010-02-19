-module(querly_client_tests).
-author('Dan Mohl').

-export([run_all/0, test_select/0, test_parse_select_all/0, test_parse_select_partial/0, test_parse_from/0,
		 test_parse_with_with_where_clause/0, test_parse_with_with_no_where_clause/0,
		 test_parse_from_with_no_where/0, test_sql_query_with_sql/0, test_sql_query_employer_with_sql/0,
		 test_sql_query_invalid_employer_with_sql/0, test_sql_query_with_sql_an_no_where_clause/0,
		 test_sql_against_bitstring_field/0]).
		 
-include_lib("../src/record_definitions.hrl").

initialize_test_suite() ->
	NewPeopleTable = ets:new(people, [{keypos, #person.'Idno'}]),
	ets:insert(NewPeopleTable, 
	   [#person{'Idno'=1, 'FirstName'="Dan", 'LastName'="Mohl", 'Dob'="08/28/1977", 'Ssn'="123-45-9876"},
		#person{'Idno'=2, 'FirstName'="Jimmy", 'LastName'="John", 'Dob'="08/28/1967", 'Ssn'="123-45-5555"},
		#person{'Idno'=3, 'FirstName'="Jimmy", 'LastName'="Smith", 'Dob'="08/28/1957", 'Ssn'="123-45-4444"},
		#person{'Idno'=4, 'FirstName'="Sally", 'LastName'="Smith", 'Dob'="08/28/1947", 'Ssn'="123-45-3333"},
		#person{'Idno'=5, 'FirstName'=list_to_bitstring("Shella"), 'LastName'=list_to_bitstring("Jackson"), 
			'Dob'=list_to_bitstring("08/28/1947"), 'Ssn'=list_to_bitstring("123-45-3333")}]),	
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
	test_select(),
	test_parse_select_all(),
	test_parse_select_partial(),
	test_parse_from(),
	test_parse_with_with_where_clause(),
	test_parse_with_with_no_where_clause(),
	test_parse_from_with_no_where(),
	test_sql_query_with_sql(),
	test_sql_query_employer_with_sql(),
	test_sql_query_invalid_employer_with_sql(),
	test_sql_query_person_with_sql_by_id(),
	test_sql_query_with_sql_an_no_where_clause(),
	test_sql_against_bitstring_field(),
	% finalize 
	finalize_test_suite().		
	
test_select() ->
	Result = querly_client:select("person", [{"FirstName", "Dan"}, {"LastName", "Mohl"}]),
	test_helper:display_message({"querly_client_tests/test_select", erlang:length(Result) == 1, erlang:length(Result)}).
	
test_sql_query_with_sql() ->
	Result = querly_client:sql_query("select * from person where FirstName=Dan and LastName = Mohl"),
	test_helper:display_message({"querly_client_tests/test_select_with_sql", erlang:length(Result) == 1, erlang:length(Result)}).

test_sql_query_with_sql_an_no_where_clause() ->
	Result = querly_client:sql_query("select * from person"),
	test_helper:display_message({"querly_client_tests/test_select_with_sql", erlang:length(Result) == 5, erlang:length(Result)}).

test_sql_query_employer_with_sql() ->
	Result = querly_client:sql_query("select * from employer where Name=ABC Corp."),
	test_helper:display_message({"querly_client_tests/test_select_employer_with_sql", erlang:length(Result) == 1, Result}).

test_sql_query_invalid_employer_with_sql() ->
	Result = querly_client:sql_query("select * from employer where Name=ABC C orp."),
	test_helper:display_message({"querly_client_tests/test_select_invalid_employer_with_sql", 
	    erlang:length(Result) == 0, erlang:length(Result)}).

test_parse_select_all() ->
	ResultList = querly_client:parse_sql("select * from person where FirstName = \"Jimmy\""),
	Result = element(2, lists:nth(1, ResultList)),
	test_helper:display_message({"querly_client_tests/test_parse_select_all", Result == "*", Result}).

test_parse_select_partial() ->
	ResultList = querly_client:parse_sql("select firstName, lastName from person where FirstName = \"Jimmy\""),
	Result = element(2, lists:nth(1, ResultList)),
	test_helper:display_message({"querly_client_tests/test_parse_select_partial", Result == "firstName, lastName", 
	    Result}).

test_parse_from() ->
	ResultList = querly_client:parse_sql("select * from person where FirstName = \"Jimmy\""),
	Result = element(2, lists:nth(2, ResultList)),
	test_helper:display_message({"querly_client_tests/test_parse_from", Result == "person", Result}).

test_parse_from_with_no_where() ->
	ResultList = querly_client:parse_sql("select * from person"),
	Result = element(2, lists:nth(2, ResultList)),
	test_helper:display_message({"querly_client_tests/test_parse_from_with_no_where", Result == "person", Result}).

test_parse_with_with_where_clause() ->
	ResultList = querly_client:parse_sql("select * from person where FirstName = \"Jimmy\""),
	Result = element(2, lists:nth(3, ResultList)),
	test_helper:display_message({"querly_client_tests/test_parse_with_with_where_clause", 
	    Result == "FirstName = \"Jimmy\"", Result}).

test_parse_with_with_no_where_clause() ->
	ResultList = querly_client:parse_sql("select * from person"),
	Result = element(2, lists:nth(3, ResultList)),
	test_helper:display_message({"querly_client_tests/test_parse_with_with_no_where_clause", Result == "", Result}).

test_sql_query_person_with_sql_by_id() ->
	Result = querly_client:sql_query("select * from person where Idno=1"),
	test_helper:display_message({"querly_client_tests/test_sql_query_person_with_sql_by_id", 
		erlang:length(Result) == 1, Result}).

test_sql_against_bitstring_field() ->
	Result = querly_client:sql_query("select * from person where FirstName = \"Shella\""),
	test_helper:display_message({"querly_client_tests/test_sql_against_bitstring_field", 
		erlang:length(Result) == 1, Result}).
