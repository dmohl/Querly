-module(querly_client_tests).
-author('Dan Mohl').

-export([run_all/0, test_get_record_metadata/0, test_select/0, test_is_valid_record_true/0, test_is_valid_record_false/0,
		 test_parse_select_all/0, test_parse_select_partial/0, test_parse_from/0,
		 test_parse_with_with_where_clause/0, test_parse_with_with_no_where_clause/0,
		 test_parse_from_with_no_where/0, test_sql_query_with_sql/0, test_sql_query_employer_with_sql/0,
		 test_sql_query_invalid_employer_with_sql/0]).
		 
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
	test_select(),
	test_is_valid_record_true(),
	test_is_valid_record_false(),
	test_parse_select_all(),
	test_parse_select_partial(),
	test_parse_from(),
	test_parse_with_with_where_clause(),
	test_parse_with_with_no_where_clause(),
	test_parse_from_with_no_where(),
	test_sql_query_with_sql(),
	test_sql_query_employer_with_sql(),
	test_sql_query_invalid_employer_with_sql(),
	% finalize 
	finalize_test_suite().		
	

test_get_record_metadata() ->
	Result = erlang:length(?recordMetadata),
    test_helper:display_message({"querly_client_tests/test_get_list_of_tables_and_fields", Result == 2, Result}).
	
test_is_valid_record_true() ->
    Result = querly_client:is_valid_record("person"),
    test_helper:display_message({"querly_client_tests/test_is_valid_record_true", Result == true, Result}).
		
test_is_valid_record_false() ->
    Result = querly_client:is_valid_record("person2"),
    test_helper:display_message({"querly_client_tests/test_is_valid_record_false", Result == false, Result}).
	
test_select() ->
	Result = querly_client:select("person", [{"firstName", "Dan"}, {"lastName", "Mohl"}]),
	test_helper:display_message({"querly_client_tests/test_select", erlang:length(Result) == 1, erlang:length(Result)}).
	
test_sql_query_with_sql() ->
	Result = querly_client:sql_query("select * from person where firstName=Dan and lastName = Mohl"),
	test_helper:display_message({"querly_client_tests/test_select_with_sql", erlang:length(Result) == 1, erlang:length(Result)}).

test_sql_query_employer_with_sql() ->
	Result = querly_client:sql_query("select * from employer where name=ABC Corp."),
	test_helper:display_message({"querly_client_tests/test_select_employer_with_sql", erlang:length(Result) == 1, erlang:length(Result)}).

test_sql_query_invalid_employer_with_sql() ->
	Result = querly_client:sql_query("select * from employer where name=ABC C orp."),
	test_helper:display_message({"querly_client_tests/test_select_invalid_employer_with_sql", erlang:length(Result) == 0, erlang:length(Result)}).

test_parse_select_all() ->
	ResultList = querly_client:parse_sql("select * from person where firstName = \"Jimmy\""),
	Result = element(2, lists:nth(1, ResultList)),
	test_helper:display_message({"querly_client_tests/test_parse_select_all", Result == "*", Result}).

test_parse_select_partial() ->
	ResultList = querly_client:parse_sql("select firstName, lastName from person where firstName = \"Jimmy\""),
	Result = element(2, lists:nth(1, ResultList)),
	test_helper:display_message({"querly_client_tests/test_parse_select_partial", Result == "firstName, lastName", 
	    Result}).

test_parse_from() ->
	ResultList = querly_client:parse_sql("select * from person where firstName = \"Jimmy\""),
	Result = element(2, lists:nth(2, ResultList)),
	test_helper:display_message({"querly_client_tests/test_parse_from", Result == "person", Result}).

test_parse_from_with_no_where() ->
	ResultList = querly_client:parse_sql("select * from person"),
	Result = element(2, lists:nth(2, ResultList)),
	test_helper:display_message({"querly_client_tests/test_parse_from_with_no_where", Result == "person", Result}).

test_parse_with_with_where_clause() ->
	ResultList = querly_client:parse_sql("select * from person where firstName = \"Jimmy\""),
	Result = element(2, lists:nth(3, ResultList)),
	test_helper:display_message({"querly_client_tests/test_parse_with_with_where_clause", 
	    Result == "firstName = \"Jimmy\"", Result}).

test_parse_with_with_no_where_clause() ->
	ResultList = querly_client:parse_sql("select * from person"),
	Result = element(2, lists:nth(3, ResultList)),
	test_helper:display_message({"querly_client_tests/test_parse_with_with_no_where_clause", Result == "", Result}).
