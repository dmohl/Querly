-module(querly_helper_tests).
-author('Dan Mohl').

-export([run_all/0, test_is_valid_record_true/0, test_is_valid_record_false/0]).
		 
-include_lib("../src/record_definitions.hrl").

initialize_test_suite() ->
	done.

finalize_test_suite() ->
	done.

run_all() ->
	% initialize tests
	initialize_test_suite(),
	% all tests
	test_is_valid_record_true(),
	test_is_valid_record_false(),
	% finalize 
	finalize_test_suite().		
	
test_is_valid_record_true() ->
    Result = querly_helper:is_valid_record("person"),
    test_helper:display_message({"querly_client_tests/test_is_valid_record_true", Result == true, Result}).
		
test_is_valid_record_false() ->
    Result = querly_helper:is_valid_record("person2"),
    test_helper:display_message({"querly_client_tests/test_is_valid_record_false", Result == false, Result}).
