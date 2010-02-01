-module(querly_db_tests).
-author('Dan Mohl').

-export([run_all/0, test_build_record/0, test_build_json/0, test_create_person_doc/0,
		test_get_all_docs/0, test_should_return_expected_person_record/0, test_should_load_person_records/0, test_get_database_names/0, 
		test_db_should_exist/0, test_get_table_by_name_with_table_found/0, test_get_table_by_name_with_table_not_found/0,
		test_should_load_employer_records/0]).
		 
-include_lib("../src/record_definitions.hrl").

get_test_db_name() ->
	"test_person_db".
	
delete_test_db() ->
    ecouch:db_delete(get_test_db_name()).

initialize_test_suite() ->
    querly_db:start(),	
	delete_test_db(),
	ecouch:db_create(get_test_db_name()).

finalize_test_suite() ->
    delete_test_db().

setup_tests() ->
    [test_build_json,
	 test_build_record,
	 test_create_person_doc,
	 test_get_all_docs,
	 test_should_return_expected_person_record,
	 test_get_database_names,
	 test_db_should_exist,
	 test_get_table_by_name_with_table_found,
	 test_get_table_by_name_with_table_not_found,
	 test_should_load_person_records,
	 test_should_load_employer_records].

run_tests() ->
    Tests = setup_tests(),
	lists:foreach(fun(Test) -> spawn(querly_db_tests, Test, []) end, Tests).

run_all() ->
	% initialize tests
	initialize_test_suite(),
	% all tests
	run_tests(),
	% cleanup
	finalize_test_suite().
	
test_build_json() ->
	Json = {obj, [{"idno", 1}, {"firstName", "Dan"}, {"lastName", "Mohl"}, {"dob", "08/28/1977"}, {"ssn", "123-45-9876"}]},
    RecordToTransform = #person{idno=1, firstName="Dan", lastName="Mohl", dob="08/28/1977", ssn="123-45-9876"},
	RecordFieldNames = record_info(fields, person),
	Result = querly_db:build_json(RecordToTransform, RecordFieldNames),
	test_helper:display_message({"test_build_json", Result == Json, Result}).

test_build_record() ->
	Json = {obj, [{"idno", 1}, {"firstName", "Dan"}, {"lastName", "Mohl"}, {"dob", "08/28/1977"}, {"ssn", "123-45-9876"}]},
    PersonRecord = #person{idno=1, firstName="Dan", lastName="Mohl", dob="08/28/1977", ssn="123-45-9876"},
	DefaultRecord = #person{},
	RecordFieldNames = record_info(fields, person),
	Result = querly_db:build_record(Json, DefaultRecord, RecordFieldNames),
	test_helper:display_message({"test_build_record", Result == PersonRecord, Result}).
	
test_create_person_doc() ->	
	PersonRecord = #person{idno=99999997, firstName="Dan", lastName="Mohl", dob="08/28/1977", ssn="123-45-9876"},
	RecordFieldNames = record_info(fields, person),
	Result = querly_db:doc_create(get_test_db_name(), "2", PersonRecord, RecordFieldNames),
	test_helper:display_message({"test_create_person_doc", element(1, Result) == ok, element(2, Result)}).
	
test_get_all_docs() ->
	PersonRecord = #person{idno=99999998, firstName="Dan2", lastName="Mohl", dob="08/28/1977", ssn="123-45-9876"},
	RecordFieldNames = record_info(fields, person),
	querly_db:doc_create(get_test_db_name(), "3", PersonRecord, RecordFieldNames),
	Result = querly_db:doc_get_all(get_test_db_name()),
	test_helper:display_message({"test_get_all_docs", element(1, Result) == ok, element(2, Result)}).

test_should_return_expected_person_record() ->	
	PersonRecord = #person{idno=99999999, firstName="Dan", lastName="Mohl", dob="08/28/1977", ssn="123-45-9876"},
	RecordFieldNames = record_info(fields, person),
	querly_db:doc_create(get_test_db_name(), "999", PersonRecord, RecordFieldNames),
	ResultJson = element(2, querly_db:doc_get(get_test_db_name(), "999")),
	Result = querly_db:build_record(ResultJson, PersonRecord, RecordFieldNames),
	test_helper:display_message({"test_should_return_expected_person_record", Result == PersonRecord, Result}).
	
test_get_database_names() ->
	ecouch:db_create("test_get_database_names_1"),
	ecouch:db_create("test_get_database_names_2"),
	ecouch:db_create("test_get_database_names_3"),
	DatabaseNameList = querly_db:get_database_names(),
	Result = erlang:length(DatabaseNameList),
	ecouch:db_delete("test_get_database_names_1"),
	ecouch:db_delete("test_get_database_names_2"),
	ecouch:db_delete("test_get_database_names_3"),
	test_helper:display_message({"test_get_database_names", Result >= 3, Result}).

test_db_should_exist() ->
	ecouch:db_create("person_exist"),
    Result = querly_db:db_exists("person_exist"),
	ecouch:db_delete("person_exist"),
	test_helper:display_message({"test_db_should_exist", Result == true, Result}).

test_get_table_by_name_with_table_found() ->
	PersonTable = ets:new(table, [{keypos, 1}]),
	CustomerTable = ets:new(table, [{keypos, 1}]),
	TableList = [{"person", PersonTable}, {"customer", CustomerTable}],
	Result = querly_db:get_table_by_name("customer", TableList), 
   	test_helper:display_message({"test_get_table_by_name_with_table_found", Result == CustomerTable, Result}).
    	
test_get_table_by_name_with_table_not_found() ->
	PersonTable = ets:new(table, [{keypos, 1}]),
	TableList = [{"person", PersonTable}, {"person2", PersonTable}],
	Result = querly_db:get_table_by_name("person3", TableList), 
   	test_helper:display_message({"test_get_table_by_name_with_table_not_found", Result  == undefined, Result}).

test_should_load_person_records() ->
	PersonRecord1 = #person{idno=1, firstName="Dan", lastName="Mohl", dob="08/28/1977", ssn="123-45-9876"},
	PersonRecord2 = #person{idno=2, firstName="Dan2", lastName="Mohl", dob="08/28/1977", ssn="123-45-9876"},
	PersonRecord3 = #person{idno=3, firstName="Dan3", lastName="Mohl", dob="08/28/1977", ssn="123-45-9876"},
	RecordFieldNames = record_info(fields, person),
	querly_db:doc_create("1", PersonRecord1, RecordFieldNames),
	querly_db:doc_create("2", PersonRecord2, RecordFieldNames),
	querly_db:doc_create("3", PersonRecord3, RecordFieldNames),
	Pid = spawn(querly_db, tables_service, [[]]),
	DefaultRecord = #person{},
	RecordFieldNames = record_info(fields, person),
	Pid ! {self(), get_table, #person.idno, DefaultRecord, RecordFieldNames},
	receive
		{table_results, Table} ->
			People = Table;
		_Received -> 
			io:format("~p", _Received),
			People = ets:new(table, [{keypos, #person.idno}])
	end,
	Result = ets:select(People, [{#person{_ = '_'}, [], ['$_']}]),
	test_helper:display_message({"test_should_load_person_records", erlang:length(Result) == 3, erlang:length(Result)}),
	ecouch:db_delete("person").

test_should_load_employer_records() ->
	Record1 = #employer{id=1, name="ABC Corp.", address="123 South, Nashville, IN, 98766"},
	Record2 = #employer{id=2, name="123 Inc.", address="321 Main, Nsh, TN 30727"},
	RecordFieldNames = record_info(fields, employer),
	querly_db:doc_create("1", Record1, RecordFieldNames),
	querly_db:doc_create("2", Record2, RecordFieldNames),
	Pid = spawn(querly_db, tables_service, [[]]),
	DefaultRecord = #employer{},
	RecordFieldNames = record_info(fields, employer),
	Pid ! {self(), get_table, #employer.id, DefaultRecord, RecordFieldNames},
	receive
		{table_results, Table} ->
			Employer = Table;
		_Received -> 
			io:format("~p", _Received),
			Employer = ets:new(table, [{keypos, #employer.id}])
	end,
	Result = ets:select(Employer, [{#employer{_ = '_'}, [], ['$_']}]),
	test_helper:display_message({"test_should_load_employer_records", erlang:length(Result) == 2, erlang:length(Result)}),
	ecouch:db_delete("employer").
