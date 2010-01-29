-module(querly_db).
-author('Dan Mohl').

-export([start/0, build_json/2, build_record/3, doc_create/3, doc_create/4, db_info/1, db_create/1, doc_get_all/1, doc_get/2,
		tables_service/1]).

-include_lib("record_definitions.hrl").

get_database_name() ->
    "test_person_db_load".

start() ->
	inets:start(),
	application:start(ecouch).

build_json(RecordToTransform, RecordFieldNames) ->
	Record = element(1, RecordToTransform),
    rfc4627:from_record(RecordToTransform, Record, RecordFieldNames).

build_record(Json, DefaultRecord, RecordFieldNames) ->
    rfc4627:to_record(Json, DefaultRecord, RecordFieldNames).

tables_service(Table) ->
	receive
		{From, get_table, PrimaryKeyPosition, DefaultRecord, RecordFieldNames} ->
			case Table of
				undefined -> 
					NewTable = ets:new(table, [{keypos, PrimaryKeyPosition}]),
					build_table_from_couch(NewTable, DefaultRecord, RecordFieldNames),
					From ! {table_results, NewTable},
					tables_service(NewTable);
				_ -> 
					From ! {table_results, Table},
					tables_service(Table)
			end
	end.
    
build_table_from_couch(Table, DefaultRecord, RecordFieldNames) ->
	Docs = doc_get_all(get_database_name()),
	Rows = lists:nth(3, element(2, element(2, Docs))),
	ResultList = element(2, Rows),	
	lists:foreach(fun(RowJson) -> 
		DocumentJson = element(2, lists:nth(4, element(2, RowJson))), 
		Record = build_record(DocumentJson, DefaultRecord, RecordFieldNames),
		ets:insert(Table, Record) end, ResultList),
	Table.

db_info(DatabaseName) -> 
    ecouch:db_info(DatabaseName).

db_create(DatabaseName) ->
    ecouch:db_create(DatabaseName).

doc_create(DatabaseName, DocName, RecordToTransform, RecordFieldNames) ->
	Json = querly_db:build_json(RecordToTransform, RecordFieldNames),
    doc_create(DatabaseName, DocName, Json).	
doc_create(DatabaseName, DocName, Json) ->
    ecouch:doc_create(DatabaseName, DocName, Json).
    
doc_get_all(DatabaseName) ->    
	ecouch:doc_get_all(DatabaseName, [{include_docs,true}]).
	
doc_get(DatabaseName, DocName) ->    
	ecouch:doc_get(DatabaseName, DocName).	
	

