-module(querly_db).
-author('Dan Mohl').

-export([start/0, build_json/2, build_record/3, doc_create/3, doc_create/4, doc_get_all/1, doc_get/2,
		tables_service/1, get_database_names/0, db_exists/1]).

-include_lib("record_definitions.hrl").

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
		{From, get_table, DatabaseName, PrimaryKeyPosition, DefaultRecord, RecordFieldNames} ->
			case Table of
				undefined -> 
					NewTable = ets:new(table, [{keypos, PrimaryKeyPosition}]),
					build_table_from_couch(DatabaseName, NewTable, DefaultRecord, RecordFieldNames),
					From ! {table_results, NewTable},
					tables_service(NewTable);
				_ -> 
					From ! {table_results, Table},
					tables_service(Table)
			end;
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
	DatabaseName = element(1, DefaultRecord),
	build_table_from_couch(DatabaseName, Table, DefaultRecord, RecordFieldNames).
build_table_from_couch(DatabaseName, Table, DefaultRecord, RecordFieldNames) ->
	db_create_if_needed(DatabaseName),
	Docs = doc_get_all(DatabaseName),
	Rows = lists:nth(3, element(2, element(2, Docs))),
	ResultList = element(2, Rows),	
	lists:foreach(fun(RowJson) -> 
		DocumentJson = element(2, lists:nth(4, element(2, RowJson))), 
		Record = build_record(DocumentJson, DefaultRecord, RecordFieldNames),
		ets:insert(Table, Record) end, ResultList),
	Table.

db_create_if_needed(DatabaseName) ->
    case db_exists(DatabaseName) of
		false -> ecouch:db_create(DatabaseName);
		_ -> ok
	end.	

db_exists(DatabaseName) ->
    lists:any(fun(DbName) -> bitstring_to_list(DbName) == DatabaseName end, get_database_names()).

doc_create(DocName, RecordToTransform, RecordFieldNames) ->
	DatabaseName = atom_to_list(element(1, RecordToTransform)),
	doc_create(DatabaseName, DocName, RecordToTransform, RecordFieldNames).
doc_create(DatabaseName, DocName, RecordToTransform, RecordFieldNames) ->
	Json = querly_db:build_json(RecordToTransform, RecordFieldNames),
    ecouch:doc_create(DatabaseName, DocName, Json).	
    
doc_get_all(DatabaseName) ->    
	ecouch:doc_get_all(DatabaseName, [{include_docs,true}]).
	
doc_get(DatabaseName, DocName) ->    
	ecouch:doc_get(DatabaseName, DocName).	
	
get_database_names() ->
	element(2, ecouch:db_list()).
