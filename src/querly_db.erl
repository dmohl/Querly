-module(querly_db).
-author('Dan Mohl').

-export([start/0, build_json/2, build_record/3, doc_create/3, doc_create/4, doc_get_all/1, doc_get/2,
		tables_service/1, get_database_names/0, db_exists/1, get_table_by_name/2]).

-include_lib("record_definitions.hrl").

start() ->
	inets:start(),
	application:start(ecouch).

build_json(RecordToTransform, RecordFieldNames) ->
	Record = element(1, RecordToTransform),
    rfc4627:from_record(RecordToTransform, Record, RecordFieldNames).

build_record(Json, DefaultRecord, RecordFieldNames) ->
    rfc4627:to_record(Json, DefaultRecord, RecordFieldNames).

tables_service(DatabaseInformationTuple) ->
	receive
		{From, get_table, DatabaseName, PrimaryKeyPosition, DefaultRecord, RecordFieldNames} ->
			process_get_table_request(From, DatabaseName, DatabaseInformationTuple, PrimaryKeyPosition, DefaultRecord, RecordFieldNames);
		{From, get_table, PrimaryKeyPosition, DefaultRecord, RecordFieldNames} ->
			Name = atom_to_list(element(1, DefaultRecord)),
			process_get_table_request(From,Name, DatabaseInformationTuple, PrimaryKeyPosition, DefaultRecord, RecordFieldNames)
	end.

process_get_table_request(From, TableName, DatabaseInformationTuple, PrimaryKeyPosition, DefaultRecord, RecordFieldNames) ->
	TableList = element(2, DatabaseInformationTuple),
	DatabaseNamingFormat = element(1, DatabaseInformationTuple),
	FormattedDatabaseName = binary_to_list(list_to_binary(io_lib:format(DatabaseNamingFormat, [TableName]))),
	case get_table_by_name(FormattedDatabaseName, TableList) of
		undefined -> 
			Table = ets:new(table, [{keypos, PrimaryKeyPosition}]),
			build_table_from_couch(FormattedDatabaseName, Table, DefaultRecord, RecordFieldNames),
			NewTableList = lists:append([{FormattedDatabaseName, Table}], TableList),
			From ! {table_results, Table},
			tables_service({DatabaseNamingFormat, NewTableList});
		Table -> 
			From ! {table_results, Table},
			tables_service({DatabaseNamingFormat, TableList})
	end.
    
build_table_from_couch(DatabaseName, Table, DefaultRecord, RecordFieldNames) ->
	db_create_if_needed(DatabaseName),
	Docs = doc_get_all(DatabaseName),
	case element(2, element(2, Docs)) of
		[] ->
			Table;
		_ ->
			Rows = lists:nth(3, element(2, element(2, Docs))),
			ResultList = element(2, Rows),	
			lists:foreach(fun(RowJson) -> 
				DocumentJson = element(2, lists:nth(4, element(2, RowJson))), 
				Record = build_record(DocumentJson, DefaultRecord, RecordFieldNames),
				ets:insert(Table, Record) end, ResultList)
	end,			
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
	db_create_if_needed(DatabaseName),
    ecouch:doc_create(DatabaseName, DocName, Json).	
    
doc_get_all(DatabaseName) ->    
	ecouch:doc_get_all(DatabaseName, [{include_docs,true}]).
	
doc_get(DatabaseName, DocName) ->    
	ecouch:doc_get(DatabaseName, DocName).	
	
get_database_names() ->
	element(2, ecouch:db_list()).

get_table_by_name(Name, TableList) ->
    TableResult = lists:filter(fun(TableTuple) -> element(1, TableTuple) == Name end, TableList),
	case TableResult of
	    [] -> undefined;
		_ -> element(2, lists:nth(1, TableResult))
	end.	
