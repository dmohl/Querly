-module(querly_client).
-author('Dan Mohl').

-export([select/1, select/2, is_valid_record/1, parse_where_clause/3, get_record_fields/1, parse_sql/1]).

-include_lib("record_definitions.hrl").

select(Sql) ->
	SqlParsed = parse_sql(Sql),
	FromRecordName = get_parsed_sql_values(from, SqlParsed),
	WhereSql = get_parsed_sql_values(where, SqlParsed),
	case re:split(WhereSql, " and ", [{return, list}, trim]) of 
		[] -> 	
			WhereElements = [];
		WhereTokens -> 
			WhereElements = lists:map(fun(Element) -> 
											    SplitResult = re:split(Element, "=", [{return, list}, trim]),
												{string:strip(lists:nth(1, SplitResult)), 
												 string:strip(lists:nth(2, SplitResult))}
											end, WhereTokens)
	end,
	select(FromRecordName, WhereElements).		
	
select(RecordName, WhereElements) ->
	case is_valid_record(RecordName) of
		true -> 
			RecordFieldNames = get_record_fields(RecordName),
			Criteria = parse_where_clause(RecordName, WhereElements, RecordFieldNames),
			DefaultRecord = transform_to_record("", list_to_atom(RecordName), '_', RecordFieldNames),
			PrimaryKeyIndex = 2,
			querly:select(Criteria, DefaultRecord, RecordFieldNames, PrimaryKeyIndex);
		false ->	
			[]
	end.
	
parse_sql(Sql) ->
	SelectList = parse_sql_select(Sql),
	SelectSyntax = lists:nth(1, SelectList),
	FromList = parse_sql_from(lists:nth(2, SelectList)),
	FromSyntax = lists:nth(1, FromList),
	WhereSyntax = parse_sql_where(lists:nth(2, FromList)),
	[SelectSyntax, FromSyntax, WhereSyntax].

parse_sql_select(Sql) ->
	FromStartPosition = string:str(string:to_lower(Sql), "from"),
	SelectStartPosition = string:str(string:to_lower(Sql), "select") + string:len("select "),
    ElementsToSelect = FromStartPosition - SelectStartPosition,	
	SelectSyntax = string:strip(string:substr(Sql, SelectStartPosition, ElementsToSelect)),
	Rest = string:substr(Sql, FromStartPosition),
	[{select, SelectSyntax}, Rest].

parse_sql_from(Sql) ->
	WhereStartPosition = string:str(string:to_lower(Sql), "where"),
	FromStartPosition = string:len("from "),
	case (WhereStartPosition) of
		0 ->
			FromSyntax = string:strip(string:substr(Sql, FromStartPosition)),
			Rest = [];
		_ -> 
			ElementsToSelect = WhereStartPosition - FromStartPosition,	 
			FromSyntax = string:strip(string:substr(Sql, FromStartPosition, ElementsToSelect)),
			Rest = string:substr(Sql, WhereStartPosition)
	end,		
	[{from, FromSyntax}, Rest].

parse_sql_where(Sql) ->
	WhereStartPosition = string:len("where "),
	case (string:len(string:strip(Sql))) of
		0 ->
			WhereSyntax = [];
		_ -> 
			WhereSyntax = string:strip(string:substr(Sql, WhereStartPosition))
	end,		
	{where, WhereSyntax}.
	
parse_where_clause(RecordName, Where, RecordFieldNames) ->
	transform_to_record(Where, list_to_atom(RecordName), '_', RecordFieldNames).
	
get_parsed_sql_values(Clause, ClauseList) ->
    Result = lists:filter(fun(ClauseTuple) -> element(1, ClauseTuple) == Clause end, ClauseList),
	case Result of
	    [] -> "";
		_ -> element(2, lists:nth(1,Result))
	end.

is_valid_record(RecordName) ->
    Result = lists:filter(fun(RecordTuple) -> atom_to_list(element(1, RecordTuple)) == RecordName end, ?recordMetadata),
	case Result of
	    [] -> false;
		_ -> true
	end.

get_record_fields(RecordName) ->
    Result = lists:filter(fun(RecordTuple) -> atom_to_list(element(1, RecordTuple)) == RecordName end, ?recordMetadata),
	case Result of
	    [] -> undefined;
		_ -> element(2, lists:nth(1,Result))
	end.
	
transform_to_record(Values, RecordName, DefaultValue, Fields) ->
    list_to_tuple([RecordName | decode_record_fields(Values, DefaultValue, 2, Fields)]).

decode_record_fields(_Values, _DefaultValue, _Index, []) ->
    [];
decode_record_fields(Values, DefaultValue, Index, [Field | Rest]) ->
    [case lists:keysearch(atom_to_list(Field), 1, Values) of
	 {value, {_, Value}} ->
	     Value;
	 false ->
	     DefaultValue
     end | decode_record_fields(Values, DefaultValue, Index + 1, Rest)].
	 

%querly_client:select("select * from person where firstName=\"Dan\" and lastName=\"Mohl\"").
