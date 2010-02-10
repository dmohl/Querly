-module(querly_client).
-author('Dan Mohl').

-export([sql_query/1, select/2, is_valid_record/1, parse_where_clause/3, get_record_fields/1, parse_sql/1]).

-include_lib("record_definitions.hrl").

cast_value(Value) ->
	cast_value(integer, Value). % start with type of integer and cycle through others until we find a valid type match
cast_value(Type, Value) ->
	case Type of
		integer -> 
			case is_typeof_integer(Value) of
				true -> 
					list_to_integer(Value);
				_ -> 
					cast_value(bitstring, Value)
			end;		
		bitstring ->
			case is_typeof_bitstring(Value) of
				true -> 
					re:replace(Value, "\"", "", [{return, binary}, global]);
				_ -> 
					cast_value(other, Value)
			end;		
		_ -> 
			Value
	end.	

is_typeof_integer(Value) ->
	Result = (catch list_to_integer(Value)),
	case Result of 
		_ when is_integer(Result) -> true;
		_ -> false
	end.

is_typeof_bitstring(Value) ->
	QuotePosition = string:str(Value, "\""),
	case QuotePosition of
		0 -> 
			false;
		_ ->  	
			Result = (catch list_to_bitstring(Value)),
			case Result of 
				_ when is_bitstring(Result) -> 
					true;
				_ -> 
					false
			end
	end.
	
sql_query(Sql) ->
	SqlParsed = parse_sql(Sql),
	FromRecordName = get_parsed_sql_values(from, SqlParsed),
	WhereSql = get_parsed_sql_values(where, SqlParsed),
	case re:split(WhereSql, " and ", [{return, list}, trim]) of 
		[[]] -> 	
			WhereElements = [];
		WhereTokens -> 
			WhereElements = lists:map(fun(Element) -> 
											    SplitResult = re:split(Element, "=", [{return, list}, trim]),
												{string:strip(lists:nth(1, SplitResult)), 
												 cast_value(string:strip(lists:nth(2, SplitResult)))}
											end, WhereTokens)
	end,
	select(FromRecordName, WhereElements).		
	
select(RecordName, WhereElements) ->
	case is_valid_record(RecordName) of
		true -> 
			RecordFieldNames = get_record_fields(RecordName),
			Criteria = parse_where_clause(RecordName, WhereElements, RecordFieldNames),
			DefaultRecord = transform_to_record([], list_to_atom(RecordName), '_', RecordFieldNames),
			PrimaryKeyIndex = 2,
			start_querly(),
			querly:select(Criteria, DefaultRecord, RecordFieldNames, PrimaryKeyIndex);
		false ->	
			[]
	end.

start_querly() ->
	querly:start().
	
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
			Rest = "";
		_ -> 
			ElementsToSelect = WhereStartPosition - FromStartPosition,	 
			FromSyntax = string:strip(string:substr(Sql, FromStartPosition, ElementsToSelect)),
			Rest = string:substr(Sql, WhereStartPosition)
	end,		
	[{from, FromSyntax}, Rest].

parse_sql_where(Sql) ->
	case (string:len(string:strip(Sql))) of
		0 ->
			WhereSyntax = "";
		_ -> 
			WhereStartPosition = string:len("where "),
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
	 

%querly_client:select("select * from person where idno = 1").
