-module(querly_helper).
-author('Dan Mohl').

-export([is_valid_record/1, get_record_fields/1, transform_to_record/4, is_typeof_integer/1, is_typeof_bitstring/1]).

-include_lib("record_definitions.hrl").

is_valid_record(RecordName) ->
	Result = get_record_fields(RecordName),
	case Result of
	    undefined -> false;
		_ -> true
	end.

get_record_fields(RecordName) ->
	RecordNameAtom = case is_atom(RecordName) of
		true ->
			RecordName;
		false ->
			list_to_atom(RecordName)
	end,	
    Result = lists:filter(fun(RecordTuple) -> 
		element(1, RecordTuple) == RecordNameAtom end, ?recordMetadata),
	case Result of
	    [] -> undefined;
		_ -> element(2, lists:nth(1,Result))
	end.
	
transform_to_record(Values, RecordName, DefaultValue, Fields) ->
	RecordNameAtom = case is_atom(RecordName) of
		true ->
			RecordName;
		false ->
			list_to_atom(RecordName)
	end,	
    list_to_tuple([RecordNameAtom | decode_record_fields(Values, DefaultValue, 2, Fields)]).

decode_record_fields(_Values, _DefaultValue, _Index, []) -> [];
decode_record_fields(Values, DefaultValue, Index, [Field | Rest]) ->
    [case lists:keysearch(atom_to_list(Field), 1, Values) of
	 {value, {_, Value}} ->
	     Value;
	 false ->
	     DefaultValue
     end | decode_record_fields(Values, DefaultValue, Index + 1, Rest)].

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
