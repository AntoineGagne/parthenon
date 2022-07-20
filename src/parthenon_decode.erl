-module(parthenon_decode).

%% API
-export([decode/2]).

%%%===================================================================
%%% API
%%%===================================================================

decode(_SchemaName, Binary) ->
    decode(Binary, value, [], <<>>).

%%%===================================================================
%%% Internal functions
%%%===================================================================

decode(Binary, Next, Nexts, Buffer) ->
    whitespace(Binary, Next, Nexts, Buffer).

whitespace(<<$\s, Binary/binary>>, Next, Nexts, Buffer) ->
    whitespace(Binary, Next, Nexts, Buffer);
whitespace(<<$\t, Binary/binary>>, Next, Nexts, Buffer) ->
    whitespace(Binary, Next, Nexts, Buffer);
whitespace(<<$\r, Binary/binary>>, Next, Nexts, Buffer) ->
    whitespace(Binary, Next, Nexts, Buffer);
whitespace(<<$\n, Binary/binary>>, Next, Nexts, Buffer) ->
    whitespace(Binary, Next, Nexts, Buffer);
whitespace(Binary, Next, Nexts, Buffer) ->
    case Next of
        value ->
            value(Binary, Nexts, Buffer);
        object ->
            object(Binary, Nexts, Buffer);
        {object_key, Object} ->
            object_key(Binary, Object, Nexts, Buffer);
        {object_next, Object} ->
            object_next(Binary, Object, Nexts, Buffer);
        {array_next, Values} ->
            array_next(Binary, Values, Nexts, Buffer)
    end.

value(<<"false", _Rest/binary>>, _Nexts, _Buffer) ->
    false;
value(<<"true", _Rest/binary>>, _Nexts, _Buffer) ->
    true;
value(<<"null", _Rest/binary>>, _Nexts, _Buffer) ->
    undefined;
value(<<${, Rest/binary>>, Nexts, Buffer) ->
    whitespace(Rest, object, Nexts, Buffer);
value(<<$[, Rest/binary>>, Nexts, Buffer) ->
    whitespace(Rest, array, Nexts, Buffer).

% next(Remainder, Value, [], _Buffer) ->
%     {ok, Value, Remainder};
% next(Binary, Value, [Next | Nexts], Buffer) ->
%     case Next of
%         {array_next, Values} ->
%             whitespace(Binary, {array_next, [Value | Values]}, Nexts, Buffer);
%         {object_value, Object} ->
%             whitespace(Binary, {object_value, Value, Object}, Nexts, Buffer);
%         {object_next, Key, Object} ->
%             whitespace(Binary, {object_next, [{Key, Value} | Object]}, Nexts, Buffer)
%     end.

object(_Binary, _Nexts, _Buffer) ->
    ok.

object_next(_Binary, _Object, _Nexts, _Buffer) ->
    ok.

array_next(_Binary, _Values, _Nexts, _Buffer) ->
    ok.

object_key(_Binary, _Object, _Nexts, _Buffer) ->
    ok.
