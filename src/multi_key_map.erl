-module(multi_key_map).

-export([
         new/1,
         is_multi_key_map/1,
         insert/3,
         update/4,
         find/3,
         erase/3,
         fold/3,
         foreach/2,
         to_list/1,
         size/1
        ]).

-export_type([
              map/0,
              keyset/0,
              key/0,
              value/0
             ]).

-define(MAP, ?MODULE).

-record(?MAP,
        {
          fields     :: [atom()],
          inner_maps :: [dict()]
        }).

-type map() :: #?MAP{}.

-type keyset() :: tuple().
-type key()    :: term().
-type value()  :: term().

-spec new(KeySetRecordFields) -> map() when
      KeySetRecordFields :: [atom()].
new(KeySetRecordFields) ->
    #?MAP{fields = KeySetRecordFields,
          inner_maps = [dict:new() || _ <- KeySetRecordFields]}.

-spec is_multi_key_map(Value::term()) -> boolean().
is_multi_key_map(#?MAP{}) -> true;
is_multi_key_map(_)       -> false.

-spec insert(keyset(), value(), map()) -> {ok, map()} | {error, Reason} when
      Reason :: {key_exists, atom(), Key::key()}.
insert(KeySet, Value, Map) ->
    Entry = {KeySet, Value},
    Result = 
        fold_inner_maps(fun (_, _, {error, Reason}) ->
                                {error, Reason};
                            (Key, InnerMap, {ok, Acc}) ->
                                case dict:is_key(Key, InnerMap) of
                                    true  ->
                                        Index = length(Acc) + 1,
                                        FieldName = lists:nth(Index, Map#?MAP.fields),
                                        {error, {key_exists, FieldName, Key}};
                                    false ->
                                        {ok, [dict:store(Key, Entry, InnerMap) | Acc]}
                                end
                        end,
                        {ok, []},
                        KeySet, Map),
    case Result of
        {error, Reason} -> {error, Reason};
        {ok, InnerMaps} -> {ok, Map#?MAP{inner_maps = lists:reverse(InnerMaps)}}
    end.

-spec update(non_neg_integer(), key(), value(), map()) -> {ok, map()} | error.
update(KeyIndex, Key, Value, Map) ->
    InnerMap = lists:nth(KeyIndex-1, Map#?MAP.inner_maps),
    case dict:find(Key, InnerMap) of
        error       -> error;
        {ok, Entry} ->
            {KeySet, _OldValue} = Entry,
            {ok, update_impl(KeySet, Value, Map)}
    end.

-spec find(non_neg_integer(), key(), map()) -> {ok, keyset(), value()} | error.
find(KeyIndex, Key, Map) ->
    InnerMap = lists:nth(KeyIndex-1, Map#?MAP.inner_maps),
    case dict:find(Key, InnerMap) of
        error                 -> error;
        {ok, {KeySet, Value}} -> {ok, KeySet, Value}
    end.

-spec erase(non_neg_integer(), key(), map()) -> map().
erase(KeyIndex, Key, Map) ->
    InnerMap = lists:nth(KeyIndex-1, Map#?MAP.inner_maps),
    case dict:find(Key, InnerMap) of
        error             -> Map;
        {ok, {KeySet, _}} -> erase_impl(KeySet, Map)
    end.

-spec fold(Fun, value(), map()) -> Result when
      Fun     :: fun ((keyset(), value(), Acc) -> NextAcc),
      Acc     :: term(),
      NextAcc :: term(),
      Result  :: term().
fold(Fun, Initial, Map) ->
    dict:fold(fun (_, {KeySet, Value}, Acc) ->
                      Fun(KeySet, Value, Acc)
              end,
              Initial,
              first_inner_map(Map)).

-spec foreach(Fun, map()) -> ok when
      Fun :: fun ((keyset(), value()) -> any()).
foreach(Fun, Map) ->
    fold(fun (KeySet, Value, _) -> Fun(KeySet, Value) end,
         ok,
         Map),
    ok.

-spec to_list(map()) -> [{keyset(), value()}].
to_list(Map) ->
    lists:reverse(dict:fold(fun (_, Entry, Acc) -> [Entry | Acc] end,
                            [],
                            first_inner_map(Map))).

-spec size(map()) -> non_neg_integer().
size(Map) ->
    dict:size(first_inner_map(Map)).

-spec fold_inner_maps(Fun, term(), keyset(), map()) -> Result::term() when
      Fun :: fun ((key(), dict(), Acc::term()) -> NextAcc::term()).
fold_inner_maps(Fun, Initial, KeySet, Map) ->
    {_, AccResult} = 
        lists:foldl(fun (InnerMap, {I, Acc}) ->
                            Key = element(I, KeySet),
                            {I+1, Fun(Key, InnerMap, Acc)}
                    end,
                    {2, Initial},
                    Map#?MAP.inner_maps),
    AccResult.

-spec update_impl(keyset(), value(), map()) -> map().
update_impl(KeySet, Value, Map) ->
    Entry = {KeySet, Value},
    InnerMaps = fold_inner_maps(fun (Key, InnerMap, Acc) ->
                                        [dict:store(Key, Entry, InnerMap) | Acc]
                                end,
                                [],
                                KeySet, Map),
    Map#?MAP{inner_maps = lists:reverse(InnerMaps)}.

-spec erase_impl(keyset(), map()) -> map().
erase_impl(KeySet, Map) ->
    InnerMaps = fold_inner_maps(fun (Key, InnerMap, Acc) ->
                                        [dict:erase(Key, InnerMap) | Acc]
                                end,
                                [],
                                KeySet, Map),
    Map#?MAP{inner_maps = lists:reverse(InnerMaps)}.

-spec first_inner_map(map()) -> dict().
first_inner_map(#?MAP{inner_maps = [First | _]}) ->
    First.
