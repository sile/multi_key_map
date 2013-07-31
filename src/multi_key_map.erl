%% @doc 一つの値に対して複数のキーが割り当て可能なマップの実装
-module(multi_key_map).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([
         new/2,
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

%%--------------------------------------------------------------------------------
%% Exported Types
%%--------------------------------------------------------------------------------
-export_type([
              map/0,
              keyset/0,
              keyset_name/0,
              keyset_field/0,
              keyset_field_index/0,
              key/0,
              value/0,
              fold_fun/0
             ]).

%%--------------------------------------------------------------------------------
%% Macros and Records
%%--------------------------------------------------------------------------------
-define(MAP, ?MODULE).

-record(?MAP,
        {
          record_name :: keyset_name(),
          fields      :: [keyset_field()], % result of `record_info(fields, RecordName)'
          inner_maps  :: [dict()]
        }).

%%--------------------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------------------
-type map()                :: #?MAP{}.
-type keyset()             :: tuple().           % キーセットを表現するレコード
-type keyset_name()        :: atom().            % キーセットの名前. キーセットを表現するレコードの名前に相当
-type keyset_field()       :: atom().            % キーセット内のキー(フィールド)の名前
-type keyset_field_index() :: non_neg_integer(). % 検索時等に使用するキーのインデックス. インデックスは `#RecordName.Field' 取得可能.

-type key()   :: term().
-type value() :: term().

-type fold_fun() :: fun ((keyset(), value(), Acc::term()) -> AccNext::term()).

%%--------------------------------------------------------------------------------
%% Functions
%%--------------------------------------------------------------------------------

%% @doc マップインスタンスを生成する.
%%
%% `KeySetFields'は`record_info(fields, KeySetNameName)'を使って取得すること.
-spec new(KeySetName, KeySetFields) -> map() when
      KeySetName   :: keyset_name(),
      KeySetFields :: [keyset_field_index()].
new(KeySetName, KeySetFields) ->
    #?MAP{record_name = KeySetName,
          fields      = KeySetFields,
          inner_maps  = [dict:new() || _ <- KeySetFields]}.

%% @doc 引数の値が`multi_key_map'のインスタンスかどうかを判定する.
-spec is_multi_key_map(Value::term()) -> boolean().
is_multi_key_map(#?MAP{}) -> true;
is_multi_key_map(_)       -> false.

%% @doc 要素を挿入する.
%%
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

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec fold_inner_maps(fold_fun(), term(), keyset(), map()) -> Result::term().
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
