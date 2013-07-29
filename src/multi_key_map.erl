%% @doc マルチキーマップの実装
-module(multi_key_map).

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
-export([
         make/1,
         store/3,
         find/3,
         erase/3,
         fold/3
        ]).

%%--------------------------------------------------------------------------------
%% Exported Types
%%--------------------------------------------------------------------------------
-export_type([
              map/0,
              index_name/0,
              key/0,
              value/0
             ]).

%%--------------------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------------------
-define(MAP, ?MODULE).

-define(INNER_MAP_NEW(), dict:new()).
-define(INNER_MAP_FIND(Key, Map), dict:find(Key, Map)).
-define(INNER_MAP_STORE(Key, Value, Map), dict:store(Key, Value, Map)).
-define(INNER_MAP_ERASE(Key, Map), dict:erase(Key, Map)).
-define(INNER_MAP_FOLD(Fun, Init, Map), dict:fold(Fun, Init, Map)).

%%--------------------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------------------
-record(?MAP,
        {
          maps :: [{index_name(), inner_map()}]
        }).

%%--------------------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------------------
-type map() :: #?MAP{}.
-type index_name() :: term().
-type key() :: term().
-type value() :: term().

-type inner_map() :: dict().

%%--------------------------------------------------------------------------------
%% Functions
%%--------------------------------------------------------------------------------
-spec make([index_name()]) -> map().
make([]) ->
    error(empty_list_is_not_allowed);
make(IndexNames) ->
    #?MAP{maps = [{Name, ?INNER_MAP_NEW()} || Name <- IndexNames]}.

-spec store(Keys, value(), map()) -> map() when
      Keys :: [{index_name(), key()}].
store(Keys, Value, Map) ->
    #?MAP{maps = Maps} = Map,
    Entry = {Keys, Value},  % メモリ使用量的に無駄は多い
    Result = 
        lists:foldl(fun ({IndexName, Key}, {OldMaps, NewMaps}) ->
                            case lists:keytake(IndexName, 1, OldMaps) of
                                false                            -> error(unknown_index, IndexName);
                                {value, {_, InnerMap}, OldMaps2} ->
                                    {OldMaps2, [{IndexName, ?INNER_MAP_STORE(Key, Entry, InnerMap)} | NewMaps]}
                            end
                    end,
                    {Maps, []},
                    Keys),
    case Result of
        {[], Maps2} -> Map#?MAP{maps = Maps2};
        _           -> error(partial_keys)
    end.


-spec find(index_name(), key(), map()) -> error | {ok, value()}.
find(IndexName, Key, Map) ->
    case lists:keyfind(IndexName, 1, Map#?MAP.maps) of
        false         -> error; % error(...) ?
        {_, InnerMap} -> case ?INNER_MAP_FIND(Key, InnerMap) of
                             error            -> error;
                             {ok, {_, Value}} -> {ok, Value}
                         end
    end.

-spec erase(index_name(), key(), map()) -> map().
erase(IndexName, Key, Map) ->
    case find_keys(IndexName, Key, Map) of
        error      -> Map;
        {ok, Keys} ->
            Maps =
                lists:foldl(fun ({IndexName2, Key2}, {OldMaps, NewMaps}) ->
                                    {value, {_, InnerMap}, OldMap2} = lists:keytake(IndexName2, 1, OldMaps),
                                    {OldMap2, [{IndexName2, ?INNER_MAP_ERASE(Key2, InnerMap)} | NewMaps]}
                            end,
                            {Map#?MAP.maps, []},
                            Keys),
            Map#?MAP{maps = Maps}
    end.

-spec fold(FoldFun, InitValue, map()) -> Result when
      FoldFun   :: fun (([{index_name(), key()}], value(), Acc) -> Acc),
      InitValue :: term(),
      Acc       :: term(),
      Result    :: term().
fold(FoldFun, InitValue, Map) ->
    #?MAP{maps = [{_, FirstInnerMap} | _]} = Map,
    ?INNER_MAP_FOLD(fun (_, {Keys, Value}, Acc) -> FoldFun(Keys, Value, Acc) end,
                    InitValue,
                    FirstInnerMap).

%%--------------------------------------------------------------------------------
%% Functions
%%--------------------------------------------------------------------------------
-spec find_keys(index_name(), key(), map()) -> error | {ok, value()}.
find_keys(IndexName, Key, Map) ->
    case lists:keyfind(IndexName, 1, Map#?MAP.maps) of
        false         -> error; % error(...) ?
        {_, InnerMap} -> case ?INNER_MAP_FIND(Key, InnerMap) of
                             error           -> error;
                             {ok, {Keys, _}} -> {ok, Keys}
                         end
    end.
