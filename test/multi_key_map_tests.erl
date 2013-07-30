%% @doc multi_key_map モジュールのテスト
-module(multi_key_map_tests).

-include_lib("eunit/include/eunit.hrl").

-record(keys,
        {
          key1 :: any(),
          key2 :: any(),
          key3 :: any()
        }).

new_test_() ->
    [
     {"マップインスタンスが生成できる",
      fun () ->
              Map = multi_key_map:new(record_info(fields, keys)),
              ?assert(multi_key_map:is_multi_key_map(Map))
      end}
    ].

insert_test_() ->
    [
     {"要素を追加する",
      fun () ->
              Keys  = #keys{key1 = 10, key2 = 20, key3 = 30},
              Value = value,
              
              Map0    = multi_key_map:new(record_info(fields, keys)),
              Result0 = multi_key_map:insert(Keys, Value, Map0),
              ?assertMatch({ok, _}, Result0),
              {ok, Map1} = Result0,

              ?assertEqual([{Keys, Value}], multi_key_map:to_list(Map1))
      end},
     {"いずれかのキーが既に存在する場合は、要素が追加できない",
      fun () ->
              Keys0  = #keys{key1 = 10, key2 = 20, key3 = 30},
              Value0 = value0,

              Keys1  = #keys{key1 = abc, key2 = 20, key3 = 'ABC'}, % key2 が重複
              Value1 = value1,
              
              Map0       = multi_key_map:new(record_info(fields, keys)),
              {ok, Map1} = multi_key_map:insert(Keys0, Value0, Map0),

              ?assertEqual({error, {key_exists, key2, 20}}, multi_key_map:insert(Keys1, Value1, Map1))
      end}
    ].

update_test_() ->
    [
     {"要素を更新する",
      fun () ->
              Keys  = #keys{key1 = 10, key2 = 20, key3 = 30},
              InitialValue = value,
              UpdatedValue = updated_value,

              Map0       = multi_key_map:new(record_info(fields, keys)),
              {ok, Map1} = multi_key_map:insert(Keys, InitialValue, Map0),

              Result1 = multi_key_map:update(#keys.key3, 30, UpdatedValue, Map1),
              ?assertMatch({ok, _}, Result1),
              {ok, Map2} = Result1,

              ?assertEqual([{Keys, UpdatedValue}], multi_key_map:to_list(Map2))
      end},
     {"キーに対応する要素が存在しない場合は、更新処理に失敗する",
      fun () ->
              Keys  = #keys{key1 = 10, key2 = 20, key3 = 30},
              InitialValue = value,
              UpdatedValue = updated_value,

              Map0       = multi_key_map:new(record_info(fields, keys)),
              {ok, Map1} = multi_key_map:insert(Keys, InitialValue, Map0),

              Result1 = multi_key_map:update(#keys.key3, 3000000, UpdatedValue, Map1),
              ?assertMatch(error, Result1)
      end}
    ].
