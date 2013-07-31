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

is_multi_key_map_test_() ->
    [
     {"マップインスタンスの判定",
      fun () ->
              Map    = multi_key_map:new(record_info(fields, keys)),
              NotMap = {a, b, c},

              ?assert(multi_key_map:is_multi_key_map(Map)),
              ?assertNot(multi_key_map:is_multi_key_map(NotMap))
      end}
    ].

insert_test_() ->
    [
     {"要素を追加する",
      fun () ->
              Keys  = #keys{key1 = 10, key2 = 20, key3 = 30},
              Value = value,
              
              Map0    = multi_key_map:new(record_info(fields, keys)),
              ?assertEqual(0, multi_key_map:size(Map0)),

              Result0 = multi_key_map:insert(Keys, Value, Map0),
              ?assertMatch({ok, _}, Result0),
              {ok, Map1} = Result0,

              ?assertEqual(1, multi_key_map:size(Map1)),
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

find_test_() ->
    [
     {"要素をそれぞれのキーで検索する",
      fun () ->
              Keys1  = #keys{key1 = 10, key2 = 20, key3 = 30},
              Value1 = 40,
              Keys2  = #keys{key1 = aa, key2 = bb, key3 = cc},
              Value2 = dd,
              
              Map0       = multi_key_map:new(record_info(fields, keys)),
              {ok, Map1} = multi_key_map:insert(Keys1, Value1, Map0),
              {ok, Map2} = multi_key_map:insert(Keys2, Value2, Map1),

              ?assertEqual({ok, Keys1, 40}, multi_key_map:find(#keys.key1, 10, Map2)),
              ?assertEqual({ok, Keys1, 40}, multi_key_map:find(#keys.key2, 20, Map2)),
              ?assertEqual({ok, Keys2, dd}, multi_key_map:find(#keys.key3, cc, Map2))
      end},
     {"要素が見つからない場合",
      fun () ->
              Keys1  = #keys{key1 = 10, key2 = 20, key3 = 30},
              Value1 = 40,
              Keys2  = #keys{key1 = aa, key2 = bb, key3 = cc},
              Value2 = dd,
              
              Map0       = multi_key_map:new(record_info(fields, keys)),
              {ok, Map1} = multi_key_map:insert(Keys1, Value1, Map0),
              {ok, Map2} = multi_key_map:insert(Keys2, Value2, Map1),

              ?assertEqual(error, multi_key_map:find(#keys.key2, 10, Map2))
      end}
    ].

erase_test_() ->
    [
     {"要素を削除する",
      fun () ->
              Keys1  = #keys{key1 = 10, key2 = 20, key3 = 30},
              Value1 = 40,
              Keys2  = #keys{key1 = aa, key2 = bb, key3 = cc},
              Value2 = dd,
              
              Map0       = multi_key_map:new(record_info(fields, keys)),
              {ok, Map1} = multi_key_map:insert(Keys1, Value1, Map0),
              {ok, Map2} = multi_key_map:insert(Keys2, Value2, Map1),
              ?assertEqual(2, multi_key_map:size(Map2)),

              Map3 = multi_key_map:erase(#keys.key1, 10, Map2),
              ?assertEqual(1, multi_key_map:size(Map3)),

              Map4 = multi_key_map:erase(#keys.key3, cc, Map3),
              ?assertEqual(0, multi_key_map:size(Map4)),

              ?assertEqual(error, multi_key_map:find(#keys.key2, 20, Map4)),
              ?assertEqual(error, multi_key_map:find(#keys.key1, aa, Map4))
      end},
     {"存在しない要素を削除しようとしてもエラーにはならない",
      fun () ->
              Keys1  = #keys{key1 = 10, key2 = 20, key3 = 30},
              Value1 = 40,
              Keys2  = #keys{key1 = aa, key2 = bb, key3 = cc},
              Value2 = dd,
              
              Map0       = multi_key_map:new(record_info(fields, keys)),
              {ok, Map1} = multi_key_map:insert(Keys1, Value1, Map0),
              {ok, Map2} = multi_key_map:insert(Keys2, Value2, Map1),

              ?assertEqual(Map2, multi_key_map:erase(#keys.key1, abcdefg, Map2))
      end}
    ].

foreach_test_() ->
    [
     {"foreach",
      fun () ->
              Keys1  = #keys{key1 = 10, key2 = 20, key3 = 30},
              Value1 = 40,
              Keys2  = #keys{key1 = aa, key2 = bb, key3 = cc},
              Value2 = dd,
              
              Map0       = multi_key_map:new(record_info(fields, keys)),
              {ok, Map1} = multi_key_map:insert(Keys1, Value1, Map0),
              {ok, Map2} = multi_key_map:insert(Keys2, Value2, Map1),

              Parent = self(),
              Child  = spawn(fun () -> receive Msg1 -> receive Msg2 -> Parent ! {finish, Msg1, Msg2} end end end),
              multi_key_map:foreach(fun (Keys, Value) -> Child ! {Keys, Value} end,
                                    Map2),

              receive
                  {finish, Msg1, Msg2} ->
                      ?assertEqual(lists:sort([Msg1, Msg2]),
                                   lists:sort([{Keys1, Value1}, {Keys2, Value2}]))
              end
      end}
    ].

fold_test_() ->
    [
     {"foldを使って、全要素の値の合計を求める",
      fun () ->
              Keys1  = #keys{key1 = 10, key2 = 20, key3 = 30},
              Value1 = 40,
              Keys2  = #keys{key1 = aa, key2 = bb, key3 = cc},
              Value2 = 70,
              
              Map0       = multi_key_map:new(record_info(fields, keys)),
              {ok, Map1} = multi_key_map:insert(Keys1, Value1, Map0),
              {ok, Map2} = multi_key_map:insert(Keys2, Value2, Map1),

              Sum = multi_key_map:fold(fun (_, Value, Acc) -> Acc + Value end,
                                       0,
                                       Map2),
              ?assertEqual(110, Sum)
      end}
    ].
