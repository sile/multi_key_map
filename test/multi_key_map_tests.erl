%% @doc multi_key_map モジュールのテスト
-module(multi_key_map_tests).

-include_lib("eunit/include/eunit.hrl").

new_test_() ->
    [
     {"マップインスタンスが生成できる",
      fun () ->
              IndexNames = [id1, id2, id3],
              Map = multi_key_map:new(IndexNames),
              ?assert(multi_key_map:is_multi_key_map(Map))
      end},
     {"インデックスを一つも持たないマップは生成できない",
      fun () ->
              IndexNames = [],
              ?assertError(_, multi_key_map:new(IndexNames))
      end}
    ].

store_test_() ->
    [
     {"要素を追加する",
      fun () ->
              IndexNames = [id1, id2, id3],
              Map0 = multi_key_map:new(IndexNames),
              Map1 = multi_key_map:store([{id1, 10}, {id2, 20}, {id3, 30}], 60, Map0),

              ?assertEqual(1, multi_key_map:size(Map1)),
              ?assertEqual({ok, 60}, multi_key_map:find(id1, 10, Map1)),
              ?assertEqual({ok, 60}, multi_key_map:find(id2, 20, Map1)),
              ?assertEqual({ok, 60}, multi_key_map:find(id3, 30, Map1))
      end}
    ].
