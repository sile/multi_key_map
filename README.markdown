multi_key_map
==============
**multi_key_map** は、一つの値に対して複数のキーが割り当て可能なマップの実装。

```erlang
% キーセットを定義
> rd(keyset, {key1, key2, key3}).

% マップ作成
> Map0 = multi_key_map:new(keyset, record_info(fields, keyset)).

% 要素追加: キーと値が多対一の関係
> {ok, Map1} = multi_key_map:insert(#keyset{key1 = 1, key2 = 2, key3 = 3}, value1, Map0).
> {ok, Map2} = multi_key_map:insert(#keyset{key1 = a, key2 = b, key3 = c}, value2, Map1).

% 要素検索: キーセットの内の任意のキーで検索が可能
> multi_key_map:find(#keyset.key1, 1, Map2).
{ok,#keyset{key1 = 1,key2 = 2,key3 = 3},value1}

> multi_key_map:find(#keyset.key2, b, Map2).
{ok,#keyset{key1 = a,key2 = b,key3 = c},value2}
```


インストール＆ビルド
-------------------
プロジェクト構成は [rebar](https://github.com/basho/rebar) に準拠。  
他のプロジェクトから使用する場合は、プロジェクトの **rebar.config** に以下の項目を追加する。
```bash
{deps,
  [
   {multi_key_map, ".*", {git, "git://github.com/sile/multi_key_map.git", "0.1.0"}}
  ]}.
```
単体でビルドする場合は、以下のコマンドを実行する。
```shell
$ git clone git://github.com/sile/multi_key_map.git
$ cd multi_key_map/
$ make
```


APIドキュメント
-----------------------
[edocのドキュメント](doc/README.md)を参照のこと。


ライセンス
---------
[MIT License](COPYING)
