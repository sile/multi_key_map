

# multi_key_map #

Copyright (c) Takeru Ohta <phjgt308@gmail.com>

__Version:__ 0.1.0


一つの値に対して複数のキーが割り当て可能なマップの実装.

使用するキーセットは事前にレコードとして定義しておき、マップへの要素追加時には、そのレコードのインスタンスを要素のキーとして使用する.
要素の検索や削除時には、そのキーセットレコードの内の任意のフィールドを、操作対象要素のキーとして使用することができる.

<h5><a name="使用例:">使用例:</a></h5>


```
## 準備
> rd(keyset, {key1, key2, key3}).
> Map0 = multi_key_map:new(keyset, record_info(fields, keyset)).

## 要素追加
> {ok, Map1} = multi_key_map:insert(#keyset{key1 = 1, key2 = 2, key3 = 3}, value1, Map0).
> {ok, Map2} = multi_key_map:insert(#keyset{key1 = a, key2 = b, key3 = c}, value2, Map1).

## 要素検索: keyset内の任意のキーで検索が可能
> multi_key_map:find(#keyset.key1, 1, Map2).
{ok,#keyset{key1 = 1,key2 = 2,key3 = 3},value1}

> multi_key_map:find(#keyset.key2, b, Map2).
{ok,#keyset{key1 = a,key2 = b,key3 = c},value2}

## 要素更新: insert/3 は既に存在する要素の更新は行えない (キーセット内の一つでも、既存の要素と重複する場合は挿入に失敗する)
> {error, {key_exists, key2, b}} = multi_key_map:insert(#keyset{key1 = new_a, key2 = b, key3 = new_c}, value3, Map2).

> {ok, Map3} = multi_key_map:update(#keyset.key2, b, value3, Map2).
> multi_key_map:find(#keyset.key3, c, Map3).
{ok,#keyset{key1 = a,key2 = b,key3 = c},value3}
```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="multi_key_map.md" class="module">multi_key_map</a></td></tr></table>

