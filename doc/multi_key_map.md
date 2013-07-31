

# Module multi_key_map #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


一つの値に対して複数のキーが割り当て可能なマップの実装.
Copyright (c) 2013 Takeru Ohta


__Authors:__ Takeru Ohta ([`phjgt308@gmail.com`](mailto:phjgt308@gmail.com)).
<a name="description"></a>

## Description ##


本モジュールの使用例に関しては[README](./README.md) も参照のこと.
<a name="types"></a>

## Data Types ##




### <a name="type-fold_fun">fold_fun()</a> ###



<pre><code>
fold_fun() = fun((<a href="#type-keyset">keyset()</a>, <a href="#type-value">value()</a>, Acc::term()) -&gt; AccNext::term())
</code></pre>





### <a name="type-foreach_fun">foreach_fun()</a> ###



<pre><code>
foreach_fun() = fun((<a href="#type-keyset">keyset()</a>, <a href="#type-value">value()</a>) -&gt; any())
</code></pre>





### <a name="type-key">key()</a> ###



<pre><code>
key() = term()
</code></pre>





### <a name="type-keyset">keyset()</a> ###



<pre><code>
keyset() = tuple()
</code></pre>



 キーセットを表現するレコード



### <a name="type-keyset_field">keyset_field()</a> ###



<pre><code>
keyset_field() = atom()
</code></pre>



 キーセット内のキー(フィールド)の名前



### <a name="type-keyset_field_index">keyset_field_index()</a> ###



<pre><code>
keyset_field_index() = non_neg_integer()
</code></pre>



 検索時等に使用するキーのインデックス. インデックスは `#RecordName.Field` 取得可能.



### <a name="type-keyset_name">keyset_name()</a> ###



<pre><code>
keyset_name() = atom()
</code></pre>



 キーセットの名前. キーセットを表現するレコードの名前に相当



### <a name="type-map">map()</a> ###



<pre><code>
map() = #'?MAP'{}
</code></pre>





### <a name="type-value">value()</a> ###



<pre><code>
value() = term()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#erase-3">erase/3</a></td><td>要素を削除する.</td></tr><tr><td valign="top"><a href="#find-3">find/3</a></td><td>要素を検索する.</td></tr><tr><td valign="top"><a href="#fold-3">fold/3</a></td><td>マップの要素の畳み込みを行う.</td></tr><tr><td valign="top"><a href="#foreach-2">foreach/2</a></td><td>マップの要素を走査し、各要素に引数の関数を適用する.</td></tr><tr><td valign="top"><a href="#insert-3">insert/3</a></td><td>要素を挿入する.</td></tr><tr><td valign="top"><a href="#is_multi_key_map-1">is_multi_key_map/1</a></td><td>引数の値が<code>multi_key_map</code>のインスタンスかどうかを判定する.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>マップインスタンスを生成する.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>マップに格納されている要素の数を取得する.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>マップをリストに変換する.</td></tr><tr><td valign="top"><a href="#update-4">update/4</a></td><td>要素の値を更新する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="erase-3"></a>

### erase/3 ###


<pre><code>
erase(KeyIndex::non_neg_integer(), Key::<a href="#type-key">key()</a>, Map::<a href="#type-map">map()</a>) -&gt; <a href="#type-map">map()</a>
</code></pre>

<br></br>



要素を削除する.


キーセット内の`FieldIndex`で指定されたフィールドの値が`Key`となる要素の値を削除する. 
<br></br>

対応する要素が存在しない場合は、単に引数で渡されたマップがそのまま返される.
<a name="find-3"></a>

### find/3 ###


<pre><code>
find(FieldIndex::<a href="#type-keyset_field_index">keyset_field_index()</a>, Key::<a href="#type-key">key()</a>, Map::<a href="#type-map">map()</a>) -&gt; {ok, <a href="#type-keyset">keyset()</a>, <a href="#type-value">value()</a>} | error
</code></pre>

<br></br>



要素を検索する.


キーセット内の`FieldIndex`で指定されたフィールドの値が`Key`となる要素を検索する. 
<br></br>

対応する要素が存在しない場合は`error`が返される.
<a name="fold-3"></a>

### fold/3 ###


<pre><code>
fold(Fun::<a href="#type-fold_fun">fold_fun()</a>, Initial::term(), Map::<a href="#type-map">map()</a>) -&gt; Result::term()
</code></pre>

<br></br>



マップの要素の畳み込みを行う.


要素の畳み込み順は未定義.
<a name="foreach-2"></a>

### foreach/2 ###


<pre><code>
foreach(Fun::<a href="#type-foreach_fun">foreach_fun()</a>, Map::<a href="#type-map">map()</a>) -&gt; ok
</code></pre>

<br></br>



マップの要素を走査し、各要素に引数の関数を適用する.


要素の走査順は未定義.
<a name="insert-3"></a>

### insert/3 ###


<pre><code>
insert(KeySet::<a href="#type-keyset">keyset()</a>, Value::<a href="#type-value">value()</a>, Map::<a href="#type-map">map()</a>) -&gt; {ok, <a href="#type-map">map()</a>} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Reason = {key_exists, <a href="#type-keyset_field">keyset_field()</a>, <a href="#type-key">key()</a>}</code></li></ul>


要素を挿入する.


キーセット内のキーのいずれかが、既存のマップに存在する場合は、要素の上書きは行われずに、挿入処理に失敗する.
<br></br>

その場合は`{error, {key_exists, 衝突したキーのフィールド名, 衝突したキー}}`が結果として返される. 
<br></br>


<br></br>

既に存在する要素の更新を行いたい場合は代わりに[`update/4`](#update-4)を使用すること. 
<br></br>


<br></br>

使用例:

```
  > rd(keyset, {key1, key2, key3}).
  > Map0 = multi_key_map:new(keyset, record_info(fields, keyset)).
  > {ok, Map1} = multi_key_map:insert(#keyset{key1 = a, key2 = b, key3 = c}, value1, Map0).
  > multi_key_map:to_list(Map1).
  [{#keyset{key1 = a,key2 = b,key3 = c},value1}]
```

<a name="is_multi_key_map-1"></a>

### is_multi_key_map/1 ###


<pre><code>
is_multi_key_map(Value::term()) -&gt; boolean()
</code></pre>

<br></br>


引数の値が`multi_key_map`のインスタンスかどうかを判定する.
<a name="new-2"></a>

### new/2 ###


<pre><code>
new(KeySetName, KeySetFields) -&gt; <a href="#type-map">map()</a>
</code></pre>

<ul class="definitions"><li><code>KeySetName = <a href="#type-keyset_name">keyset_name()</a></code></li><li><code>KeySetFields = [<a href="#type-keyset_field_index">keyset_field_index()</a>]</code></li></ul>


マップインスタンスを生成する.


`KeySetFields`は`record_info(fields, KeySetNameName)`を使って取得すること.

<br></br>

使用例:

```
  > rd(keyset, {key1, key2, key3}).
  > multi_key_map:new(keyset, record_info(fields, keyset)).
```

<a name="size-1"></a>

### size/1 ###


<pre><code>
size(Map::<a href="#type-map">map()</a>) -&gt; non_neg_integer()
</code></pre>

<br></br>


マップに格納されている要素の数を取得する.
<a name="to_list-1"></a>

### to_list/1 ###


<pre><code>
to_list(Map::<a href="#type-map">map()</a>) -&gt; [{<a href="#type-keyset">keyset()</a>, <a href="#type-value">value()</a>}]
</code></pre>

<br></br>


マップをリストに変換する.
<a name="update-4"></a>

### update/4 ###


<pre><code>
update(FieldIndex::<a href="#type-keyset_field_index">keyset_field_index()</a>, Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, Map::<a href="#type-map">map()</a>) -&gt; {ok, <a href="#type-map">map()</a>} | error
</code></pre>

<br></br>



要素の値を更新する.


キーセット内の`FieldIndex`で指定されたフィールドの値が`Key`となる要素の値を更新する. 
<br></br>

キーに対応する要素が存在しない場合は`error`が返される. 
<br></br>


<br></br>

使用例:

```
  > rd(keyset, {key1, key2, key3}).
  > Map0 = multi_key_map:new(keyset, record_info(fields, keyset)).
  > {ok, Map1} = multi_key_map:insert(#keyset{key1 = a, key2 = b, key3 = c}, value1, Map0).
  > {ok, Map2} = multi_key_map:update(#keyset.key2, b, value2, Map1).
  > multi_key_map:to_list(Map2).
  [{#keyset{key1 = a,key2 = b,key3 = c},value2}]
```

