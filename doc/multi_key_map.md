

# Module multi_key_map #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


一つの値に対して複数のキーが割り当て可能なマップの実装.


<a name="types"></a>

## Data Types ##




### <a name="type-fold_fun">fold_fun()</a> ###



<pre><code>
fold_fun() = fun((<a href="#type-keyset">keyset()</a>, <a href="#type-value">value()</a>, Acc::term()) -&gt; AccNext::term())
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#erase-3">erase/3</a></td><td></td></tr><tr><td valign="top"><a href="#find-3">find/3</a></td><td></td></tr><tr><td valign="top"><a href="#fold-3">fold/3</a></td><td></td></tr><tr><td valign="top"><a href="#foreach-2">foreach/2</a></td><td></td></tr><tr><td valign="top"><a href="#insert-3">insert/3</a></td><td>要素を挿入する.</td></tr><tr><td valign="top"><a href="#is_multi_key_map-1">is_multi_key_map/1</a></td><td>引数の値が<code>multi_key_map</code>のインスタンスかどうかを判定する.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>マップインスタンスを生成する.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#update-4">update/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="erase-3"></a>

### erase/3 ###


<pre><code>
erase(KeyIndex::non_neg_integer(), Key::<a href="#type-key">key()</a>, Map::<a href="#type-map">map()</a>) -&gt; <a href="#type-map">map()</a>
</code></pre>

<br></br>



<a name="find-3"></a>

### find/3 ###


<pre><code>
find(KeyIndex::non_neg_integer(), Key::<a href="#type-key">key()</a>, Map::<a href="#type-map">map()</a>) -&gt; {ok, <a href="#type-keyset">keyset()</a>, <a href="#type-value">value()</a>} | error
</code></pre>

<br></br>



<a name="fold-3"></a>

### fold/3 ###


<pre><code>
fold(Fun, Initial::<a href="#type-value">value()</a>, Map::<a href="#type-map">map()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Fun = fun((<a href="#type-keyset">keyset()</a>, <a href="#type-value">value()</a>, Acc) -&gt; NextAcc)</code></li><li><code>Acc = term()</code></li><li><code>NextAcc = term()</code></li><li><code>Result = term()</code></li></ul>


<a name="foreach-2"></a>

### foreach/2 ###


<pre><code>
foreach(Fun, Map::<a href="#type-map">map()</a>) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Fun = fun((<a href="#type-keyset">keyset()</a>, <a href="#type-value">value()</a>) -&gt; any())</code></li></ul>


<a name="insert-3"></a>

### insert/3 ###


<pre><code>
insert(KeySet::<a href="#type-keyset">keyset()</a>, Value::<a href="#type-value">value()</a>, Map::<a href="#type-map">map()</a>) -&gt; {ok, <a href="#type-map">map()</a>} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Reason = {key_exists, atom(), Key::<a href="#type-key">key()</a>}</code></li></ul>

要素を挿入する.

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
<a name="size-1"></a>

### size/1 ###


<pre><code>
size(Map::<a href="#type-map">map()</a>) -&gt; non_neg_integer()
</code></pre>

<br></br>



<a name="to_list-1"></a>

### to_list/1 ###


<pre><code>
to_list(Map::<a href="#type-map">map()</a>) -&gt; [{<a href="#type-keyset">keyset()</a>, <a href="#type-value">value()</a>}]
</code></pre>

<br></br>



<a name="update-4"></a>

### update/4 ###


<pre><code>
update(KeyIndex::non_neg_integer(), Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, Map::<a href="#type-map">map()</a>) -&gt; {ok, <a href="#type-map">map()</a>} | error
</code></pre>

<br></br>



