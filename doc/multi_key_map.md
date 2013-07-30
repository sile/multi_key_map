

# Module multi_key_map #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-key">key()</a> ###



<pre><code>
key() = term()
</code></pre>





### <a name="type-keyset">keyset()</a> ###



<pre><code>
keyset() = tuple()
</code></pre>





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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#erase-3">erase/3</a></td><td></td></tr><tr><td valign="top"><a href="#find-3">find/3</a></td><td></td></tr><tr><td valign="top"><a href="#fold-3">fold/3</a></td><td></td></tr><tr><td valign="top"><a href="#foreach-2">foreach/2</a></td><td></td></tr><tr><td valign="top"><a href="#insert-3">insert/3</a></td><td></td></tr><tr><td valign="top"><a href="#is_multi_key_map-1">is_multi_key_map/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#update-4">update/4</a></td><td></td></tr></table>


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


<a name="is_multi_key_map-1"></a>

### is_multi_key_map/1 ###


<pre><code>
is_multi_key_map(Value::term()) -&gt; boolean()
</code></pre>

<br></br>



<a name="new-1"></a>

### new/1 ###


<pre><code>
new(KeySetRecordFields) -&gt; <a href="#type-map">map()</a>
</code></pre>

<ul class="definitions"><li><code>KeySetRecordFields = [atom()]</code></li></ul>


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



