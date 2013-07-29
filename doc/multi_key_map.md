

# Module multi_key_map #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


マルチキーマップの実装.


<a name="types"></a>

## Data Types ##




### <a name="type-index_name">index_name()</a> ###



<pre><code>
index_name() = term()
</code></pre>





### <a name="type-key">key()</a> ###



<pre><code>
key() = term()
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#erase-3">erase/3</a></td><td></td></tr><tr><td valign="top"><a href="#find-3">find/3</a></td><td></td></tr><tr><td valign="top"><a href="#fold-3">fold/3</a></td><td></td></tr><tr><td valign="top"><a href="#make-1">make/1</a></td><td></td></tr><tr><td valign="top"><a href="#store-3">store/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="erase-3"></a>

### erase/3 ###


<pre><code>
erase(IndexName::<a href="#type-index_name">index_name()</a>, Key::<a href="#type-key">key()</a>, Map::<a href="#type-map">map()</a>) -&gt; <a href="#type-map">map()</a>
</code></pre>

<br></br>



<a name="find-3"></a>

### find/3 ###


<pre><code>
find(IndexName::<a href="#type-index_name">index_name()</a>, Key::<a href="#type-key">key()</a>, Map::<a href="#type-map">map()</a>) -&gt; error | {ok, <a href="#type-value">value()</a>}
</code></pre>

<br></br>



<a name="fold-3"></a>

### fold/3 ###


<pre><code>
fold(FoldFun, InitValue, Map::<a href="#type-map">map()</a>) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>FoldFun = fun(([{<a href="#type-index_name">index_name()</a>, <a href="#type-key">key()</a>}], <a href="#type-value">value()</a>, Acc) -&gt; Acc)</code></li><li><code>InitValue = term()</code></li><li><code>Acc = term()</code></li><li><code>Result = term()</code></li></ul>


<a name="make-1"></a>

### make/1 ###


<pre><code>
make(IndexNames::[<a href="#type-index_name">index_name()</a>]) -&gt; <a href="#type-map">map()</a>
</code></pre>

<br></br>



<a name="store-3"></a>

### store/3 ###


<pre><code>
store(Keys, Value::<a href="#type-value">value()</a>, Map::<a href="#type-map">map()</a>) -&gt; <a href="#type-map">map()</a>
</code></pre>

<ul class="definitions"><li><code>Keys = [{<a href="#type-index_name">index_name()</a>, <a href="#type-key">key()</a>}]</code></li></ul>


