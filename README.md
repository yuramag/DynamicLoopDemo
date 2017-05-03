<h1>Iterating Over Dynamic Number of Nested Loops</h1>

<h2>Introduction</h2>

<p>Sometimes, you need to iterate over an unknown number of <code>for</code> or <code>foreach</code> loops. For example, let&#39;s say we have the following <code>collection of collections of strings</code>:</p>

<pre lang="cs">
var data = new List&lt;List&lt;string&gt;&gt;{
    new List&lt;string&gt;{&quot;red&quot;, &quot;green&quot;, &quot;blue&quot;},
    new List&lt;string&gt;{&quot;apple&quot;, &quot;banana&quot;, &quot;peach&quot;, &quot;mellon&quot;},
    new List&lt;string&gt;{&quot;one&quot;, &quot;two&quot;}
};</pre>

<p>And we need to generate the following sequence (all possible ordered combinations):</p>

<ul class="list">
	<li><code>red-apple-one </code></li>
	<li><code>red-apple-two </code></li>
	<li><code>red-banana-one </code></li>
	<li><code>red-banana-two </code></li>
	<li><code>red-peach-one </code></li>
	<li><code>red-peach-two </code></li>
	<li><code>red-mellon-one </code></li>
	<li><code>red-mellon-two </code></li>
	<li><code>green-apple-one </code></li>
	<li><code>green-apple-two </code></li>
	<li><code>green-banana-one </code></li>
	<li><code>green-banana-two </code></li>
	<li><code>green-peach-one </code></li>
	<li><code>green-peach-two </code></li>
	<li><code>green-mellon-one </code></li>
	<li><code>green-mellon-two </code></li>
	<li><code>blue-apple-one </code></li>
	<li><code>blue-apple-two </code></li>
	<li><code>blue-banana-one </code></li>
	<li><code>blue-banana-two </code></li>
	<li><code>blue-peach-one </code></li>
	<li><code>blue-peach-two </code></li>
	<li><code>blue-mellon-one </code></li>
	<li><code>blue-mellon-two</code></li>
</ul>

<p>This can be achieved by the following<font color="#990000" face="Consolas"> </font>C# code:</p>

<pre lang="cs">
for(var x = 0; x &lt; data[0].Count; x++)
    for(var y = 0; y &lt; data[1].Count; y++)
        for(var z = 0; z &lt; data[2].Count; z++)
            Console.WriteLine(&quot;{0}-{1}-{2}&quot;, data[0][x], data[1][y], data[2][z]);</pre>

<p>As you can see, we had to execute 3 nested loops in order to access information we need. What if the <code>data</code> collection had more sub-collections? We would have to code as many nested loops as many sub-collections contained in the parent <code>data</code> collection. Below is an example that addresses this issue and allows iterating over any number of nested sub-collections producing the same result as shown above.</p>

<h2>Solution</h2>

<p>The idea is to maintain 2 arrays of integers (<code>bounds</code> and <code>counters</code>) of size <code>N</code>, where <code>N</code> is the number of nested collections. The <code>bounds</code> array will be populated with sizes of all nested collections while the <code>counters</code> array will be initialized with zeros and will represent current state (iteration indexes) of all nested loops. In the <code>while loop</code>, we will keep incrementing the counter at <code>loopIndex</code> position till it hits the bound. When the bound is hit, we will increment adjacent counter (<code>loopIndex - 1</code>) while setting current one to zero to start it over, and so on.</p>

<h2>C# Implementation</h2>

<pre lang="cs">
private static void Main()
{
    var data = new List&lt;List&lt;string&gt;&gt;
    {
        new List&lt;string&gt; {&quot;red&quot;, &quot;green&quot;, &quot;blue&quot;},
        new List&lt;string&gt; {&quot;apple&quot;, &quot;banana&quot;, &quot;peach&quot;, &quot;mellon&quot;},
        new List&lt;string&gt; {&quot;one&quot;, &quot;two&quot;}
    };

    foreach (var item in IterateDynamicLoop(data).Select(x =&gt; string.Join(&quot;-&quot;, x)))
        Console.WriteLine(item);

    Console.ReadLine();
}

public static IEnumerable&lt;IEnumerable&lt;T&gt;&gt; IterateDynamicLoop&lt;T&gt;(IList&lt;List&lt;T&gt;&gt; data)
{
    var count = data.Count;

    var loopIndex = count - 1;
    var counters = new int[count];
    var bounds = data.Select(x =&gt; x.Count).ToArray();

    do
    {
        yield return Enumerable.Range(0, count).Select(x =&gt; data[x][counters[x]]);
    } while (IncrementLoopState(counters, bounds, ref loopIndex));
}

private static bool IncrementLoopState(IList&lt;int&gt; counters, IList&lt;int&gt; bounds, ref int loopIndex)
{
    if (loopIndex &lt; 0)
        return false;

    counters[loopIndex] = counters[loopIndex] + 1;

    var result = true;

    if (counters[loopIndex] &gt;= bounds[loopIndex])
    {
        counters[loopIndex] = 0;
        loopIndex--;
        result = IncrementLoopState(counters, bounds, ref loopIndex);
        loopIndex++;
    }

    return result;
}</pre>

<h2>Delphi Implementation</h2>

<pre lang="text">
program DynamicLoopDemo;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Variants;

type
  TDynStringArray = array of string;

function IncrementLoopState(var ACounters, ABounds: array of Integer; var ALoopIndex: Integer): Boolean;
begin
  Result := False;
  if ALoopIndex &lt; 0 then
    Exit;

  ACounters[ALoopIndex] := ACounters[ALoopIndex] + 1;

  if ACounters[ALoopIndex] &gt;= ABounds[ALoopIndex] then
  begin
    ACounters[ALoopIndex] := 0;
    Dec(ALoopIndex);
    Result := IncrementLoopState(ACounters, ABounds, ALoopIndex);
    Inc(ALoopIndex);
  end
  else
    Result := True;
end;

procedure IterateDynamicLoop(AList: TStrings; AData: Variant);
var
  I, lMaxCount, lLoopIndex: Integer;
  lCounters, lBounds: array of Integer;
  lItem: string;
begin
  lMaxCount := VarArrayHighBound(AData, 1) + 1;
  lLoopIndex := lMaxCount - 1;
  SetLength(lCounters, lMaxCount);
  SetLength(lBounds, lMaxCount);

  for I := 0 to lMaxCount - 1 do
  begin
    lCounters[I] := 0;
    lBounds[I] := VarArrayHighBound(AData[I], 1) + 1;
  end;

  repeat
    lItem := &#39;&#39;;
    for I := VarArrayLowBound(AData, 1) to VarArrayHighBound(AData, 1) do
    begin
      if lItem &lt;&gt; &#39;&#39; then
        lItem := lItem + &#39;-&#39;;
      lItem := lItem + AData[I][lCounters[I]];
    end;
    AList.Add(lItem);
  until not IncrementLoopState(lCounters, lBounds, lLoopIndex);
end;

procedure RunDemo;
var
  lData: Variant;
  lList: TStrings;
  I: Integer;
begin
  lData := VarArrayOf([
    VarArrayOf([&#39;red&#39;, &#39;green&#39;, &#39;blue&#39;]),
    VarArrayOf([&#39;apple&#39;, &#39;banana&#39;, &#39;peach&#39;, &#39;mellon&#39;]),
    VarArrayOf([&#39;one&#39;, &#39;two&#39;])]);

  lList := TStringList.Create;
  try
    IterateDynamicLoop(lList, lData);
    for I := 0 to lList.Count - 1 do
      WriteLn(lList[I]);
  finally
    lList.Free;
  end;
end;

begin
  RunDemo;
  ReadLn;
end.</pre>
