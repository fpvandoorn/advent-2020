```mathematica
(* 1 *) 
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["1.txt"];
{p1a, p1b} = 
 Times @@@ 
  Select[Subsets[ToExpression@StringSplit@str, 3], Total@# == 2020 &]

(* 2 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["2.txt"];
data = MapAt[ToExpression, #, {{1}, {2}}] & /@ 
   StringSplit[StringSplit[str, {"\n"}], {": ", " ", "-"}];
p2a = Count[data, _?(#1 <= StringCount[#4, #3] <= #2 & @@ # &)]
p2b = Count[
  data, _?(Count[StringTake[#4, {{#1}, {#2}}], #3] == 1 & @@ # &)]

(* 3 *)
SetDirectory["D:\\projects\\advent-2020\\input\\input"];
str = Import["3.txt"];
trees = Map[If[# == "#", 1, 0] &, #, {2}] &[
   Characters@StringSplit[str, "\n"]];
width = Length[trees[[1]]];
height = Length[trees];
counttrees[right_, down_] := Module[{x = 1,
   ntrees = 0},
  For[i = 1, i <= height, i += down; x = Mod[x + right, width, 1], 
   ntrees += trees[[i, x]]]; ntrees]
p3a = counttrees[3, 1]
p3b = counttrees[1, 1] counttrees[3, 1] counttrees[5, 1] counttrees[7,
    1] counttrees[1, 2]

(* 4 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["4.txt"];
mandatory = Sort@{"eyr", "hgt", "pid", "ecl", "byr", "hcl", "iyr"};
eyecolors = {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"};
hex = (CharacterRange["0", "9"] | CharacterRange["a", "f"]);
p4a = Count[
  StringSplit[str, 
   "\n\n"], _?(SubsetQ[
      StringTake[StringCases[#, _ ~~ _ ~~ _ ~~ ":"], 3], 
      mandatory] &)]
check[pass_] :=
 Length[#] == 7 \[And] (1920 <= ToExpression@#1 <= 2002 \[And]
        MemberQ[eyecolors, #2] \[And]
        2020 <= ToExpression@#3 <= 2030 \[And]
        StringMatchQ[#4, "#" ~~ Repeated[hex, {6}]] \[And]
        ((StringTake[#5, -2] == "cm" \[And] 
            150 <= ToExpression@StringDrop[#5, -2] <= 193) \[Or]
          (StringTake[#5, -2] == "in" \[And] 
            59 <= ToExpression@StringDrop[#5, -2] <= 76)) \[And]
        2010 <= ToExpression@#6 <= 2020 \[And]
        StringMatchQ[#7, Repeated[CharacterRange["0", "9"], {9}]] & @@
       StringDrop[#, 4]) &@
  Select[Sort@StringSplit[pass], ! StringMatchQ[#, "cid" ~~ ___] &]
p4b = Count[StringSplit[str, "\n\n"], _?(check[#] &)]

(* 5 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["5.txt"];
seats = FromDigits[#, 
    2] & /@ (Characters@StringSplit[str] /. {"B" -> 1, "L" -> 0, 
     "F" -> 0, "R" -> 1}); p5a = Max[seats]
min = Min[seats];
Complement[Range[min, p5a], seats]

(* 6 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["6.txt"];
p6a = Total[
  Length /@ (Union /@ 
     Characters[
      StringReplace[StringSplit[str, "\n\n"], {"\n" -> ""}]])]
p6b = Total[
  Length /@ (Intersection @@@ 
     Characters@StringSplit@StringSplit[str, "\n\n"])]

(* 7 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["7.txt"];
data1 = StringSplit[
   StringSplit[
    StringReplace[str, {" bags", " bag", "no other", "."} -> ""], 
    "\n"], {" contain ", ", "}];
test = Tally@
   StringTake[
    Flatten[data1[[All, 
      2 ;; -1]]], {2}];(*check: every number is a single digit*)
data = {#1, {ToExpression@StringTake[#, 1], 
        StringDrop[#, 2]} & /@ {##2}} & @@@ data1;
dict = #[[All, 2]] & /@ 
   GroupBy[Flatten[(a \[Function] a -> #1) /@ ##2[[All, 2]] & @@@ 
      data], First];
start = "shiny gold";
visited = {start};
todo = {start};
While[todo =!= {},
 current = First@todo;
 todo = Rest@todo;
 new = Complement[
   If[(current /. dict) === current, {}, current /. dict], visited];
 visited = visited~Join~new;
 todo = todo~Join~new
 ]
p7a = Length@visited - 1
dictb = Rule @@@ data;
count[bag_] := 
 count[bag] = Total[#1 (count[#2] + 1) & @@@ (bag /. dictb)]
p7b = count@start

(* 8 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["8.txt"];
instr = MapAt[ToExpression, #, 2] & /@ 
   StringSplit@StringSplit[str, "\n"];
i = 1;
visited = {};
acc = 0;
While[! MemberQ[visited, i],
  AppendTo[visited, i];
  in = instr[[i]];
  If[in[[1]] === "acc", acc += in[[2]]];
  If[in[[1]] === "jmp", i += in[[2]], i++]];
p8a = acc
n = Length@instr;
remaining = Range@n;
looping = {};
halting = {};
While[remaining =!= {},
 i = First@remaining;
 current = {};
 While[! MemberQ[current, i] && MemberQ[remaining, i],
  AppendTo[current, i];
  in = instr[[i]];
  If[in[[1]] === "jmp", i += in[[2]], i++]];
 If[i > n || MemberQ[halting, i], halting = Union[halting, current], 
  looping = Union[looping, current]];
 remaining = Complement[remaining, current]]
i = 1;
visited = {};
acc = 0;
canflip = True;
While[i <= n && ! MemberQ[visited, i],
  AppendTo[visited, i];
  in = instr[[i]];
  If[in[[1]] === "acc",
   acc += in[[2]]; i++,
   If[canflip,
    flipd = If[in[[1]] === "jmp", 1, in[[2]]];
    If[MemberQ[halting, i + flipd],
     i += flipd; canflip = False; Continue[]]];
   i += If[in[[1]] === "jmp", in[[2]], 1]];
  ];
If[i == n + 1, p8b = acc, Print["No flippable instruction found"]]

(* 9 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["9.txt"];
n = 25;
list = ToExpression /@ StringSplit@str;
For[i = n + 1, i <= Length@list, i++,
 x = list[[i]];
 l = list[[i - n ;; i - 1]];
 If[Intersection[l, x - l] === {}, Break[]]]
p9a = list[[i]]
sums = {};
For[i = 1, i <= Length@list, i++,
 x = list[[i]];
 sums = {#1 + x, Min[#2, x], Max[#3, x]} & @@@ sums;
 p = Position[sums[[All, 1]], p9a];
 If[p =!= {}, p9b = sums[[p[[1, 1]], 2]] + sums[[p[[1, 1]], 3]]];
 AppendTo[sums, {x, x, x}]]
p9b
p9b

(* 10 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["10.txt"];
list = ToExpression@StringSplit@str;
dict = Rule @@@ Tally@Differences@Union[list, {0}];
p10a = (1 /. dict) ((3 /. dict) + 1)
slist = Sort@list;
temp = {{0, 1}};
For[i = 1, i <= Length@list, i++,
  value = slist[[i]];
  temp = Select[temp, #[[1]] + 3 >= value &];
  AppendTo[temp, {value, Total@temp[[All, 2]]}]
  ];
p10b = temp[[-1, 2]]

(* 11 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["11.txt"];
data = Characters /@ StringSplit[str];
n = Length[data];
m = Length[data[[1]]];

cnt[data_] := (Count[
    Flatten[data[[Max[#2[[1]] - 1, 1] ;; Min[#2[[1]] + 1, n], 
      Max[#2[[2]] - 1, 1] ;; Min[#2[[2]] + 1, m]]]], "#"] &)
next[data_] := 
  MapIndexed[
   If[#1 === "L" && cnt[data][##] == 0, "#", 
     If[#1 === "#" && cnt[data][##] >= 5, "L", #1]] &, data, {2}];
prev = data;
nxt = next[prev];
While[nxt =!= prev, prev = nxt; nxt = next[prev]];
p11a = Count[Flatten@prev, "#"]
directions = 
 Select[Join @@ 
   Table[{ii, jj}, {ii, -1, 1}, {jj, -1, 1}], # =!= {0, 0} &]
cntb[data_] := (Count[(v \[Function] (k = #2 + v; 
        While[1 <= k[[1]] <= n && 1 <= k[[2]] <= m, 
         If[Extract[data, k] === "#", Return[1], 
          If[Extract[data, k] === "L", Return[0], k += v]]]; 
        Return[0])) /@ directions, Return[1]] &)
nextb[data_] := 
  MapIndexed[
   If[#1 === "L" && cntb[data][##] == 0, "#", 
     If[#1 === "#" && cntb[data][##] >= 5, "L", #1]] &, data, {2}];
prev = data;
nxt = nextb[prev];
While[nxt =!= prev, prev = nxt; nxt = nextb[prev]];
p11b = Count[Flatten@prev, "#"]

(* 12 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["12.txt"];
data = {StringTake[#, 1], ToExpression@StringDrop[#, 1]} & /@ 
   StringSplit@str;
dirs = {0 -> {1, 0}, 90 -> {0, 1}, 180 -> {-1, 0}, 270 -> {0, -1}, 
   "E" -> {1, 0}, "N" -> {0, 1}, "W" -> {-1, 0}, "S" -> {0, -1}};
dir = 0;
pos = {0, 0};
For[i = 1, i <= Length@data, i++,
 instr = data[[i]];
 If[instr[[1]] == "L", dir += instr[[2]],
  If[instr[[1]] == "R", dir -= instr[[2]],
   pos += ((instr[[1]] /. {"F" -> Mod[dir, 360]}) /. dirs)*instr[[2]]
   ]]]
p12a = Abs[pos[[1]]] + Abs[pos[[2]]]
dirs = {0 -> {1, 0}, 90 -> {0, 1}, 180 -> {-1, 0}, 270 -> {0, -1}, 
   "E" -> {1, 0}, "N" -> {0, 1}, "W" -> {-1, 0}, "S" -> {0, -1}};
shippos = {0, 0};
wppos = {10, 1};(*relative to the ship*)
For[i = 1, i <= Length@data, i++,
 instr = data[[i]];
 If[instr[[1]] == "L", wppos = RotationMatrix[instr[[2]] Degree].wppos,
  If[instr[[1]] == "R", 
   wppos = RotationMatrix[-instr[[2]] Degree].wppos,
   If[instr[[1]] == "F", shippos += wppos*instr[[2]],
    wppos += (instr[[1]] /. dirs)*instr[[2]]
    ]]]]
p12b = Abs[shippos[[1]]] + Abs[shippos[[2]]]

(* 13 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["13.txt"];
data = StringSplit[str, {"\n", ","}];
data2 = ToExpression /@ Select[data, # =!= "x" &];
time = data2[[1]];
data3 = Rest@data2;
waittimes = Mod[-time, #] & /@ data3;
leastpos = Ordering[waittimes, 1][[1]];
p13a = Min[waittimes]*data3[[leastpos]]
datab = ToExpression@
   Select[MapIndexed[{#, 1 - #2[[1]]} &, 
     data[[2 ;; -1]]], #[[1]] =!= "x" &];
p13b = ChineseRemainder @@ Reverse@Transpose@datab

(* 14 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["14.txt"];
data = StringSplit[
   StringSplit[str, "\n"], {"mem[", "] = ", "mask = "}];
dataWithInstructionsDeleted = 
  Reverse@DeleteDuplicates[Reverse@data, 
    First[#1] == First[#2] &];(*delete duplicate writes*)
UniqueInstructions = 
  Join @@ ((a \[Function] {#[[1]], ToExpression@a}) /@ Rest@# & /@ 
     Split[dataWithInstructionsDeleted, Length@#2 == 2 &]);
p14a = (inf \[Function] 
     FromDigits[
      If[#1 == "X", #2, ToExpression@#1] & @@@ 
       Transpose@{Characters@inf[[1, 1]], 
         PadLeft[IntegerDigits[inf[[2, 2]], 2], 36]}, 2]) /@ 
   UniqueInstructions // Total
InstructionsB = 
  Join @@ ((a \[Function] {#[[1]], ToExpression@a}) /@ Rest@# & /@ 
     Split[data, Length@#2 == 2 &]);
AllMemoryWrites = 
  Join @@ ((inf \[Function] {#, 
          inf[[2, 2]]} & /@ (FromDigits[
           If[#1 == "1", 1, If[#1 == "X", 0, #2]] & @@@ 
            Transpose@{Characters@inf[[1, 1]], 
              PadLeft[IntegerDigits[inf[[2, 1]], 2], 36]}, 
           2] + (Total /@ 
            Subsets[
             2^(36 - StringPosition[inf[[1, 1]], "X"][[All, 1]])]))) /@
      InstructionsB);
p14b = Total@
  DeleteDuplicatesBy[Reverse[AllMemoryWrites], First][[All, 2]]

(* 15 *)
str = "20,9,11,0,1,2";
data = Reverse@ToExpression@StringSplit[str, ","];
n = 2020;
it = n - Length@data;
data2 = data;
For[i = 1, i <= it, i++,
  PrependTo[data2, 
   If[MemberQ[Rest@data2, data2[[1]]], 
    Position[data2, data2[[1]], 1, 2][[-1, 1]] - 1, 0]]];
p15a = data2[[1]]
n = 30000000;
assoc = Association@MapIndexed[#1 -> #2[[1]] &, Most@Reverse@data];
value = First@data;
Timing@For[i = Length@data, i < n, i++,
  p = assoc[value];
  AppendTo[assoc, {value -> i}];
  value = If[MatchQ[p, Missing[_, _]], 0, i - p]]
p15b = value

(* 16 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["16.txt"];
data = StringSplit[str, "\n\n"];
ranges = ToExpression@
   StringSplit[
    Flatten@StringSplit[
       StringSplit[data[[1]], "\n"], {": ", " or "}][[All, 2 ;; -1]], 
    "-"];
ranges2 = {#1, ToExpression@StringSplit[#2, "-"], 
     ToExpression@StringSplit[#3, "-"]} & @@@ 
   StringSplit[StringSplit[data[[1]], "\n"], {": ", " or "}];
numbers = ToExpression@Rest@StringSplit[data[[3]], {"\n", ","}];
invalid = 
  Select[numbers, ! (Or @@ ((a \[Function] a[[1]] <= # <= a[[2]]) /@ 
         ranges)) &];
p16a = Total@invalid
othertickets = 
  ToExpression@StringSplit[Rest@StringSplit[data[[3]], {"\n"}], ","];
validtickets = 
  Select[othertickets, Intersection[#, invalid] === {} &];
range = ranges2[[All, {2, 3}]];
poss = {};
foo = Transpose@validtickets;
For[i = 1, i <= Length@foo, i++,
 thisfoo = foo[[i]];
 AppendTo[poss, 
  Position[((And @@ ((a \[Function] #[[1, 1]] <= 
               a <= #[[1, 2]] \[Or] #[[2, 1]] <= a <= #[[2, 2]]) /@ 
           thisfoo)) & /@ range), True][[All, 1]]]]
truepos = 
  Select[#, 
     a \[Function] ! 
       MemberQ[Flatten@
         Select[poss, b \[Function] Length@b < Length@#], a]] & /@ 
   poss;
deppos = Position[truepos[[All, 1]], _?(# <= 6 &)];
myticket = ToExpression@Rest@StringSplit[data[[2]], {"\n", ","}];
p16b = Times @@ Extract[myticket, deppos]

(* 17 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["17.txt"];
data = Characters /@ StringSplit[str, "\n"];
n = 6;
h = Length@data;
w = Length@data[[1]];
data2 = PadLeft[
   PadRight[{data}, {1 + n, h + n, w + n}, "."], {1 + 2 n, h + 2 n, 
    w + 2 n}, "."];
directions = 
  Select[Flatten[
    Table[{ii, jj, kk}, {ii, -1, 1}, {jj, -1, 1}, {kk, -1, 1}], 
    2], # =!= {0, 0, 0} &];
cnt[data_] := (Count[
    Flatten[data[[Max[#2[[1]] - 1, 1] ;; Min[#2[[1]] + 1, 1 + 2 n], 
      Max[#2[[2]] - 1, 1] ;; Min[#2[[2]] + 1, h + 2 n], 
      Max[#2[[3]] - 1, 1] ;; Min[#2[[3]] + 1, w + 2 n]]]], "#"] &)
next[data_] := 
  MapIndexed[
   If[#1 === "#" && 3 <= cnt[data][##] <= 4, "#", 
     If[#1 === "." && cnt[data][##] == 3, "#", "."]] &, data, {3}];
result = Nest[next, data2, 6];
p17a = Count[Flatten@result, "#"]
data = Characters /@ StringSplit[str, "\n"];
n = 6;
h = Length@data;
w = Length@data[[1]];
data2 = PadLeft[
   PadRight[{{data}}, {1 + n, 1 + n, h + n, w + n}, "."], {1 + 2 n, 
    1 + 2 n, h + 2 n, w + 2 n}, "."];
directions = 
  Select[Flatten[
    Table[{ii, jj, kk, ll}, {ii, -1, 1}, {jj, -1, 1}, {kk, -1, 
      1}, {ll, -1, 1}], 2], # =!= {0, 0, 0, 0} &];
cnt[data_] := (Count[
    Flatten[data[[Max[#2[[1]] - 1, 1] ;; Min[#2[[1]] + 1, 1 + 2 n], 
      Max[#2[[2]] - 1, 1] ;; Min[#2[[2]] + 1, 1 + 2 n], 
      Max[#2[[3]] - 1, 1] ;; Min[#2[[3]] + 1, h + 2 n], 
      Max[#2[[4]] - 1, 1] ;; Min[#2[[4]] + 1, w + 2 n]]]], "#"] &)
next[data_] := 
  MapIndexed[
   If[#1 === "#" && 3 <= cnt[data][##] <= 4, "#", 
     If[#1 === "." && cnt[data][##] == 3, "#", "."]] &, data, {4}];
result = Nest[next, data2, 6];
p17b = Count[Flatten@result, "#"]

(* 18 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["18.txt"];
data = StringSplit[StringReplace[str, " " -> ""], "\n"];
compute := Module[{j = consumeN}, infix[j]]
consumeN := 
 Module[{pos}, 
  If[StringTake[expr, 1] === "(", expr = StringDrop[expr, 1]; compute,
    j = ToExpression@StringTake[expr, 1]; expr = StringDrop[expr, 1]; 
   j]]
infix[j_] := 
 If[expr === "", j, 
  Module[{t = StringTake[expr, 1], k}, expr = StringDrop[expr, 1]; 
   If[t === ")", j, k = consumeN; infix[If[t === "+", j + k, j k]]]]]
eval := ((expr = #; compute) &)
p18a = Total[eval /@ data]
8929569623593
data2 = StringReplace[#, {"+" -> 
       "\[CircleTimes]", \!\(TraditionalForm\`"\<*\>" -> "\<\
\[CirclePlus]\>"\)}] & /@ data;
p18b = Total[
  ToExpression[data2] /. {CirclePlus -> Times, CircleTimes -> Plus}]

(* 19 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["19.txt"];
{rules, msgs} = StringSplit[str, "\n\n"];
rules2 = Sort[
    MapAt[ToExpression, #, 1] & /@ 
     StringSplit[StringSplit[rules, "\n"], ": "]][[All, 2]];
rules3 = Map[If[# =!= "|", ToExpression@#, #] &, 
   StringSplit /@ rules2, {2}];
rules4 = Rest@rules3;
Clear[interpret]
interpret[ruleslist_, rule_] := 
 interpret[ruleslist, rule] = 
  StringJoin @@ (If[MemberQ[rule, "|"], 
       Prepend[Append[#, ")"], "("], #] &[
     If[StringQ@#, #, interpret[ruleslist, ruleslist[[#]]]] & /@ rule])
regex = RegularExpression@interpret[rules4, rules3[[1]]];
p19a = Length@Select[StringSplit@msgs, StringMatchQ[#, regex] &]
n = 7;
rules5 = ReplacePart[
   rules4, {8 -> {"(", 42, ")+"}, 
    11 -> Flatten@
      Riffle[Array[
        ConstantArray[42, #]~Join~ConstantArray[31, #] &, {n}], "|"]}];
regexb = RegularExpression@interpret[rules5, rules3[[1]]];
p19b = Length@Select[StringSplit@msgs, StringMatchQ[#, regexb] &]

(* 20 *)
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["20.txt"];
{numbers, grids} = 
  Transpose@
   StringSplit[StringSplit[StringReplace[str, "Tile " -> ""], "\n\n"],
     ":\n"];
grids2 = Characters@StringSplit[grids, "\n"];
numbers2 = ToExpression@numbers;
sides = Sort[{#, Reverse@#}][[1]] & /@ {#[[All, 1]], #[[All, -1]], #[[
       1]], #[[-1]]} & /@ grids2;
sides2 = Flatten[sides, 1];
pos = {#} & /@ 
   Select[Tally@
      Ceiling[Position[sides2, _?(Count[sides2, #] == 1 &)][[All, 1]]/
        4], #[[2]] == 2 &][[All, 1]];
p20a = Times @@ Extract[numbers2, pos]
adj = Position[
      sides, _?(a \[Function] Length@Intersection[a, #] == 1), {1}, 
      Heads -> False][[All, 1]] & /@ sides;
addelement[pos_, nr_] := (AppendTo[table, pos -> nr]; 
  AppendTo[done, nr])
table = {};
done = {};
addelement[{1, 1}, Position[adj, _?(Length@# == 2 &), 1][[1, 1]]];
addelement[{1, 2}, adj[[done[[1]], 2]]];
addelement[{2, 1}, adj[[done[[1]], 1]]];
width = Sqrt@Length@grids;
For[i = 2, i <= width, i++,
  lasttwo = Extract[adj, {#} & /@ table[[{-2, -1}, 2]]];
  addelement[{i, 2}, Complement[Intersection @@ lasttwo, done][[1]]];
  addelement[{i + 1, 1}, #] & /@ 
   Complement[lasttwo[[2]], lasttwo[[1]]]];
For[j = 3, j <= width, j++,
 For[i = 1, i <= width, i++,
  addelement[{i, j}, #] & /@ 
   Complement[adj[[{i, j - 1} /. table]], done]]]

positions = Normal@SparseArray[table];

MatrixForm[grids3 = MapIndexed[
    ({i, j} = #2;
      If[i < width,
       nr = 
        Position[
          
          sides[[#]], _?(MemberQ[
              sides[[positions[[i + 1, j]]]], #] &)][[1, 1]];
       Switch[nr, 1, Reverse@Transpose@#, 2, Transpose@#, 3, 
          Reverse@#, 4, #] &[grids2[[#]]],
       nr = 
        Position[
          sides[[#]], _?(MemberQ[
              sides[[positions[[i - 1, j]]]], #] &)][[1, 1]];
       Switch[nr, 1, Transpose@#, 2, Reverse@Transpose@#, 3, #, 4, 
          Reverse@#] &[grids2[[#]]]]) &, positions, {2}]];

MatrixForm[grids4 = MapIndexed[
    ({i, j} = #2;
      If[j < width,
       If[
        MemberQ[Transpose@grids3[[i, j + 1, All, {1, -1}]], #[[
          All, -1]]], #, Reverse /@ #],
       If[
        MemberQ[Transpose@grids3[[i, j - 1, All, {1, -1}]], #[[
          All, -1]]], Reverse /@ #, #]]) &, grids3, {2}]];
grids5 = grids4[[All, All, 2 ;; -2, 2 ;; -2]];
image = ArrayFlatten@grids5;
monsterstr = "                  # 
  #    ##    ##    ###
   #  #  #  #  #  #   ";
monster = Characters@StringSplit[monsterstr, "\n"];
mh = Length@monster;
mw = Length@monster[[1]];
monster2 = Flatten@monster;
image = Reverse /@ Reverse@Transpose@ArrayFlatten@grids5;
matches = {};
For[i = 1, i + mh - 1 <= Length@image, i++,
 For[j = 1, j + mw - 1 <= Length@image[[1]], j++,
  If[And @@ (If[#1 == "#", #2 == "#", True] & @@@ 
      Transpose[{monster2, 
        Flatten@image[[i ;; i + mh - 1, j ;; j + mw - 1]]}]),
   AppendTo[matches, {i, j}]]]]
image2 = image;
MapIndexed[({a, i} \[Function] 
      If[a === "#", image2[[# + i[[1]] - 1, #2 + i[[2]] - 1]] = "O"]),
     monster, {2}] & @@@ matches;
p20b = Count[Flatten@image2, "#"]

(* 21 *)

(* 22 *)

(* 23 *)

(* 24 *)

(* 25 *)

(* output *)
SetDirectory["D:\\projects\\advent-2020"];
txt = First@
   FrontEndExecute[
    FrontEnd`ExportPacket[NotebookGet@InputNotebook[], "InputText"]];
txt = "```mathematica\n" <> 
   StringReplace[StringReplace[txt, "\r\n" -> "\n"], 
    "\n(*" -> "\n\n(*"] <> "\n```\n";
Export["advent.md", txt, "Text"];
```
