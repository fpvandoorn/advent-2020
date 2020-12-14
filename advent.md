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

(* 16 *)

(* 17 *)

(* 18 *)

(* 19 *)

(* 20 *)

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
