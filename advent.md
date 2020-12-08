```mathematica
# 1 
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["1.txt"];
{p1a, p1b} = 
 Times @@@ 
  Select[Subsets[ToExpression@StringSplit@str, 3], Total@# == 2020 &]
# 2
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["2.txt"];
data = MapAt[ToExpression, #, {{1}, {2}}] & /@ 
   StringSplit[StringSplit[str, {"\n"}], {": ", " ", "-"}];
p2a = Count[data, _?(#1 <= StringCount[#4, #3] <= #2 & @@ # &)]
p2b = Count[
  data, _?(Count[StringTake[#4, {{#1}, {#2}}], #3] == 1 & @@ # &)]
# 3
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
# 4
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
# 5
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["5.txt"];
seats = FromDigits[#, 
    2] & /@ (Characters@StringSplit[str] /. {"B" -> 1, "L" -> 0, 
     "F" -> 0, "R" -> 1}); p5a = Max[seats]
min = Min[seats];
Complement[Range[min, p5a], seats]
# 6
SetDirectory["D:\\projects\\advent-2020\\input"];
str = Import["6.txt"];
p6a = Total[
  Length /@ (Union /@ 
     Characters[
      StringReplace[StringSplit[str, "\n\n"], {"\n" -> ""}]])]
p6b = Total[
  Length /@ (Intersection @@@ 
     Characters@StringSplit@StringSplit[str, "\n\n"])]
# 7
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
# 8
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
# 9
# 10
# 11
# 12
# 13
# 14
# 15
# 16
# 17
# 18
# 19
# 20
# 21
# 22
# 23
# 24
# 25
# output
SetDirectory["D:\\projects\\advent-2020"];
txt = First@
   FrontEndExecute[
    FrontEnd`ExportPacket[NotebookGet@InputNotebook[], "InputText"]];
txt = "```mathematica\n" <> 
   StringReplace[txt, {"\r\n" -> "\n", "\n#" -> "\n\n#"}] <> "\n```\n";
Export["advent.md", txt, "Text"];
```
