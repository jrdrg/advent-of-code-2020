let exampleInput = "16
10
15
5
1
11
7
19
6
12
4";

let example2 = "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3";

let inputStr = "151
94
14
118
25
143
33
23
80
95
87
44
150
39
148
51
138
121
70
69
90
155
144
40
77
8
97
45
152
58
65
63
128
101
31
112
140
86
30
55
104
135
115
16
26
60
96
85
84
48
4
131
54
52
139
76
91
46
15
17
37
156
134
98
83
111
72
34
7
108
149
116
32
110
47
157
75
13
10
145
1
127
41
53
2
3
117
71
109
105
64
27
38
59
24
20
124
9
66";

let parseInput = (input: string) => {
  input |> Js.String.split("\n") |> Array.to_list |> List.map(int_of_string);
};

let inputToSet = (input: list(int)) => {
  Belt.Set.Int.fromArray(input |> Array.of_list);
};

let rec chainAdapters =
        (~chains: list(list(int))=[[0]], set: Belt.Set.Int.t) => {
  switch (chains) {
  | [] => []
  | [[head, ..._restChain] as chain, ..._rest]
      // +1 because of the extra 0
      when List.length(chain) === Belt.Set.Int.size(set) + 1 => [
      head + 3,
      ...chain,
    ]
  | [[head, ..._restChain] as chain, ...restChains] =>
    let available =
      set
      ->Belt.Set.Int.keep(v => v > head && v <= head + 3)
      ->Belt.Set.Int.toArray;

    let nextChains = available |> Array.map(start => [start, ...chain]);
    chainAdapters(~chains=Array.to_list(nextChains) @ restChains, set);
  | _ => []
  };
};

let rec findDifferences =
        (
          ~diffs: Belt.Map.Int.t(int)=Belt.Map.Int.fromArray([||]),
          chain: list(int),
        ) => {
  switch (chain) {
  | [head, next, ...rest] =>
    let diff = head - next;
    let updatedDiffs =
      diffs->Belt.Map.Int.update(diff, v => {
        switch (v) {
        | Some(value) => Some(value + 1)
        | None => Some(1)
        }
      });
    findDifferences(~diffs=updatedDiffs, [next, ...rest]);
  | _ => diffs
  };
};

let day1 = () => {
  let input = parseInput(inputStr);
  let set = input |> inputToSet;
  let chain = chainAdapters(set);
  let diffs = findDifferences(chain);

  let result = Belt.Map.Int.getExn(diffs, 1) * Belt.Map.Int.getExn(diffs, 3);
  result;
};

Js.log2("Day 1:", day1());

let rec findAllValidChains =
        (
          ~chains: list(list(int))=[[0]],
          ~valid: int=0,
          set: Belt.Set.Int.t,
        ) => {
  switch (chains) {
  | [] => valid
  | [[head, ..._restChain] as chain, ...restChains] =>
    let available =
      set
      ->Belt.Set.Int.keep(v => v > head && v <= head + 3)
      ->Belt.Set.Int.toList;

    let valid =
      if (List.length(available) === 0) {
        Js.log2("valid", valid + 1);
        valid + 1;
      } else {
        valid;
      };

    let nextChains = available |> List.map(start => [start, ...chain]);
    findAllValidChains(~chains=nextChains @ restChains, ~valid, set);
  | _ => 0
  };
};

let day2 = () => {
  let input = parseInput(inputStr);
  let set = input |> inputToSet;

  Js.log2("set> ", set->Belt.Set.Int.toArray);

  let validChains = findAllValidChains(set);

  Js.log2("valid", validChains);
  validChains;
};

Js.log2("Day 2:", day2());
