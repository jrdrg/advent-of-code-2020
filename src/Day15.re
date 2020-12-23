let exampleStr = "0,3,6";

let inputStr = "0,8,15,2,12,1,4";

let parseInput = input =>
  input |> Js.String.split(",") |> Array.map(int_of_string) |> Array.to_list;

let rec turn =
        (
          ~counts: Belt.Map.Int.t(list(int))=Belt.Map.Int.fromArray([||]),
          ~current: int=1,
          ~targetTurn: int,
          ~lastSpoken: int=0,
          numbers: list(int),
        ) => {
  exception InvalidNumbers;

  switch (numbers) {
  | [] => raise(InvalidNumbers)
  | [head, ...rest] =>
    let (numbers, spoken) =
      if (current <= List.length(numbers)) {
        (rest @ [head], head);
      } else {
        let updatedCounts =
          switch (Belt.Map.Int.get(counts, lastSpoken)) {
          | Some([last, prev, ..._rest]) => last - prev
          | Some([_last]) => 0
          | None => 0
          | Some(x) =>
            Js.log3("?????", x |> Array.of_list, head);
            raise(InvalidNumbers);
          };
        (numbers, updatedCounts);
      };
    if (current === targetTurn) {
      spoken;
    } else {
      let counts =
        counts->Belt.Map.Int.update(spoken, v =>
          switch (v) {
          | Some(turns) => Some([current, ...turns])
          | None => Some([current])
          }
        );
      turn(
        ~counts,
        ~current=current + 1,
        ~targetTurn,
        ~lastSpoken=spoken,
        numbers,
      );
    };
  };
};

let day1 = () => {
  let input = parseInput(inputStr);
  let counts = input |> turn(~targetTurn=2020);

  counts;
};

Js.log2("Day 1:", day1());

let day2 = () => {
  let input = parseInput(inputStr);
  let counts = input |> turn(~targetTurn=30000000);

  counts;
};

Js.log2("Day 2:", day2());
