let between = (low, high, value) => {
  value <= high && value >= low;
};

let parseInput = (process, input: string) => {
  input |> Js.String.split("\n") |> Array.map(process);
};

let isEmptyList = list => {
  switch (list) {
  | [] => true
  | _ => false
  };
};
