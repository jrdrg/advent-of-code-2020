let exampleStr = "939
7,13,x,x,59,x,31,19";

let inputStr = "1006697
13,x,x,41,x,x,x,x,x,x,x,x,x,641,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,29,x,661,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,23";

type departure = {
  timestamp: float,
  buses: list(option(int)),
};

let parseInput = input => {
  exception InvalidInput(string);
  let lines = input |> Js.String.split("\n");
  switch (lines) {
  | [|timestamp, buses|] => {
      timestamp: float_of_string(timestamp),
      buses:
        buses
        |> Js.String.split(",")
        |> Array.to_list
        |> List.map(i => {
             switch (i) {
             | "x" => None
             | bus => Some(int_of_string(bus))
             }
           }),
    }
  | _ => raise(InvalidInput(input))
  };
};

let canLeave = (timestamp: float, bus: int) => {
  let fDiv = timestamp /. float_of_int(bus);
  let iDiv = floor(fDiv);
  fDiv === iDiv;
};

let rec findEarliestBus = ({timestamp, buses}: departure) => {
  let foundBus =
    buses
    |> List.find_opt(bus => {
         switch (bus) {
         | Some(b) => canLeave(timestamp, b)
         | None => false
         }
       });
  switch (foundBus) {
  | Some(Some(bus)) => (bus, timestamp)
  | _ => findEarliestBus({timestamp: timestamp +. 1., buses})
  };
};

let day1 = () => {
  let input = parseInput(inputStr);

  Js.log(input);
  let (bus, timestamp) = findEarliestBus(input);
  float_of_int(bus) *. (timestamp -. input.timestamp);
};

Js.log2("Day 1:", day1());

let rec firstBus = (buses: list(option(int))) => {
  exception NoBus;
  switch (buses) {
  | [Some(bus), ..._rest] => bus
  | [_head, ...rest] => firstBus(rest)
  | _ => raise(NoBus)
  };
};

let rec findSequentialDepartures =
        (timestamp: float, buses: list(option(int))) => {
  let first = firstBus(buses);

  let isValid =
    buses
    |> List.mapi((index, bus) => (index, bus))
    |> List.fold_left(
         (valid, (index, entry)) => {
           switch (entry) {
           | Some(bus) =>
             let validBus = canLeave(timestamp +. float_of_int(index), bus);
             valid && validBus;
           | None => valid
           }
         },
         true,
       );
  if (isValid) {
    timestamp;
  } else {
    findSequentialDepartures(timestamp +. float_of_int(first), buses);
  };
};

let day2 = () => {
  let input = parseInput(inputStr);

  findSequentialDepartures(100000000000000., input.buses);
};

Js.log2("Day 2:", day2());
