let exampleInput = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6";

let inputStr = "jmp +27
acc +32
acc +10
acc +23
jmp +88
acc +46
acc -3
jmp +209
acc +1
jmp +159
acc +29
jmp +328
acc +44
jmp +14
acc +14
jmp +557
nop +127
acc +34
nop +227
jmp +214
jmp +512
jmp +78
jmp +544
acc +14
acc +5
acc -11
jmp +291
acc +5
nop +115
jmp +166
acc +2
jmp +1
jmp +500
jmp +244
jmp +186
acc +43
acc +26
jmp +502
acc +14
nop +63
jmp +115
acc -11
nop +153
acc +3
nop +107
jmp +468
acc -6
acc +38
acc +0
jmp +102
acc +27
acc -9
acc +45
jmp +186
nop +457
acc +14
jmp +483
nop +35
acc +27
jmp +202
jmp -55
jmp +1
acc +33
acc -2
acc +5
jmp +296
acc +17
acc +11
acc +36
nop +11
jmp +10
acc +20
nop +115
acc +37
jmp +284
acc +39
acc +40
jmp +1
jmp +1
jmp +233
acc +42
acc +27
jmp +1
nop +189
jmp +329
jmp +118
acc +13
jmp -82
acc +18
acc -1
acc +23
jmp +104
acc +25
acc +5
acc +49
jmp +274
acc +35
jmp +1
jmp +45
acc -1
jmp +128
jmp +110
acc +11
acc +48
nop +316
acc -15
jmp +150
nop +396
acc +19
acc +15
jmp +129
acc +17
acc +42
jmp +1
acc +13
jmp +333
nop -24
acc +5
acc -9
acc -14
jmp +129
acc +14
nop +486
acc -4
jmp +274
jmp +269
acc +0
acc +36
acc +8
jmp -102
acc -3
acc +18
jmp +162
acc +16
acc +26
nop +313
acc +9
jmp -26
acc +16
jmp +383
acc +10
jmp +245
jmp +119
jmp -57
acc +17
jmp +75
acc +13
jmp +452
acc -13
acc -13
jmp -115
acc +18
jmp +97
acc +0
jmp -28
acc +43
jmp +401
acc -17
jmp +91
acc +16
acc +22
acc +42
jmp +244
nop +376
acc +36
acc +20
acc +32
jmp -157
acc -6
acc +33
jmp +295
jmp -20
acc -2
acc +7
jmp +305
nop -76
acc +18
acc +24
jmp +89
acc -8
acc -1
jmp +171
acc +40
acc +11
acc +15
acc +43
jmp +234
jmp +1
acc +45
nop +343
jmp -140
acc +40
acc -6
acc +35
jmp +67
acc -5
acc +2
acc +32
acc +32
jmp +199
acc +40
acc +19
jmp +337
acc -1
acc -14
acc +34
jmp +266
nop +265
acc -1
acc +31
jmp +151
jmp -206
acc +49
acc +24
acc -16
jmp -82
jmp -117
nop +238
acc -10
jmp +150
acc +26
nop -95
acc +21
jmp +59
nop -13
acc +45
acc +45
jmp +350
jmp +285
acc +28
acc +31
acc +6
jmp -106
jmp +294
nop -142
acc +13
nop +347
acc +43
jmp +79
acc +7
jmp +368
acc +35
acc +1
acc +4
jmp +355
acc -10
jmp -175
acc +35
jmp -3
acc -2
acc +35
acc +33
acc +34
jmp -154
acc +27
jmp +131
acc -18
jmp +74
acc -14
nop +173
jmp +79
nop -82
acc +26
acc -4
nop -237
jmp +270
jmp +118
acc +0
acc +34
jmp -212
nop -59
jmp -150
acc +26
jmp +224
jmp +1
acc -18
jmp +85
nop -134
acc +6
jmp -136
acc +4
jmp -246
acc +9
acc +24
jmp -105
nop +99
acc -13
acc -15
nop +286
jmp -187
jmp -276
acc -14
acc -12
jmp +148
acc -18
jmp -254
acc +23
acc -10
acc +32
acc +49
jmp +39
acc -10
acc +10
acc -17
acc +39
jmp +19
jmp +236
jmp -205
acc +0
acc +5
acc -15
jmp +41
acc +28
acc -18
nop -20
jmp -175
jmp +23
acc +36
nop +198
jmp +223
jmp +1
nop -60
acc +28
jmp +118
acc +12
acc +9
jmp +159
nop +176
nop +11
acc -1
jmp +183
acc -6
acc +16
jmp -43
acc -17
nop +222
acc -4
jmp +1
jmp -21
acc +43
acc +42
acc -2
acc +12
jmp +168
acc +10
acc +38
nop -159
jmp +94
acc +5
acc -1
jmp -317
jmp -294
jmp +42
acc +11
acc +38
acc +27
acc +0
jmp -63
jmp -57
acc +23
jmp -111
nop +1
acc -12
jmp -91
acc +22
acc -1
nop -163
jmp +1
jmp -165
acc -12
acc -7
acc -9
acc +37
jmp +82
acc -10
acc +29
acc +0
nop +200
jmp -129
acc +13
acc +33
jmp -33
acc +27
jmp -172
jmp +57
jmp -234
jmp -141
acc +35
nop +202
acc -6
jmp +51
acc +10
jmp -8
jmp -291
acc +36
acc +25
jmp -263
jmp +211
acc +21
acc -7
acc -6
nop -222
jmp -247
acc -8
acc +29
jmp -21
acc +0
jmp -256
jmp +1
acc +37
nop +55
acc +40
jmp -266
acc +17
jmp +200
jmp +1
acc +7
acc +10
acc +24
jmp -6
acc +8
jmp -104
nop -64
acc +3
nop -391
acc +26
jmp +6
acc +12
acc -9
nop +110
jmp -420
jmp -411
nop -273
nop -287
acc +39
jmp +117
jmp -119
acc +38
jmp +119
acc +0
jmp -430
acc -14
jmp -231
acc +26
acc +1
acc -13
acc +15
jmp -208
jmp +1
acc +50
jmp -263
acc +14
jmp +1
acc +31
jmp -13
nop -334
nop +76
nop -435
nop -52
jmp +131
nop +53
acc +19
nop -213
acc +5
jmp -338
acc +48
acc +22
acc +43
acc +1
jmp -377
acc +38
jmp -268
nop -269
acc +20
acc +6
nop -395
jmp -415
jmp +1
jmp -398
acc -12
acc -10
acc -18
jmp +1
jmp +94
jmp -358
jmp -313
acc +12
acc +20
acc -13
jmp -110
acc +28
acc +12
acc +42
acc +43
jmp +101
acc -14
jmp -6
acc +25
acc -7
acc +5
jmp -420
acc -4
jmp -89
acc -17
nop -499
jmp -379
nop -395
acc +37
acc +30
acc +5
jmp -25
jmp +63
jmp +71
acc -3
jmp -24
jmp -117
acc -6
jmp +1
acc +26
nop -212
jmp -498
jmp -395
jmp -210
acc +44
acc +12
acc +21
jmp +40
acc +43
jmp -382
nop -509
acc -17
jmp -111
jmp -16
acc +31
jmp -306
jmp -22
acc +50
acc +47
jmp -398
nop -300
jmp -246
jmp +49
acc +0
acc +12
acc +7
nop -6
jmp -109
acc -19
acc +21
acc -19
nop -355
jmp -418
jmp -245
acc +50
jmp +1
nop -3
jmp -177
acc +29
acc +40
acc -15
nop -123
jmp -305
nop -313
acc -3
acc +50
jmp -530
jmp -398
acc +16
acc +29
nop -358
acc +37
jmp -165
jmp -193
jmp -132
acc +21
jmp -355
jmp -450
jmp -456
acc +25
acc +49
acc +50
acc +0
jmp -60
acc +5
acc -15
jmp -565
acc +10
acc -9
acc -3
jmp -220
acc +44
acc -10
jmp -70
acc -17
jmp -174
jmp -168
acc +6
acc +35
jmp -133
acc -12
acc +41
acc +41
jmp -580
acc +45
acc +27
acc +12
acc +0
jmp -24
acc +35
nop -507
nop -27
nop -456
jmp -379
jmp -222
acc +6
acc +43
acc -9
acc +45
jmp +1";

type instruction =
  | Acc(int)
  | Jmp(int)
  | Nop(int);

type program = {
  accumulator: int,
  instructions: array(instruction),
  pos: int,
};

type execResult =
  | Running(program)
  | Complete(program);

type runResult =
  | InfiniteLoop(int)
  | Success(int);

exception InvalidInstruction(string);

let stringToInstruction = (instruction: string): instruction => {
  let re = [%re "/(\\w+)\\s([+-])(\\d+)/"];
  switch (instruction |> Js.Re.exec_(re)) {
  | Some(result) =>
    switch (result |> Js.Re.captures |> Array.map(Js.Nullable.toOption)) {
    | [|_, Some("nop"), Some("+"), Some(value)|] =>
      Nop(int_of_string(value))
    | [|_, Some("nop"), Some("-"), Some(value)|] =>
      Nop((-1) * int_of_string(value))
    | [|_, Some("acc"), Some("+"), Some(value)|] =>
      Acc(int_of_string(value))
    | [|_, Some("acc"), Some("-"), Some(value)|] =>
      Acc((-1) * int_of_string(value))
    | [|_, Some("jmp"), Some("+"), Some(value)|] =>
      Jmp(int_of_string(value))
    | [|_, Some("jmp"), Some("-"), Some(value)|] =>
      Jmp((-1) * int_of_string(value))
    | _ => raise(InvalidInstruction(instruction))
    }
  | None => raise(InvalidInstruction(instruction))
  };
};

let parseInput = (input: string): program => {
  let lines =
    input |> Js.String.split("\n") |> Array.map(stringToInstruction);

  {accumulator: 0, instructions: lines, pos: 0};
};

let step = (~program: program): execResult =>
  if (program.pos >= Array.length(program.instructions)) {
    Complete(program);
  } else {
    let instruction = program.instructions[program.pos];
    switch (instruction) {
    | Acc(value) =>
      Running({
        ...program,
        accumulator: program.accumulator + value,
        pos: program.pos + 1,
      })
    | Jmp(value) => Running({...program, pos: program.pos + value})
    | Nop(value) => Running({...program, pos: program.pos + 1})
    };
  };

let program = parseInput(exampleInput);

let rec runUntilRepeatInstruction =
        (
          ~seen: Belt.Set.Int.t=Belt.Set.Int.fromArray([||]),
          result: execResult,
        ) => {
  switch (result) {
  | Complete(program) => Success(program.accumulator)
  | Running(program) =>
    seen->Belt.Set.Int.has(program.pos)
      ? InfiniteLoop(program.accumulator)
      : runUntilRepeatInstruction(
          ~seen=seen->Belt.Set.Int.add(program.pos),
          step(~program),
        )
  };
};

let day1 = () => {
  exception NoInfiniteLoop(int);

  let seen = Belt.Set.Int.fromArray([||]);
  let program = parseInput(inputStr);
  switch (runUntilRepeatInstruction(~seen, Running(program))) {
  | Success(value) => raise(NoInfiniteLoop(value))
  | InfiniteLoop(value) => value
  };
};

Js.log2("Day 1:", day1());

let instructionAtPos = (program: program, pos: int) =>
  if (pos >= Array.length(program.instructions)) {
    None;
  } else {
    Some(program.instructions[pos]);
  };

let swapInstruction = (~program: program, pos: int) => {
  let updatedInstruction =
    switch (program.instructions[pos]) {
    | Nop(value) => Jmp(value)
    | Jmp(value) => Nop(value)
    | instruction => instruction
    };
  let instructions = program.instructions |> Array.copy;

  instructions[pos] = updatedInstruction;
  {...program, instructions};
};

let rec findInstructionToSwap = (pos: int, program: program, exec: execResult) => {
  switch (exec) {
  | Complete(p) => p.accumulator
  | Running(p) =>
    switch (runUntilRepeatInstruction(Running(p))) {
    | Success(value) => value
    | InfiniteLoop(_) =>
      let swapped = swapInstruction(~program, pos);
      findInstructionToSwap(pos + 1, program, Running(swapped));
    }
  };
};

let day2 = () => {
  let program = parseInput(inputStr);

  findInstructionToSwap(0, program, Running(program));
};

Js.log2("Day 2:", day2());
