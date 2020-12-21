let exampleStr = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0";

let exampleStr2 = "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1";

let inputStr = "mask = 001X10110010XXX1011X10X0X010110011X0
mem[7813] = 131
mem[54447] = 69257
mem[4649] = 764452
mem[19188] = 31285
mem[12729] = 12561
mem[53924] = 61854625
mem[30643] = 31622
mask = X10X010101101011X011X0X1010XX0001101
mem[65231] = 6306247
mem[9606] = 842
mem[35435] = 39105817
mask = 00X010010XX01001011011111010010X10X0
mem[15228] = 58105116
mem[57434] = 3463765
mem[65279] = 4039619
mem[21972] = 185592
mem[23182] = 637258
mem[26506] = 21618105
mem[59419] = 1185
mask = 11100X1X1X1X100110X110011X1001X01100
mem[41224] = 182354525
mem[63664] = 11139
mem[59848] = 465555194
mem[7441] = 426
mask = 0000X0XX100010X111101000100011010X00
mem[32794] = 26498404
mem[36528] = 4609841
mem[52372] = 928374411
mem[50746] = 13530768
mask = X0X0011X01101XX1101X00X1110X00111011
mem[23705] = 4702
mem[53987] = 260925767
mem[38759] = 107180723
mem[4145] = 1388225
mask = 01100X1X00111X0100XX10001X1X11X11011
mem[21659] = 48254
mem[47784] = 579
mem[15339] = 3634
mem[26981] = 17056
mem[56944] = 7553
mem[54362] = 25509
mask = 11100011111X1X0X10010010X01X01X01100
mem[36538] = 720281
mem[40349] = 4553
mem[38747] = 121580
mem[47936] = 853
mem[64464] = 14729
mem[13543] = 7412
mask = 0110X0X10X111001001010011001011XXXXX
mem[59367] = 1805959
mem[35408] = 1062861636
mem[2438] = 104177
mem[45204] = 49297768
mask = 11110111X1X110X11111000010110X101X01
mem[43520] = 50829
mem[52879] = 6973
mem[34130] = 1858472
mem[30580] = 116283566
mem[59200] = 77309650
mem[39793] = 19368
mem[21462] = 484824
mask = 0111X001100011X11X0000001X1010XX101X
mem[48944] = 713350
mem[15050] = 125604
mem[62553] = 4265286
mem[32331] = 207
mask = 00101X010X00X00110X0010X00000X010000
mem[37193] = 24490589
mem[47114] = 24972867
mem[19137] = 85518698
mem[28948] = 15755
mem[63089] = 207050481
mask = 0X001000XX10100X111010X011XX1000X100
mem[21622] = 12726803
mem[65486] = 259190646
mem[9456] = 367
mem[57461] = 22120
mem[47219] = 461
mem[42381] = 905567
mask = 011XX001100011X110X0X00X001X010X0010
mem[41554] = 246921
mem[47818] = 12679687
mem[48892] = 139693999
mask = 00001X0XX01010XX11X010101100X00011XX
mem[15036] = 387380
mem[4946] = 2220181
mem[3330] = 3484229
mem[60601] = 708273
mem[30390] = 22847
mask = X11010110X11100X0010X00X0001X0X1100X
mem[26506] = 48168688
mem[39792] = 1782856
mem[35767] = 30354
mem[3782] = 3439
mem[25399] = 804
mask = X01X10X10X0X1001001X0X0100X100001100
mem[36728] = 830047900
mem[39236] = 454
mem[35767] = 4930
mask = X1XX101010XX1001101X0X0000X000001100
mem[26504] = 38
mem[26618] = 6509202
mem[55528] = 49356209
mem[39373] = 427885078
mem[6797] = 35727
mem[60494] = 11588408
mask = 0X1000X1001X100100100000X010X01X1001
mem[28964] = 3377598
mem[37600] = 15038
mem[54440] = 13014026
mem[1872] = 54034
mask = 010000101X00X0X1101000011X1000X11001
mem[2672] = 38529642
mem[4612] = 12358076
mem[54440] = 698
mem[11542] = 158875347
mem[29683] = 130676
mem[58914] = 13086566
mask = 001XX001X100X0101X10100001100011X11X
mem[61472] = 33350
mem[22694] = 1012806073
mem[23462] = 118427580
mask = 0000100110001001XX100111110X100X0100
mem[33458] = 194706
mem[6488] = 4762211
mem[55925] = 577
mem[53312] = 691832
mem[33982] = 74563572
mem[54155] = 673882
mem[10046] = 455259
mask = 00X0X001X0001001X1100000000X1X00X00X
mem[65279] = 18926
mem[8787] = 404263
mem[6780] = 192
mem[57364] = 7578011
mem[36592] = 1320217
mask = 01001010X110100X10111101000X10X00110
mem[10414] = 28478
mem[8303] = 850886106
mem[23434] = 109585
mem[59687] = 55963101
mem[26357] = 149399655
mask = 011000010X01100100100X01000X000X1000
mem[32249] = 957872254
mem[37097] = 2407
mem[38976] = 7487
mask = 0X10000110001XXX1X100X000X10X100X010
mem[59096] = 256975
mem[57558] = 187691607
mem[28499] = 50844685
mem[21980] = 413222
mem[9178] = 224473274
mask = 011010XX00X010010110010000110X000100
mem[28499] = 13442626
mem[13668] = 767
mem[65204] = 1294
mem[29824] = 761
mem[7353] = 107008372
mask = 010010X0111X10011010110X000XXX01010X
mem[31019] = 2529
mem[25249] = 531972
mem[7705] = 1198443
mem[18755] = 1091
mask = 0100101001X0100X101110010XXX01X01011
mem[4011] = 170
mem[55626] = 1616
mem[64402] = 1736957
mem[59367] = 1198
mask = 00100001X01X10011110X111110110000X00
mem[16640] = 927178
mem[62784] = 134127
mem[52017] = 209325301
mem[62522] = 310138953
mem[26793] = 913812410
mem[13893] = 16876
mask = X000X010X11X100110101000111010001X01
mem[62349] = 548
mem[35033] = 529
mem[55681] = 98474
mem[21671] = 462821
mem[24184] = 15122039
mask = 011000010X10100X0110100010100000X0X1
mem[64651] = 19198
mem[65318] = 860
mem[30180] = 43025
mem[33280] = 216670269
mask = 0X00XX1X011010X1101110X0X0X1X0001000
mem[30267] = 3237
mem[28430] = 2022016
mem[10792] = 19348
mem[10561] = 879088
mem[32331] = 1030297216
mem[53667] = 1950
mask = 0X1X1010100000111X1011X0X01100101010
mem[3301] = 228441
mem[57487] = 133621
mem[64673] = 14195
mem[6012] = 106013562
mask = 0010X00X10011001001X0X0110X00X011X01
mem[28691] = 7828
mem[3272] = 5314381
mem[53221] = 906
mem[36257] = 483
mem[39139] = 239793066
mask = 011000011X0010X01X1X010X0X0X010000XX
mem[6731] = 146985
mem[22302] = 5265995
mem[25582] = 124093
mem[15205] = 160800
mem[60546] = 47017168
mask = 00100001100011XX1110010X0111000101X0
mem[62302] = 118384
mem[4442] = 943
mem[32331] = 829093
mem[10054] = 261101
mem[21200] = 2244874
mask = 000X10010000100101101X110X1001X00101
mem[22694] = 160856
mem[57484] = 16648378
mem[11347] = 265832659
mask = 0X100011001X1X01001000XX0X01X1111XX1
mem[39729] = 15425
mem[10968] = 43923
mem[15911] = 153
mask = 0001X001X00010010110010X0000010111X0
mem[12587] = 58174
mem[42542] = 272951779
mem[4845] = 155009
mem[47818] = 498
mem[18721] = 12927
mem[762] = 13549777
mem[21972] = 1070883192
mask = 0X00X0101XX0100110100001101XX0XX11X1
mem[412] = 805494480
mem[34671] = 511352857
mem[56856] = 1382270
mem[63089] = 67321554
mask = 00000111011X10X11011X101X001X0001010
mem[62102] = 10598387
mem[47818] = 1528395
mem[50481] = 4148455
mask = 00X010X0111X1001101010000X101110110X
mem[65185] = 729486
mem[56292] = 5502033
mem[12729] = 7021
mask = 0X10X0010XX1100100100000100XX10X1X0X
mem[37193] = 844441364
mem[9631] = 725022597
mem[37488] = 3522
mem[53516] = 357
mask = X000X110XX10101X1011100010X100000X00
mem[6390] = 164431
mem[8483] = 115021
mem[47710] = 243784
mem[49932] = 1658
mem[32606] = 257364
mem[41256] = 2470
mask = 110X0010XX10101110X100X10X111100100X
mem[40832] = 65381067
mem[29277] = 994339
mem[9389] = 9978911
mem[21671] = 11603
mask = 0100100X10101111111X00X010110111X110
mem[53651] = 1911
mem[35511] = 88697
mem[57887] = 177165
mask = 0XX0100100X01000111011X1X1X01011X1X0
mem[59444] = 425
mem[44821] = 7937
mem[36257] = 1973535
mem[40669] = 7964428
mem[57733] = 16992
mem[31224] = 9670413
mem[54960] = 99917
mask = 1111011111111001111XX1X00X011X10000X
mem[52107] = 52615
mem[46430] = 2797674
mem[35037] = 489648
mem[61728] = 5450284
mask = 00101X01000010011010010XX00101000X00
mem[53337] = 167323
mem[23249] = 144
mem[49058] = 78020
mem[65279] = 34789011
mem[57994] = 278
mask = X0X11001X000100X1010X00100000111011X
mem[24461] = 69231
mem[33718] = 3112488
mem[1525] = 205706
mem[24842] = 6365432
mem[37193] = 983
mem[41010] = 1250771
mem[9773] = 492
mask = 000010000X10100X1110XXX00010001X0X01
mem[11359] = 19286
mem[15549] = 671
mask = 001X000110X01X0X10101010011XX001X000
mem[90] = 4165576
mem[7059] = 59706
mem[4845] = 197
mem[35511] = 28681534
mask = 1100XX101110X00X1011X0X0111100001X11
mem[9839] = 7327
mem[33097] = 475963031
mem[47912] = 8358
mem[43269] = 259178327
mask = 0X1010X10X101001011X11X1101011X0X1XX
mem[16654] = 8368090
mem[41291] = 2146
mem[20101] = 34091
mem[21208] = 76242
mem[35435] = 15664567
mask = 00101001010010X1011011010X11010XX1X0
mem[18755] = 314
mem[44355] = 1709364
mem[56366] = 15879386
mem[23427] = 1174
mem[19831] = 39454
mask = X1101011001X1X01001X10111001011XX110
mem[23505] = 402
mem[47596] = 2233058
mem[21760] = 45933
mem[44370] = 27609
mask = 001010010X0010XXX1101111X0X000X1X01X
mem[55611] = 1454243
mem[38591] = 232585
mem[21621] = 231761578
mem[10169] = 4045271
mem[52902] = 35939890
mem[60957] = 205198448
mask = 01001000X0X01111X11X111100011011X11X
mem[32807] = 243712052
mem[45204] = 774567686
mem[62717] = 171
mem[55911] = 132
mem[39733] = 44429
mem[53259] = 79063
mask = 00100001101X1X0110100X11011X0X010000
mem[54912] = 65854650
mem[33458] = 865332
mem[40603] = 96
mem[62127] = 4144
mask = 00101001X00X1001XX10000X1X1X0101X000
mem[37193] = 127119
mem[21633] = 16917
mem[36257] = 211
mask = 01X010110010XXX101100X10X0010100X1X0
mem[48000] = 1030
mem[390] = 3489961
mem[14671] = 218111318
mem[55181] = 232521891
mem[56447] = 5498
mask = 00XX101011111001111100XX10X10111X000
mem[17945] = 14147766
mem[5804] = 9317
mem[3243] = 76354
mem[39812] = 17121
mem[65185] = 882507
mem[24184] = 1503364
mask = 001011X10110X00X0110X0100100000X0111
mem[7059] = 77105630
mem[9842] = 93469
mem[28235] = 46130
mask = 00011001X00010010X101111101X000XX00X
mem[46578] = 188558839
mem[32794] = 563
mem[57424] = 16073628
mem[26204] = 53518555
mem[21585] = 7589033
mem[1209] = 174591
mask = 00X11001000010X1X11XX11100100000001X
mem[14670] = 552230
mem[59848] = 230555109
mem[52210] = 2510
mem[62683] = 42631
mem[34868] = 20381
mem[63506] = 20704
mem[13543] = 149934393
mask = 0000X010X1101001111100X000X0X0XXX001
mem[44146] = 9534
mem[38367] = 71280
mem[48566] = 39540
mem[6301] = 488439556
mem[16638] = 9302
mem[13893] = 744336
mask = 0010000X00001001X1X00001X00110XX0X01
mem[7422] = 659
mem[7156] = 536497
mem[50861] = 75915798
mask = X1X00010X11010X1101X1X101011010X10X0
mem[23749] = 197897888
mem[53516] = 19394
mem[52597] = 327870
mem[18755] = 9112
mem[46245] = 256412
mem[14234] = 89528
mem[57220] = 95738416
mask = 00X01010X000X0X1101X1101011X0111111X
mem[60185] = 799555
mem[33559] = 1502644
mask = X11X001001101011101011100X11X1001XXX
mem[38795] = 45814003
mem[16914] = 10781
mem[22548] = 288
mask = 011001111X111X011111X0001100001XX11X
mem[43238] = 115469
mem[62522] = 25284
mem[7798] = 116533186
mem[40416] = 14162
mem[15067] = 511159
mem[44546] = 9872
mask = 0X00X0101000100X10100X01111000001011
mem[28691] = 35897352
mem[13371] = 68541533
mem[31458] = 6263059
mem[53277] = 8235
mask = 00XX100100001001011011111XXXXXX10X10
mem[246] = 4365
mem[63802] = 28023932
mem[62691] = 38120268
mem[32606] = 32155701
mem[8787] = 73310772
mem[6000] = 187657
mem[34053] = 6302546
mask = 001X001100101X01X01010001XXXXX111001
mem[62717] = 2689826
mem[6969] = 3812794
mem[24914] = 682498819
mask = 0X10X0010XX010X1011001110X100000X00X
mem[29061] = 34429630
mem[60199] = 3521402
mem[59922] = 1653
mask = 00101001000010010X100110XX10010X1001
mem[47632] = 30829832
mem[40349] = 2360241
mask = 000011100110101X101X000X010X11001010
mem[18625] = 2810
mem[21671] = 6351
mem[61897] = 11389
mask = 011000011000X1XX101010XX00100001X000
mem[32419] = 9855
mem[54566] = 13281403
mem[32842] = 3060651
mem[52744] = 15731
mask = 11X01111111110011XX1010X1X1001X101XX
mem[25842] = 357864
mem[55611] = 4142
mem[26491] = 1993
mem[14721] = 46996265
mem[23074] = 163733
mask = 0X000XX1X11010X1101X1010X00111000001
mem[22158] = 121237
mem[36592] = 656697
mem[32719] = 676
mem[33097] = 1034487408
mem[50670] = 3906154
mask = 0000001010101X01101X00010X011X1X1101
mem[47329] = 918208
mem[1840] = 30632603
mem[59200] = 2028
mem[52744] = 32
mem[30837] = 1214273
mem[43183] = 3866571
mask = 0X10X001X000111X1010111X00100X01X1X0
mem[50788] = 115928382
mem[50698] = 3427485
mem[4920] = 452475222
mem[27354] = 14200
mem[50989] = 261259
mem[18721] = 159393273
mask = 01X011X0011010X11X11X00110XX10001011
mem[63758] = 15239
mem[7707] = 2466
mem[42828] = 51703347
mem[56944] = 3802
mem[53539] = 47276878
mem[51073] = 214485441
mask = 0010100X010010011110X1100X100X110110
mem[15448] = 14340
mem[44061] = 2031
mem[19707] = 43457433
mem[40603] = 21065817
mem[4662] = 30053
mem[38981] = 1624176
mem[43872] = 10552
mask = 0000X0101X1X10011X1X000X1010100101X0
mem[6012] = 1491
mem[45255] = 670205912
mem[7096] = 2912
mem[4621] = 10593101
mask = 001010X01111100X10X01001X00110001100
mem[40984] = 1721577
mem[56292] = 31350
mem[27264] = 11075225
mem[13404] = 35195435
mask = 0010100110011X01101001X111X0110X1001
mem[27264] = 1667120
mem[35927] = 155343034
mem[40700] = 93937438
mem[4312] = 16188010
mem[25983] = 493768
mask = 00101X0101X01X01011011XX1X10X100X111
mem[54715] = 79942609
mem[33413] = 711314
mem[17167] = 667493
mem[60601] = 951299
mask = 0X0X10XX10001XX11X1000010X0110010011
mem[6829] = 299200
mem[246] = 7814
mem[60199] = 134519412
mem[44196] = 1696
mask = X11XX111111110011X11XX00101X0111010X
mem[38202] = 63903
mem[6488] = 295501257
mem[15959] = 196571
mem[56914] = 3039159
mask = 001000XX0010X0X1XX100010X1X11000010X
mem[15428] = 17873557
mem[46435] = 5226
mem[26157] = 253038623
mem[29824] = 51824195
mask = 00100001X000111010100101X1X1X00X0X00
mem[1886] = 20037780
mem[23634] = 438988
mem[53277] = 24774137
mem[63775] = 1560
mem[20283] = 16629883
mem[43116] = 58381263
mem[44729] = 272355
mask = 0001X001X00010X101100101100X0001X11X
mem[56960] = 1407
mem[12587] = 28256
mem[49069] = 54282286
mem[42639] = 202261
mem[11480] = 7915801
mem[10095] = 517305
mem[47429] = 782
mask = 0X00X00100X0X00101X01111100010X1X00X
mem[64530] = 2002357
mem[22346] = 193156
mem[412] = 6478
mask = XX0X1111011010X1101X11010X1XX010000X
mem[16907] = 15143671
mem[4135] = 1838
mask = 00101001X11011010110XX1010101X001101
mem[7156] = 773
mem[10128] = 2543913
mem[30159] = 79295
mem[26178] = 967449
mask = 001010010100101X111011010001X0110X1X
mem[47329] = 276882
mem[24610] = 287174
mem[8685] = 29977825
mask = 01001XX011101X01101001XX1X100110110X
mem[64033] = 250909
mem[56548] = 10731345
mem[39241] = 6913465
mask = 01X000X011101011101110001010X100001X
mem[51388] = 4106124
mem[29060] = 4687691
mem[27501] = 16143
mem[25418] = 152
mem[56360] = 3973
mem[50100] = 7595264";

type system = {
  bitmask: array(option(int)),
  memory: Belt.Map.Int.t(int),
};

let makeSystem = () => {
  bitmask: Array.make(35, None),
  memory: Belt.Map.Int.fromArray([||]),
};

let intToArray = num => {
  let l = 36;
  Js.String.repeat(l, "0")
  ++ Js.Int.toStringWithRadix(num, ~radix=2)
  |> Js.String.slice(~from=(-1) * l, ~to_=70)
  |> Js.String.split("")
  |> Array.map(int_of_string);
};

let parseInt: string => int = [%raw {| x => parseInt(x, 2) |}];

let arrayToInt = arr => {
  Js.Array.joinWith("", arr) |> parseInt;
};

let stringToBitmask = (str: string) => {
  let chars = str |> Js.String.split("");
  chars
  |> Array.map(c =>
       switch (c) {
       | "X" => None
       | num => Some(int_of_string(num))
       }
     );
};

let applyBitmask = (system: system, value: int) => {
  intToArray(value)
  |> Array.mapi((i, digit) => {
       switch (system.bitmask[i]) {
       | Some(mask) => mask
       | None => digit
       }
     })
  |> arrayToInt;
};

let regexCaptures = (re: Js.Re.t, str: string) => {
  switch (str |> Js.Re.exec_(re)) {
  | Some(result) =>
    Some(result |> Js.Re.captures |> Array.map(Js.Nullable.toOption))
  | None => None
  };
};

let parseMask = (line: string) => {
  let re = [%re "/mask = (.*)/"];
  switch (line |> regexCaptures(re)) {
  | Some([|_, mask|]) => mask
  | _ => None
  };
};

let parseMemValue = (line: string) => {
  let re = [%re "/mem\\[(\\d+)\\] = (\\d+)/"];
  switch (line |> regexCaptures(re)) {
  | Some([|_, Some(address), Some(value)|]) =>
    Some((int_of_string(address), int_of_string(value)))
  | _ => None
  };
};

let parseLine = (system: system, line: string) => {
  exception InvalidLine(string);

  switch (parseMask(line), parseMemValue(line)) {
  | (Some(mask), None) => {...system, bitmask: stringToBitmask(mask)}
  | (None, Some((address, value))) =>
    let maskedValue = applyBitmask(system, value);
    let memory = system.memory->Belt.Map.Int.set(address, maskedValue);
    {...system, memory};
  | _ => raise(InvalidLine(line))
  };
};

let rec runProgram =
        (
          ~system: system=makeSystem(),
          ~updater=parseLine,
          program: list(string),
        ) => {
  switch (program) {
  | [line, ...rest] =>
    let updated = updater(system, line);
    runProgram(~system=updated, ~updater, rest);
  | _ => system
  };
};

let parseInput = (input: string) => {
  input |> Js.String.split("\n") |> Array.to_list;
};

let day1 = () => {
  let input = inputStr |> parseInput;

  let system = runProgram(input);
  let values = system.memory->Belt.Map.Int.valuesToArray;

  values |> Array.fold_left((sum, value) => sum +. float_of_int(value), 0.);
};

Js.log2("Day 1:", day1());

let applyBitmaskToAddress = (system: system, address: int) => {
  exception InvalidBitmaskDigit(int);
  let bits = intToArray(address);

  let rec generateAddresses =
          (
            ~index: int=0,
            ~addresses: list(list(int))=[[]],
            addr: array(int),
          ) =>
    if (index >= Array.length(addr)) {
      addresses;
    } else {
      switch (system.bitmask[index]) {
      | Some(0) =>
        let generated = addresses |> List.map(a => {[addr[index], ...a]});
        generateAddresses(~index=index + 1, ~addresses=generated, addr);
      | Some(1) =>
        let generated = addresses |> List.map(a => {[1, ...a]});
        generateAddresses(~index=index + 1, ~addresses=generated, addr);
      | None =>
        let generated0 = addresses |> List.map(a => {[0, ...a]});
        let generated1 = addresses |> List.map(a => {[1, ...a]});
        generateAddresses(
          ~index=index + 1,
          ~addresses=generated0 @ generated1,
          addr,
        );

      | Some(d) => raise(InvalidBitmaskDigit(d))
      };
    };

  let addresses = generateAddresses(bits);
  addresses
  |> List.map(List.rev)
  |> List.map(addr => Array.of_list(addr) |> arrayToInt);
};

let parseLineDay2 = (system: system, line: string) => {
  exception InvalidLine(string);

  switch (parseMask(line), parseMemValue(line)) {
  | (Some(mask), None) => {...system, bitmask: stringToBitmask(mask)}
  | (None, Some((address, value))) =>
    let addresses = applyBitmaskToAddress(system, address);
    let memory =
      addresses
      |> List.fold_left(
           (mem, addr) => {Belt.Map.Int.set(mem, addr, value)},
           system.memory,
         );
    {...system, memory};
  | _ => raise(InvalidLine(line))
  };
};

let day2 = () => {
  let input = inputStr |> parseInput;
  let system = runProgram(~updater=parseLineDay2, input);
  let values = system.memory->Belt.Map.Int.valuesToArray;

  values |> Array.fold_left((sum, value) => sum +. float_of_int(value), 0.);
};

Js.log2("Day 2:", day2());
