// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

var inputStr = "LLLLLLLL.LLLLLLLLL.LL.LLLLLLL.LLLLL.LLLLLLLL.LLLLL.LLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLL\nLLLLLLLL.LLLLLLLLLLLLLL.L.LLL..LLLL.LLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLL..LLLLLLLLLL\nLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LL.LL.LLLLLLLLLLLLLLL.LLLLLLLLL.LLLLL.L.LLL.LLLL.L.LLLLLLL.LLLLLLL\nLLLLLLLL.LLLLLLL.L.LLLL.LLLLL.LLLLL.LLLLLLLLLLLLLLL..LLLLLL.LLLLLLLLL.LLLLLLLL..LLLLLLLLLLLLLL.L\nLLLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLL.LL.LLLLLLLL.LLLLLL.LLLLLL.LLL\nLLLLLLLL.LLLLLLLLL.LLLLLLLLLL.LLL.LLLLLLLLLL.L.LLL..LLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLL.LLLL.LLLLL\nLLLL.LLL.LLLLLL.LL.LLLLL..LLL.LLLL..LL.L.LLLLLLLLLLL.LLLLLLLL.LLLLLLL..LLLLLLL.LLL.LLLLLLLLLLLLL\nLLLLLLLL.LLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLL.LLLLLLLLLL\nLL...L......L.L.L......L....L.....LLLL..L.L.L.L................LL........L..L...L......L..L.LL..\nL.LLLLLLLLLLLLLL.L.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LL\nLLLLLLLL.L.LLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLL\nLLLLLLLL..L.LLLLLL.LLLL.LLLLLLLL.LLLLLL.LL.L.LLLLLL.LLLLLLLLLLLLLLLLL.L.LLLLL..LLLLLLLLLLLLLLLLL\nLLLLLLLL.LLLL.LLLL.LLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLLL.LLLLLL.L.LLLLLLL.LL\nLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLL.LLLLLLLL.LLLLLLLLLLLLL.LL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLL\nLLLLLLLL.LLLL.LLLL.LLLL.LLLLL.LLLLL.LL.LLLL..L.LLLL.LLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLL..LLLLL.LLLL\n.L...L.....L.........L.L..L...LL.....L.L.L..L.L.....L..L.L.L........L...L..L.......L....L...LL.L\nLLLLLLLLLLL.LLL.LL.LLLL.LLL.L.LLLLLLLLLLLLL..LLLLLLLLLLLLLLLL.LL.LLLL.LLLLLLLLLLLLLLLLLLLLL.LLLL\nLLLLLLLL.LLLLLLLLLLLLLLLLLLL..LL.LLLLLLLLLLL.LLLLLL.LLLLLLLL..LLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLL\nLLLLLLLL.LLLLLLLLLLLLLL.LLLL..LLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.L.LLLLLL.LLLLLLLLLL\nLLLLLLLL...LLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLL.L.LLLLLLLLLLL.LLLLLLL.LL..LLLLLLLLLLL.LLLLLLLLLL\nL...LL.L........L..LLL.L.LL..L..L.L.L..L..L.........L.....LLL.....LLL........LLL...LL..L.L...LL.\nLLLLLLLLLLLLLLLLLL.LLLL.LLLLL.LLLLL.L.LLLLL...LLL.L.L.LLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLL\nLLL.LLLL.LLLLLLLLL.LLLLLLLLL..L.L.L.LLLLLLLL.LLLLLL.LLLLLLL.L.LLLLLLL.LLL.LLLLLLLLLLL.LLLLLLLLLL\nLLLLLLLL.LLLL.LLLLLLLLL.LLLLLLLLLLL.LLL.LLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.\nLLLLLLLL.LLLLLLLLL.LLLL.LLLLL..LLLL.LLLLLLLL.LLLLLL.LLLLLLLLL.LL.LLLL.LLLLLLLL.LLLLLL.LLLLLLLLLL\n...L.L.L..LL.......L.LL.L..LL........LLLL.....LL.LLL.L.LL...L.L.......LL....L..L..LL.L....L..LLL\nL...LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLL.L.LLLL.LLLLL.LLL.LLLLLLL.LLLLLLLLLLLLLLL.LL.LLLLLLL\nLLLLLLLL.LLL.LLLLL.LLLLLLLLLL.LLLLL.LLLLLLLLLLL.LLL.LLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLL.LLLLLLLLLLL\nLLLLLLLL.L.LLLLLLL.LLLL.LLLLL.LLLLLLLLLLLLLL..LLLLL.LLLLLL.L..LLLLLLL.LL.L.LLL.LLLLLL.LLLLLLLLLL\nLLLLLLLL.LLLLLL.LL.LLLL.LLLLL.L.LLL.LLLLLLLL.LLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLL.LLLLLL.LLLLLLLLLL\nLLLLLLLLLLLLLLLLLL.LL.L.LLLLL.LLLLL.LLLLLLLLLLLLLLL.LLLLLLLLL.LLLLL.L.LLLLLLLL.LLLLLL.LLLL.LLLLL\n..LL...LL..LLL.L...L....L.L...L.L.L...L.LL.L........L.L..L...L..L.L.LL.L.L..L..LLLL..L..L..L...L\nLL.LLLLL.LLLLLLL.LLLL.L.LLLLL.LLLLL.LLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLL.L.LLLLLLLLLLLLL.LL.LLLLLLL\nLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLL.LLL.L.LLLL\nLLLLLLLLLLLL.LLLLLLLLLL.LLLLL.LLLLL.LLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLL\nLLLL.LLL.LLLLLLLLL.LL...LLL.L.LLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLL.LLLLLLLLLL\nLLLLLLL.L.LLLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLL...LLLLL.LLLLLLLLL.LLLLLLL.LLLL.LLLLLLLLLL.LLLLLLLLLL\nLLLLLLLLLLLLLLLLLL.LL.L.LLLLL.LLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLL.L.L.LLLLLLLL.LLLL.LLLLLLLLLL.L\nLLLLLLLL.LLLLLLLL..LLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLL.LL.LLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.\n...L......L....L...LLLL.LLLLL.L..LL.L........LL...L.L.LLLLLLLLLL.LL.LLLL...L....L.....L....LL...\nLLLL.LLLLLLLLLLLLL..L.L.LLLLLLLLLLLLLLLLLL.L.LLLLLL.LLLLLLL.L.LLLLLLL.LL.LLLLL.LLLLLL.LLLLLLLLLL\nLLLLLLLL.LLLLL.LLL.LLLL.LLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LL.LLLLLLLLLLLLLLLLL\nLLLLLLLLLLL.LLLLLL.LLLL.LLLLLLLLL.L.LLLLLLLL.LLLLLLLLLLLLLLLL..LLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLL\nLLLLLLLLL.LLLLLLLL.LLL..LLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLL.L.LLLLLLLL\nLLL.LLLL.LLLLL.LLL..LLL.LLLL..LLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL...LLL.LLL.LLLLLL.LLLLLLLLLL\nLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLL.LLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLLL.LLLLLLLL.LLLLLLL.LL\nL.L.L.L.L.L....L.LL.L...LL......LL.LL...LLL....L..L..LL.LL.L.L..L..LL.L...LLL.....LLLL.....L....\nLLLLLL...LLLLLLLLL..LLL.LLLLL.LLL.LLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLL.LLLLLLL.LL\nLLLLLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLL.LLLLLLLL.LLLLL..LLLLLL.LL.LLLL..L.LLLLLLLL.LLLLL.LLLLLLLLLLL\nLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.L.LLL.LLLLLLLL.LLLL.L.LLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLL\nLLLLLLLL.LLLLL.LL..LLLL.LLLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LL.LLLL.LLLLL..L.L.LLLL.LLLLLL.LLL\nLLLLLLLL.LLL.LLLLL.LLLLLL.LLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLL.L.LLLLL..LLLLLLLL\nLLLLLLLLLLLLLL.LLL..LLL.LLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLL.L.LLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLL\nL.LLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLLL.LLL.LLLL.LLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL..LLLLL.LLLL.LLLLL\nLLLL.L....L.L.LL.LLLL...LL....L.L..L....L...LLL..L..LLLL...L..L.L.L....L..L.LL.L.L...L...L....L.\nL.LLLLLL.LLLLLLLLL.LLL..LLLLL.LLLLL.LLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL\nLLLLLLLL.LLLLLLL.LLLLLL.LLLLL.LLLLL.LLLLLLLL.LL.LLLL.LLLLL.LL.L.LL.LL.LLLLLLLLLLLLLLL.LLLLLLLLLL\nLLLLLLLLLLLLLLLLLL..LLL.LLLLL.LLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLL\nLLLLLLLL.LLLLLLLLL.LLLL..LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LL.LLLL.LLLLLLLLLLL.LLLLLLLLL\nLLLLLLLL.LLLLLL.LL.LLLL.LLLLLL.L.LL.LLLLLLLL.LLLL.L.LLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLLL\nLLLLLLLL.LLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLL.LLLLLL.LLLLLLLLLLL.LLLLLLL.LLLLLLL..LLLLLLLLLLLLLLLLL\n.L.L..L....L...........L.LL....L.......L..L.LL..L.L........L..L.............L.L....LL.LLL..L.LL.\nLLLLLLLL.LLLLLL.LL.LLLL.LLLLL.LLLLL.LLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL\nLLLLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLL.L.LLL.LL.LLLLLLLLLL\nLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLLLLL.LLLLLLLLLLLLLL..LLLLL.LLL.LL.LLLL.LLLLLLLL.LLL.LLLLLLLLLLLLL\nLLLLLLLLLLLLLLLLLL.LLLL.LL.LLL..LLLLLLLLLLLL.LLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLL..LL.LL.LLL.LL.LLL\n........L...L....LL.....LL..L.....LL.LLLL.LL.LLL.....LLL.....LL.L..LLL.....LL.............L....L\nLLL.LLLL.LLLLLLLLLL.LLL.LLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLL...LLLLLL.L.L.LL..LLLLLLLLL\nLLLLLLLLLL.LL.LLLLLLLLL.L.LLL.LLLLL.LLLLLLLL..LLLLLLLLLLLLLLLLLLLLL...LLLLLLLL.LLLLLL.LLLLLLLLLL\n.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLL.LLLL.LL..LLL.LLLLLLLLL.LLLLLL.LL.L.LL.LLLLLLLLLLLLLLLLLLLLLLLLLL\nLLLLLLLL.LLLLL.LLL.LLLLL.LLL.LLLLLL.LLLLL.LL.LLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLL.LLL\nLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLL.LL.LLL.LLLLLLLL..LLLLLLL.LLLLLLLL.LLLLLL.LLLLLLLLLL\nLLLL.LLL.LLLLLLLLLLLLLLLLLLL.LLLL.L.LLL..LLLL.L.LLL.LL.L.LL.L.LLLLLLL.LLLLLLLL.LLLL.L.L.LLLLLLLL\nLLLLLLLL.LLLLLLLLL.LLLLLLLLLL.LLLLL.LLLLLLLLLLLLL.L.LL.LLLLLL.L.LLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLL\nLLL.LLLL.LLLLLLL.L.LLLLLLLLLL.LLLLL..LLL.LL..LLLLLL.LL.LLLLLL.LLLLLLL.LL.LLLLLLLLLLLL.LLLLLLLLLL\n.L.....L...L.......L...L....L....L..L..LLL.....L............L..LL.L...L.L.L..LL...LLL..L.L.L....\nLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLL.LLLLLLLLLL\n.L.LLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLL..LLLLL.LLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLL\nLLLLLLLLLLLLLLLL.L.LLLLLLLLLL.LLLLLLLLL.LLLL.LLLLLL.LLLLLLL.L.LLLLLL.LLLLLL.LL.LLLLLL.LLLLLL.LLL\nLL.LLLLL.LLLLLLLLL..LLL.L.LLL.LLLLL.L.LLL.LL.LLLLLL.LLLLLLLLL.LLLLLL..LLL.LLLL.LLLLL.LLLLLLLLLLL\n.L...LLL.....L.....LLL....LLLL.L.LL....L...L...L.L.L....L.LLLL...L.L....L.....LL.L.....L..L.L...\nLLLLL.L...LLLLLLLL.LLLLLLLLLL.LLLLLL.LLLLLLL.LLLLLLLLLLLLLLLL.LL.LLLL.L.LLLLLL.LLLLLL.LLLL.LLLLL\nLLLL.LLL.LLLLLLLLLLLLLLLL.LLL.LLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLL.L.LLLLLL.LLLLLLLLLLLLLLLLL\nLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLL.L.LLLLLLL.L.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL\nLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLL.LL.LLLLLLLLL.\n..L..................LL..LL....L.....L..L..L.L...LLLL......L.L.L......L..L....LLLL.L...L....LL..\nLLLLLLLL.LLLLLLL..LLLLL.LLLL.LLLLLL.LLLLLLLL.LLLLLL.L.LLLLLLL.LLL.LLLLLLLLLLLL.LLLLLL.LLLLLLLLLL\nLLLLLLLL.LLLLLLLL.LLLLL.LLLLL.LLLLLLLLLLLL.L.LLLLLL..LLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLL.LLLLLLLLLL\nLLLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLL.LLL.LLLLLL..LLLLLLL.L\nLLLLLLLLLLLLLLLLLL.LLLL.LLLLL.LL.LLLLLLLLLLL.LLLLLL..LLLLLL.LLLLLLLLL..LL.LLLL.LLLL.L.LLLLLLLLLL\nLLLLLLLL.LLLLLLL.LLLLLL.LLLLLLLL.LLLLLLLLLLL.LLL.LLLLLLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLL\nL.LLL.LL.LLLL.LLL..LLLL.LLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.L.LL.LLLL.LLLLLLL\nLLLLLLLL.LLLLLLLLL.LLLL.LLLLLL..LLL.LLLLL.LL.LLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLLLL.LLLL.LLLLLLLLLL\nLLLLLLLL.LLLLLLLLL..LLLLL.LL..LLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL\nLLLLLLLLLLLLLLLLLL..LLL.LLLLL.LLLLL.LLLLLLLL.LLLLLL..L.LLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLL\nLLLLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLLL.LLLLLL.LLLLL.LLL.LLLLLLL.LLLLL.LL.LLLL.L.LLLLLLLLLL\nLLL.LLLL.LL.LLLLLLLLLLL.LLLLLLLLLL..LLLLLLLL.LL.LLLLL.LL.LLLLLLLLLLLLLLLLLLLLLLLLLLL..LLLLLLLLLL\nLLLLLLLL.LLLLLLLLL.LLLL.LLLL..LLLLL.LLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLL";

function stringToCell(str) {
  var InvalidString = Caml_exceptions.create("InvalidString");
  switch (str) {
    case "#" :
        return /* Occupied */1;
    case "." :
        return /* Floor */0;
    case "L" :
        return /* Empty */2;
    default:
      throw [
            InvalidString,
            str
          ];
  }
}

function cellToString(cell) {
  switch (cell) {
    case /* Floor */0 :
        return ".";
    case /* Occupied */1 :
        return "#";
    case /* Empty */2 :
        return "L";
    
  }
}

function layoutToString(layout) {
  return $$Array.map((function (row) {
                  return $$Array.map(cellToString, row).join("");
                }), layout).join("\n");
}

function parseInput(input) {
  return $$Array.map((function (row) {
                return $$Array.map(stringToCell, row.split(""));
              }), input.split("\n"));
}

function isWithinBounds(x, y, layout) {
  if (x >= 0 && y >= 0 && y < layout.length) {
    return x < Caml_array.caml_array_get(layout, 0).length;
  } else {
    return false;
  }
}

function getCellAt(x, y, layout) {
  if (isWithinBounds(x, y, layout)) {
    return Caml_array.caml_array_get(Caml_array.caml_array_get(layout, y), x);
  }
  
}

function occupiedCount(x, y, layout) {
  var match = getCellAt(x, y, layout);
  if (match === 1) {
    return 1;
  } else {
    return 0;
  }
}

function getAdjacentOccupied(x, y, layout) {
  return ((((((occupiedCount(x - 1 | 0, y - 1 | 0, layout) + occupiedCount(x, y - 1 | 0, layout) | 0) + occupiedCount(x + 1 | 0, y - 1 | 0, layout) | 0) + occupiedCount(x - 1 | 0, y, layout) | 0) + occupiedCount(x + 1 | 0, y, layout) | 0) + occupiedCount(x - 1 | 0, y + 1 | 0, layout) | 0) + occupiedCount(x, y + 1 | 0, layout) | 0) + occupiedCount(x + 1 | 0, y + 1 | 0, layout) | 0;
}

function step(layout) {
  var changed = {
    contents: false
  };
  var next = $$Array.mapi((function (rIdx, row) {
          return $$Array.mapi((function (cIdx, cell) {
                        switch (cell) {
                          case /* Floor */0 :
                              return cell;
                          case /* Occupied */1 :
                              if (getAdjacentOccupied(cIdx, rIdx, layout) >= 4) {
                                changed.contents = true;
                                return /* Empty */2;
                              } else {
                                return cell;
                              }
                          case /* Empty */2 :
                              if (getAdjacentOccupied(cIdx, rIdx, layout) === 0) {
                                changed.contents = true;
                                return /* Occupied */1;
                              } else {
                                return cell;
                              }
                          
                        }
                      }), row);
        }), layout);
  return /* tuple */[
          next,
          changed.contents
        ];
}

function stabilizeLayout(nextStep, _layout) {
  while(true) {
    var layout = _layout;
    var match = Curry._1(nextStep, layout);
    var next = match[0];
    if (match[1]) {
      _layout = next;
      continue ;
    } else {
      return next;
    }
  };
}

function countOccupiedSeats(layout) {
  return $$Array.fold_left((function (sum, row) {
                return sum + $$Array.fold_left((function (rowSum, cell) {
                              if (cell !== 1) {
                                return rowSum;
                              } else {
                                return rowSum + 1 | 0;
                              }
                            }), 0, row) | 0;
              }), 0, layout);
}

function day1(param) {
  var input = parseInput(inputStr);
  return countOccupiedSeats(stabilizeLayout(step, input));
}

console.log("Day 1:", day1(/* () */0));

function visibleOccupiedSeat(_param, _param$1, layout) {
  while(true) {
    var param = _param;
    var param$1 = _param$1;
    var dy = param$1[1];
    var dx = param$1[0];
    var NoPosChange = Caml_exceptions.create("NoPosChange");
    if (dx === 0 && dy === 0) {
      throw NoPosChange;
    }
    var x = param[0] + dx | 0;
    var y = param[1] + dy | 0;
    var match = getCellAt(x, y, layout);
    if (match !== undefined) {
      switch (match) {
        case /* Floor */0 :
            _param$1 = /* tuple */[
              dx,
              dy
            ];
            _param = /* tuple */[
              x,
              y
            ];
            continue ;
        case /* Occupied */1 :
            return 1;
        case /* Empty */2 :
            return 0;
        
      }
    } else {
      return 0;
    }
  };
}

var directions = /* :: */[
  /* tuple */[
    -1,
    -1
  ],
  /* :: */[
    /* tuple */[
      0,
      -1
    ],
    /* :: */[
      /* tuple */[
        1,
        -1
      ],
      /* :: */[
        /* tuple */[
          -1,
          0
        ],
        /* :: */[
          /* tuple */[
            1,
            0
          ],
          /* :: */[
            /* tuple */[
              -1,
              1
            ],
            /* :: */[
              /* tuple */[
                0,
                1
              ],
              /* :: */[
                /* tuple */[
                  1,
                  1
                ],
                /* [] */0
              ]
            ]
          ]
        ]
      ]
    ]
  ]
];

function getVisibleOccupied(x, y, layout) {
  return List.fold_left((function (sum, param) {
                return sum + visibleOccupiedSeat(/* tuple */[
                            x,
                            y
                          ], /* tuple */[
                            param[0],
                            param[1]
                          ], layout) | 0;
              }), 0, directions);
}

function step2(layout) {
  var changed = {
    contents: false
  };
  var next = $$Array.mapi((function (rIdx, row) {
          return $$Array.mapi((function (cIdx, cell) {
                        switch (cell) {
                          case /* Floor */0 :
                              return cell;
                          case /* Occupied */1 :
                              if (getVisibleOccupied(cIdx, rIdx, layout) >= 5) {
                                changed.contents = true;
                                return /* Empty */2;
                              } else {
                                return cell;
                              }
                          case /* Empty */2 :
                              if (getVisibleOccupied(cIdx, rIdx, layout) === 0) {
                                changed.contents = true;
                                return /* Occupied */1;
                              } else {
                                return cell;
                              }
                          
                        }
                      }), row);
        }), layout);
  return /* tuple */[
          next,
          changed.contents
        ];
}

function day2(param) {
  var input = parseInput(inputStr);
  return countOccupiedSeats(stabilizeLayout(step2, input));
}

console.log("Day 2:", day2(/* () */0));

var exampleStr = "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL";

exports.exampleStr = exampleStr;
exports.inputStr = inputStr;
exports.stringToCell = stringToCell;
exports.cellToString = cellToString;
exports.layoutToString = layoutToString;
exports.parseInput = parseInput;
exports.isWithinBounds = isWithinBounds;
exports.getCellAt = getCellAt;
exports.occupiedCount = occupiedCount;
exports.getAdjacentOccupied = getAdjacentOccupied;
exports.step = step;
exports.stabilizeLayout = stabilizeLayout;
exports.countOccupiedSeats = countOccupiedSeats;
exports.day1 = day1;
exports.visibleOccupiedSeat = visibleOccupiedSeat;
exports.directions = directions;
exports.getVisibleOccupied = getVisibleOccupied;
exports.step2 = step2;
exports.day2 = day2;
/*  Not a pure module */
