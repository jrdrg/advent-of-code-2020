// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Belt_MapInt = require("bs-platform/lib/js/belt_MapInt.js");
var Belt_SetInt = require("bs-platform/lib/js/belt_SetInt.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");

var inputStr = "151\n94\n14\n118\n25\n143\n33\n23\n80\n95\n87\n44\n150\n39\n148\n51\n138\n121\n70\n69\n90\n155\n144\n40\n77\n8\n97\n45\n152\n58\n65\n63\n128\n101\n31\n112\n140\n86\n30\n55\n104\n135\n115\n16\n26\n60\n96\n85\n84\n48\n4\n131\n54\n52\n139\n76\n91\n46\n15\n17\n37\n156\n134\n98\n83\n111\n72\n34\n7\n108\n149\n116\n32\n110\n47\n157\n75\n13\n10\n145\n1\n127\n41\n53\n2\n3\n117\n71\n109\n105\n64\n27\n38\n59\n24\n20\n124\n9\n66";

function parseInput(input) {
  return List.map(Caml_format.caml_int_of_string, $$Array.to_list(input.split("\n")));
}

function inputToSet(input) {
  return Belt_SetInt.fromArray($$Array.of_list(input));
}

function chainAdapters(_$staropt$star, set) {
  while(true) {
    var $staropt$star = _$staropt$star;
    var chains = $staropt$star !== undefined ? $staropt$star : /* :: */[
        /* :: */[
          0,
          /* [] */0
        ],
        /* [] */0
      ];
    if (chains) {
      var chain = chains[0];
      if (chain) {
        var head = chain[0];
        if (List.length(chain) === (Belt_SetInt.size(set) + 1 | 0)) {
          return /* :: */[
                  head + 3 | 0,
                  chain
                ];
        } else {
          var available = Belt_SetInt.toArray(Belt_SetInt.keep(set, (function(head){
                  return function (v) {
                    if (v > head) {
                      return v <= (head + 3 | 0);
                    } else {
                      return false;
                    }
                  }
                  }(head))));
          var nextChains = $$Array.map((function(chain){
              return function (start) {
                return /* :: */[
                        start,
                        chain
                      ];
              }
              }(chain)), available);
          _$staropt$star = Pervasives.$at($$Array.to_list(nextChains), chains[1]);
          continue ;
        }
      } else {
        return /* [] */0;
      }
    } else {
      return /* [] */0;
    }
  };
}

function findDifferences(_$staropt$star, _chain) {
  while(true) {
    var chain = _chain;
    var $staropt$star = _$staropt$star;
    var diffs = $staropt$star !== undefined ? Caml_option.valFromOption($staropt$star) : Belt_MapInt.fromArray(/* array */[]);
    if (chain) {
      var match = chain[1];
      if (match) {
        var next = match[0];
        var diff = chain[0] - next | 0;
        var updatedDiffs = Belt_MapInt.update(diffs, diff, (function (v) {
                if (v !== undefined) {
                  return v + 1 | 0;
                } else {
                  return 1;
                }
              }));
        _chain = /* :: */[
          next,
          match[1]
        ];
        _$staropt$star = Caml_option.some(updatedDiffs);
        continue ;
      } else {
        return diffs;
      }
    } else {
      return diffs;
    }
  };
}

function day1(param) {
  var input = parseInput(inputStr);
  var set = Belt_SetInt.fromArray($$Array.of_list(input));
  var chain = chainAdapters(undefined, set);
  var diffs = findDifferences(undefined, chain);
  return Caml_int32.imul(Belt_MapInt.getExn(diffs, 1), Belt_MapInt.getExn(diffs, 3));
}

console.log("Day 1:", day1(/* () */0));

function findAllValidChains(_$staropt$star, _$staropt$star$1, set) {
  while(true) {
    var $staropt$star = _$staropt$star$1;
    var $staropt$star$1 = _$staropt$star;
    var chains = $staropt$star$1 !== undefined ? $staropt$star$1 : /* :: */[
        /* :: */[
          0,
          /* [] */0
        ],
        /* [] */0
      ];
    var valid = $staropt$star !== undefined ? $staropt$star : 0;
    if (chains) {
      var chain = chains[0];
      if (chain) {
        var head = chain[0];
        var available = Belt_SetInt.toList(Belt_SetInt.keep(set, (function(head){
                return function (v) {
                  if (v > head) {
                    return v <= (head + 3 | 0);
                  } else {
                    return false;
                  }
                }
                }(head))));
        var valid$1 = List.length(available) === 0 ? (console.log("valid", valid + 1 | 0), valid + 1 | 0) : valid;
        var nextChains = List.map((function(chain){
            return function (start) {
              return /* :: */[
                      start,
                      chain
                    ];
            }
            }(chain)), available);
        _$staropt$star$1 = valid$1;
        _$staropt$star = Pervasives.$at(nextChains, chains[1]);
        continue ;
      } else {
        return 0;
      }
    } else {
      return valid;
    }
  };
}

function day2(param) {
  var input = parseInput(inputStr);
  var set = Belt_SetInt.fromArray($$Array.of_list(input));
  console.log("set> ", Belt_SetInt.toArray(set));
  var validChains = findAllValidChains(undefined, undefined, set);
  console.log("valid", validChains);
  return validChains;
}

console.log("Day 2:", day2(/* () */0));

var exampleInput = "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4";

var example2 = "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3";

exports.exampleInput = exampleInput;
exports.example2 = example2;
exports.inputStr = inputStr;
exports.parseInput = parseInput;
exports.inputToSet = inputToSet;
exports.chainAdapters = chainAdapters;
exports.findDifferences = findDifferences;
exports.day1 = day1;
exports.findAllValidChains = findAllValidChains;
exports.day2 = day2;
/*  Not a pure module */