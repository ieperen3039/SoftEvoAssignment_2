module AstBasedCloneDetector

import lang::java::m3::AST;

import steps::astbased::FileParser;
import DataTypes;

import IO;
import Map;

void detectClonesUsingAstsOnSmallSet() = detectClonesUsingAsts(|project://assignment2/data/small|);
void detectClonesUsingAstsOnLargeSet() = detectClonesUsingAsts(|project://assignment2/data/large|);

void detectClonesUsingAsts(loc dataDir) {
  map[loc,Declaration] asts = parseFiles(dataDir);

  // TODO: implement some AST based clone detection technique.
  //   HINT: Try to decompose in seperate steps (like in the previous assignment)
}