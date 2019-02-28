module AstBasedCloneDetector

import lang::java::m3::AST;

import steps::astbased::FileParser;
import steps::astbased::ASTComparator;
import DataTypes;

import IO;
import Map;

void detectClonesUsingAstsOnSmallSet() = detectClonesUsingAsts(|project://assignment2/data/small|);
void detectClonesUsingAstsOnLargeSet() = detectClonesUsingAsts(|project://assignment2/data/large|);

void detectClonesUsingAsts(loc dataDir) {
	map[loc l, Declaration d] asts = parseFiles(dataDir);
	
	tuple[Declaration a, Declaration b] clones;
	
	for (<f1, d1> <- asts){
		for (<f2, d2> <- asts){
			bool clone = areClones(d1, d2);
			clones += <d1, d2>;
		}
	}
}