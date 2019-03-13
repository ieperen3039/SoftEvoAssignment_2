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
	
	list[Clone] clones;
	
	for (f1 <- asts){
		for (f2 <- asts, f1 != f2){
			Declaration d1 = asts[f1];
			Declaration d2 = asts[f2];
			
			findClones(d1, d2);
		}
	}
}