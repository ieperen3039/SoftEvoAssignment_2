module AstBasedCloneDetector

import lang::java::m3::AST;

import steps::astbased::FileParser;
import steps::astbased::ASTComparator;
import DataTypes;

import IO;
import Map;

void detectClonesUsingAstsOnSmallSet() = detectClonesUsingAsts(|project://assignment2/data/small|);
void detectClonesUsingAstsOnLargeSet() = detectClonesUsingAsts(|project://assignment2/data/large|);
void main() = detectClonesUsingAstsOnSmallSet();

void detectClonesUsingAsts(loc dataDir) {
	map[loc l, Declaration d] asts = parseFiles(dataDir);
	int i = 0;
	
	for (f1 <- asts){
		for (f2 <- asts, f1.uri < f2.uri){
			i += 1;
			Declaration d1 = asts[f1];
			Declaration d2 = asts[f2];
			
			print("<i>: \n");
			findClones(d1, d2);
		}
	}
}