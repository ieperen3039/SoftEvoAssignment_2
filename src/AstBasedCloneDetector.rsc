module AstBasedCloneDetector

import lang::java::m3::AST;

import steps::astbased::FileParser;
import steps::astbased::ASTDetector;
import GoldenStandardReader;
import DataTypes;

import IO;
import Map;

void main() = detectClonesUsingAstsOnSmallSet();

void detectClonesUsingAstsOnSmallSet() = detectClonesUsingAsts(|project://assignment2/data/small|);
void detectClonesUsingAstsOnLargeSet() = detectClonesUsingAsts(|project://assignment2/data/large|);

void detectClonesUsingAsts(loc dataDir) {
	map[loc l, Declaration d] asts = parseFiles(dataDir);
	Clone clones = {};
	int i = 0;
	
	int total = size(asts);
	total *= (total - 1);
	total /= 2;
	
	for (f1 <- asts){
		for (f2 <- asts, f1.uri < f2.uri){
			i += 1;
			Declaration d1 = removeAnnotations(asts[f1]);
			Declaration d2 = removeAnnotations(asts[f2]);
			
			print("<i> / <total>: \n");
			clones += compare(d1, d2);
		}
	}
	
	writeFile(|project://assignment2/data/result_ast.csv|, createOutput(clones));
	
	compareResults(clones, readGoldenStandardSmall());
}