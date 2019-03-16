module steps::stringbased::Type3Detection

import steps::stringbased::FileReader;
import steps::stringbased::CombineFragment;
import steps::stringbased::FunctionReformating;
import DataTypes;

import String;
import List;
import Map;
import IO;


int simpleComparison(loc f1, Content lines1, loc f2, Content lines2){
	int size = size(lines1);
	int codesize1 = 0;
	int codesize2 = 0;
	int count = 0;
	for(i <- [0..size]){
		codesize1 += lines1[i].nr.length;
		codesize2 += lines2[i].nr.length;
		if(lines1[i].line != lines2[i].line){
			count += 1;
		}
	}
	return 100 - ((count * 100) / size);
}

int calculateSimilarity(loc f1, Content lines1, loc f2, Content lines2){
	int size1 = size(lines1);
	int size2 = size(lines2);
	int similarity = 0;
	if(size1 == size2){
		similarity = simpleComparison(f1, lines1, f2, lines2);
	}
	return similarity;
}


Clone type3Detection(MethodContent methods){
	Clone found = {};
	methods = (f: replaceSIDnames(methods[f]) | f <- methods);
	for(f1 <- methods){
		for(f2 <- methods, f2 != f1){
			int similarity = calculateSimilarity(f1, methods[f1], f2, methods[f2]);
			if(similarity >= 70){
				found += <f1, f2, type3(), similarity>;
				found += <f2, f1, type3(), similarity>;
			}
		}
	}
	return found;
}