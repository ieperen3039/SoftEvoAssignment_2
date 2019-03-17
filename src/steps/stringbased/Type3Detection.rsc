module steps::stringbased::Type3Detection

import steps::stringbased::FileReader;
import steps::stringbased::CombineFragment;
import steps::stringbased::FunctionReformating;
import DataTypes;

import String;
import List;
import Map;
import IO;


int simpleComparison(Content lines1, Content lines2){
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

int complexComparison(Content lines1, Content lines2){
	int size1 = size(lines1);
	int size2 = size(lines2);
	int min = size1;
	if(size2 < min){
		min = size2;
	}
	int pos1 = 0;
	int pos2 = 0;
	int count = 0;
	for(i <- [0..min]){
		if(lines1[pos1].line == lines2[pos2].line){
			pos1 += 1;
			pos2 += 1;
		} else if(lines1[pos1 + 1].line == lines2[pos2 + 1].line){
			count += 1;
			pos1 += 2;
			pos2 += 2;
		} else if(lines1[pos1].line == lines2[pos2 + 1].line){
			count += 1;
			pos2 += 1;
		} else if(lines1[pos1 + 1].line == lines2[pos2].line){
			count += 1;
			pos1 += 1;
		} else if(pos2 < size2 - 2 && lines1[pos1].line == lines2[pos2 + 2].line){
			count += 2;
			pos2 += 2;
		} else if(pos1 < size1 - 2 && lines1[pos1 + 2].line == lines2[pos2].line){
			count += 2;
			pos1 += 2;
		} else{
			return 0;
		}
		if(pos1 >= size1 - 1 || pos2 >= size2 - 1){
			return 0;
		}
	}
	return 100 - ((count * 100) / min);
}

int calculateSimilarity(Content lines1, Content lines2){
	int size1 = size(lines1);
	int size2 = size(lines2);
	int similarity = 0;
	if(size1 == size2){
		similarity = simpleComparison(lines1, lines2);
	} else if(size1 - size2 <= 5 && size2 - size1 <= 5){
		similarity = complexComparison(lines1, lines2);
	}
	return similarity;
}


Clone type3Detection(MethodContent methods){
	Clone found = {};
	methods = (f: replaceSIDnames(methods[f]) | f <- methods);
	for(f1 <- methods){
		for(f2 <- methods, f2 != f1){
			int similarity = calculateSimilarity(methods[f1], methods[f2]);
			if(similarity >= 70){
				found += <f1, f2, type3(), similarity>;
				found += <f2, f1, type3(), similarity>;
			}
		}
	}
	return found;
}