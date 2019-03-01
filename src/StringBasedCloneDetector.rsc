module StringBasedCloneDetector

import DataTypes;
import steps::stringbased::FileReader;

import IO;
import Map;
import String;
import List;

void detectClonesUsingStringsOnSmallSet() = detectClonesUsingStrings(|project://assignment2/data/small|);
void detectClonesUsingStringsOnLargeSet() = detectClonesUsingStrings(|project://assignment2/data/large|);


Content trimLines(Content lines) {
	// Remove each line consisting of comments and for each line remove the trim
	Content trimmed = [];
	for(<loc nr, str line> <- lines){
		str temp = trim(line);
		if(!(startsWith(temp, "/*") || startsWith(temp, "*") || startsWith(temp, "//") || isEmpty(temp))){
			trimmed += <nr, temp>;
		}
	}
	
	return trimmed;
}


MethodContent filterContents(MethodContent methods){
	list[loc] filtered = [];
	// Take all trimmed fragments which could be a clone (so have the appropriate length)
	for(f <- methods, couldFragmentBeAClone(trimLines(methods[f]))){
		filtered += f;
	}
	// Return those fragments with trimmed lines
	return (f : trimLines(methods[f]) | f <- filtered);
}

str combineFragment(Content fragment){
	str combine = "";
	map[str, str] remove = (" " : "");
	for(f <- fragment){
		combine += escape(f.line, remove);
	}
	return combine;
}

int stringCompare(str f1, str f2){
	// Seeing the fact that each type1 declaration in the golden set is of 100%
	// And the fact that if a type1 is not 100%, it most likely is type2 or 3
	// This routine calculating the difference in percentage is not used by the type1Detection() function
	int errors = 0;
	int size1 = size(f1);
	int size2 = size(f2);
	int min = size1;
	if(size2 < min){
		min = size2;
	}
	min -= 1;
	for(int i <- [0 .. min]){
		if(charAt(f1, i) != charAt(f2, i)){
			errors += 1;
		}
	}
	return ((min - errors) / min) * 100;
}

Clone type1Detection(MethodContent methods){
	Clone found = {};
	map[loc, str] combined = (f: combineFragment(methods[f]) | f <- methods);
	int count = 0;
	for(f1 <- combined){
		for(f2 <- combined, f2 != f1){
			//int similarity = stringCompare(combined[f1], combined[f2]);
			//if(similarity >= 50){
				//println("<f1>, <f2> <similarity>");
				//count += 1;
			//}
			// See comment of stringCompare() function
			// Besides that is it much faster
			if(combined[f1] == combined[f2]){
				found += <f1, f2, type1(), 100>;
				found += <f2, f1, type1(), 100>;
			}
		}
	}
	return found;
}

str createOutput(Clone input){
	str output = "fragment1,fragment2,cloneType,lineSimilarity\n";
	for(<f1, f2, typedef, similarity> <- input){
		output += "\"<f1>\",\"<f2>\",<typedef>,<similarity>\n";
	}
	return output;
}


void detectClonesUsingStrings(loc dataDir) {
	println("Step 1: Reading files");
	MethodContent origMethods = readFiles(dataDir);
	println("Step 2: Filtering methods");
	MethodContent filtered = filterContents(origMethods);
	println("Step 3: Type1 detection");
	Clone type1 = type1Detection(filtered);
	// Simple output writing to use in a diff tool with the given result data set
	writeFile(|project://assignment2/data/result.csv|, createOutput(type1));
	
}