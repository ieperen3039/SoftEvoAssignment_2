module StringBasedCloneDetector

import DataTypes;
import steps::stringbased::FileReader;

import IO;
import Map;
import String;
import List;
import GoldenStandardReader;

void detectClonesUsingStringsOnSmallSet() = detectClonesUsingStrings(|project://assignment2/data/small|);
void detectClonesUsingStringsOnLargeSet() = detectClonesUsingStrings(|project://assignment2/data/large|);


bool removeLine(str line){
	return	startsWith(line, "/*") ||
			startsWith(line, "*") ||
			startsWith(line, "//") ||
			startsWith(line, "@") ||
			isEmpty(line) ||
			// This is removing elements from the method self. Is this correct ?? Investigate
			line == "}" ||
			line == "{";
}

Content trimLines(Content lines) {
	// Remove each line consisting of comments and for each line remove the trim
	Content trimmed = [];
	for(<loc nr, str line> <- lines){
		str temp = trim(line);
		if(!removeLine(temp)){
			trimmed += <nr, temp>;
		}
	}
	return trimmed;
}

bool couldFragmentBeAClone(Content fragment) = size(fragment) >= 5;

MethodContent filterMethods(MethodContent methods){
	MethodContent filtered = ();
	// Take all trimmed fragments which could be a clone (so have the appropriate length)
	for(f <- methods){
		Content temp = trimLines(methods[f]);
		if(size(temp) > 1 && couldFragmentBeAClone(temp[1..])){
			filtered += (f : temp);
		}
	}
	return filtered;
}

str combineFragment(Content fragment){
	str combine = "";
	map[str, str] remove = (" " : "", "\r" : "");
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
	for(f1 <- combined){
		for(f2 <- combined, f2 != f1){
			//int similarity = stringCompare(combined[f1], combined[f2]);
			//if(similarity >= 50){
				//println("<f1>, <f2> <similarity>");
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

Content replaceFuncname(Content lines){
	// Replace the function name to "funcname"
	Content replaced = lines;
	int sizeline = size(replaced[0].line);
	int end = findFirst(replaced[0].line, "(");
	int begin = findLast(substring(replaced[0].line, 0, end), " ") + 1;
	str funcname = substring(replaced[0].line, begin, end);
	replaced[0].line = replaceFirst(replaced[0].line, funcname, "funcname");
	int sizeReplaced = size(replaced);
	
	// If the function is recursive, also replace the calls to its own
	for(int i <- [1..sizeReplaced]){
		if(startsWith(replaced[i].line, funcname)){
			replaced[i].line = replaceAll(replaced[i].line, funcname, "funcname");
		}
	}
	return replaced;
}

Content replaceFuncvariables(Content lines){
	// Replace the function input variables to "funcvarX" where X is a numer
	Content replaced = lines;

	return replaced;
}

Content replaceMethodvariables(Content lines){
	// Replace the method variables to "varX" where X is a numer
	Content replaced = lines;

	return replaced;
}

Content replaceSIDnames(Content lines){
	Content replaced = replaceFuncname(lines);
	replaced = replaceFuncvariables(replaced);
	replaced = replaceMethodvariables(replaced);
	return replaced;
}

Clone type2Detection(MethodContent methods){
	Clone found = {};
	map[loc, str] combined = (f: combineFragment(replaceSIDnames(methods[f])) | f <- methods);
	for(f1 <- combined){
		for(f2 <- combined, f2 != f1){
			if(combined[f1] == combined[f2]){
				found += <f1, f2, type2(), 100>;
				found += <f2, f1, type2(), 100>;
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
	MethodContent filtered = filterMethods(origMethods);
	//println("<filtered>");
	//writeFile(|project://assignment2/data/result.csv|, "<filtered>");
	println("Step 3: Type1 detection");
	Clone typeone = type1Detection(filtered);
	// Simple output writing to use in a diff tool with the given result data set
	//writeFile(|project://assignment2/data/result.csv|, createOutput(type1));
	
	
	rel[loc f1, loc f2] typeoneIDs = {};
	for(<f1, f2, typedef, similarity> <- typeone){
		typeoneIDs += <f1, f2>;
	}
	
	//println("<typeoneIDs>");
	
	
	
	println("Step 4: Type2 detection");
	Clone typetwo = type2Detection(filtered);
	Clone finaltypetwo = {};
	for(<f1, f2, typedef, similarity> <- typetwo){
			if(<f1, f2> notin typeoneIDs){
				finaltypetwo += <f1, f2, typedef, similarity>;
			}
	}
	
	rel[loc f1, loc f2] typeteoIDs = {};
	for(<f1, f2, typedef, similarity> <- typetwo){
		typeteoIDs += <f1, f2>;
	}
	
	//for(<f1, f2, typedef, similarity> <- finaltypetwo){
		//println("<f1>, <f2>");
	//}
	//writeFile(|project://assignment2/data/resultrec.csv|, createOutput(finaltypetwo));
	
	
	
	
	
	
	
	rel[loc f1, loc f2] golden = {};
	int sizegolden = 0;
	Clone largeresults = readGoldenStandardLarge();
	for(<f1, f2, typedef, similarity> <- largeresults, typedef == type2()){
		golden += <f1, f2>;
		sizegolden += 1;
	}
	
	
	int sizeone = 0;
	
	int count = 0;
	for(<f1, f2, typedef, similarity> <- finaltypetwo){
		sizeone += 1;
		if(<f1, f2> notin golden){
			//println("<f1>, <f2>");
			count += 1;
		} else {
			println("<f1>, <f2>");
		}
	}
	println("<sizeone> - <sizegolden> should be <count>");
	
	
	
	

}




