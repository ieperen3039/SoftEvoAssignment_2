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

loc tempfname1 = |project://assignment2/data/resultrec.csv|;
loc tempfname2 = |project://assignment2/data/resultrec.csv|;

Content replaceFuncname(Content lines){
	// Replace the function name to "funcname"
	Content replaced = lines;
	int end = findFirst(replaced[0].line, "(");
	int begin = findLast(substring(replaced[0].line, 0, end), " ") + 1;
	str funcname = substring(replaced[0].line, begin, end);
	replaced[0].line = replaceFirst(replaced[0].line, funcname, "funcname");
	int sizeReplaced = size(replaced);
	
	
	int retBegin = findLast(substring(replaced[0].line, 0, begin - 1), " ") + 1;
	if(retBegin > 0){
		str retname = substring(replaced[0].line, retBegin, begin - 1);
		replaced[0].line = replaceFirst(replaced[0].line, retname, "ret");
	}
	
	// If the function is recursive, also replace the calls to its own
	for(int i <- [1..sizeReplaced]){
		if(startsWith(replaced[i].line, funcname)){
			replaced[i].line = replaceAll(replaced[i].line, funcname, "funcname");
		}
	}
	
	if(funcname == "encodeFileToFile"){
		tempfname1 = replaced[0].nr;
	}
	if(funcname == "decodeFileToFile"){
		tempfname2 = replaced[0].nr;
	}
	
	return replaced;
}

Content replaceFuncvariables(Content lines){
	// Replace the function input variables to "funcvarX" where X is a number
	Content replaced = lines;
	int begin = findFirst(replaced[0].line, "(") + 1;
	int end = findFirst(replaced[0].line, ")");
	if(begin != end){
		str funcvars = substring(replaced[0].line, begin, end);
		list[str] vars = split(",", funcvars);
		int count = 0;
		str funcvarsrep = "";
		for(v <- vars){
			count += 1;
			str var = trim(substring(v, findLast(v, " ")));
			str rep = "fvar<count>";
			if(funcvarsrep != ""){
				funcvarsrep += ", ";
			}
			funcvarsrep += "type <rep>";
			int count2 = 0;
			for(<nr, l> <- replaced){
				if(count2 > 0){
					replaced[count2].line = replaceFirst(l, var, rep);
				}
				count2 += 1;
			}
		}
		replaced[0].line = replaceFirst(replaced[0].line, funcvars, funcvarsrep);
	}
	return replaced;
}

Content replaceFunctionExeptions(Content lines){
	// Replace the functions thrown exceptions to excep
	Content replaced = lines;
	int begin = findFirst(replaced[0].line, "throws");
	if(begin > 0){
		int end = findFirst(replaced[0].line, "{");
		list[str] exceps = split(",", trim(substring(replaced[0].line, begin + 6, end)));
		for(exp <- exceps){
			replaced[0].line = replaceAll(replaced[0].line, exp, "exp");
		}
	}
	return replaced;
}

Content replaceMethodvariables(Content lines){
	// Replace the method variables to "varX" where X is a number
	Content replaced = lines;

	return replaced;
}

Content replaceKeywords(Content lines){
	Content replaced = lines;
	int j = 0;
	for(<nr, l> <- replaced){
		if(toLowerCase(replaced[j].line) != replaced[j].line){
			int i;
			sizeline = size(replaced[j].line);
			int count = 0;
			str result = replaced[j].line;
			for(i <- [0..sizeline]){
				int number = charAt(replaced[j].line, i);
				if(number >= 65 && number <= 90 || number == 95){
					int prev;
					if(i > 0){
						 prev = charAt(replaced[j].line, i - 1);
					} else{
						prev = 0;
					}
					if((prev < 48 || prev > 57) && (prev < 97 || prev > 122)){
						count += 1;
					}
				} else{
					if((number < 48 || number > 57) && (number < 97 || number > 122)){
						if(count > 1){
							str rep = substring(replaced[j].line, i - count, i);
							result = replaceAll(result, rep, "keyword");
						}
					}
					count = 0;
				}
			}
			replaced[j].line = result;
		}
		j += 1;
	}
	return replaced;
}

Content replaceMethodFuncCalls(Content lines){
	// Replace the method variables to "varX" where X is a number
	Content replaced = lines;
	int sizeReplaced = size(replaced);
	int count = 0;
	for(int i <- [1..sizeReplaced]){
		int begin = findFirst(replaced[i].line, "(");
		if(begin > 0){
			count += 1;
			replaced[i].line = replaceAll(replaced[i].line, substring(replaced[i].line, 0, begin), "funccall<count>");
		}
	}
	return replaced;
}

Content replaceMethodStrings(Content lines){
	Content replaced = lines;
	int sizeReplaced = size(replaced);
	int count = 0;
	for(int i <- [1..sizeReplaced]){
		int begin = findFirst(replaced[i].line, "\"");
		if(begin > 0){
			count += 1;
			int end = findLast(replaced[i].line, "\"");
			replaced[i].line = replaceAll(replaced[i].line, substring(replaced[i].line, begin + 1, end), "string<count>");
		}
	}
	return replaced;
}

Content replaceSIDnames(Content lines){
	Content replaced = replaceFuncname(lines);
	replaced = replaceFuncvariables(replaced);
	replaced = replaceFunctionExeptions(replaced);
	replaced = replaceMethodvariables(replaced);
	replaced = replaceMethodFuncCalls(replaced);
	replaced = replaceMethodStrings(replaced);
	replaced = replaceKeywords(replaced);
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
			if(methods[f1][0].nr == tempfname1 && methods[f2][0].nr == tempfname2){
				println("<combined[f1]>");
				println("<combined[f2]>");
				println("");
			}
		}
	}
	return found;
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
	
	//for(<f1, f2, typedef, similarity> <- finaltypetwo){
		//println("<f1>, <f2>");
	//}
	writeFile(|project://assignment2/data/resultrec.csv|, createOutput(finaltypetwo));
	
	
	println("Post processing");
	
	compareResults(typetwo, readGoldenStandardLarge());
}




