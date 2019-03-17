module steps::stringbased::FunctionReformating

import steps::stringbased::FileReader;
import DataTypes;

import String;
import List;

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
	Content replaced = lines;
	bool hadOverwrite = false;
	Content overwrite;
	if(startsWith(replaced[0].line, "@")){
		overwrite = replaced[0];
		replaced = replaced[1..];
		hadOverwrite = true;
	}
	replaced = replaceFuncname(replaced);
	replaced = replaceFuncvariables(replaced);
	replaced = replaceFunctionExeptions(replaced);
	replaced = replaceMethodvariables(replaced);
	replaced = replaceMethodFuncCalls(replaced);
	replaced = replaceMethodStrings(replaced);
	replaced = replaceKeywords(replaced);
	if(hadOverwrite){
		replaced = overwrite + replaced;
	}
	return replaced;
}