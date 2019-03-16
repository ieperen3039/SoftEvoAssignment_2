module steps::stringbased::Filtering

import steps::stringbased::FileReader;
import String;
import Map;
import List;


bool removeLine(str line){
	return	startsWith(line, "/*") ||
			startsWith(line, "*") ||
			startsWith(line, "//") ||
			isEmpty(line) ||
			// This is removing elements from the method self. Is this correct ?? Investigate
			//startsWith(line, "@") ||
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

bool couldFragmentBeAClone(Content fragment) = size(fragment) >= 6;

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