module DataTypes
import IO;


data CloneType
  = type1()
  | type2()
  | type3()
  ;

alias Clone = rel[loc fragment1, loc fragment2, CloneType cloneType, int lineSimilarity];

bool isLess(CloneType a, CloneType b) = 
	(type1() := a && type2() := b) || (type1() := a && type3() := b) || (type2() := a && type3() := b);
	
str createOutput(Clone input){
	str output = "fragment1,fragment2,cloneType,lineSimilarity\n";
	for(<f1, f2, typedef, similarity> <- input){
		output += "\"<f1>\",\"<f2>\",<typedef>,<similarity>\n";
	}
	return output;
}

// currently only checks type 2
void compareResults(Clone clones, Clone largeresults){
	println("\nComparing results with golden standard:");
	rel[loc f1, loc f2] typetwoIDs = {};
	
	rel[loc f1, loc f2, CloneType typedef] golden = {};
	int sizegolden = 0;
	for(<f1, f2, typedef, similarity> <- largeresults){
		golden += <f1, f2, typedef>;
		sizegolden += 1;
	}
	
	int invalids = 0;
	int misseds = 0;
	int cloneSize = 0;
	// Print what found but no in the standard
	for(<f1, f2, typedef, similarity> <- clones){
		if(<f1, f2, typedef> notin golden){
			println("misclassified <typedef>, <f1>, <f2>");
			invalids += 1;
		}
		
		cloneSize += 1;
	}

	// Print what in the golden standard but not found
	for (<f1, f2, typedef> <- golden){	
		if(<f1, f2, typedef> notin clones){
			println("missed <typedef>, <f1>, <f2>");
			misseds += 1;
		}
	}
	
	println("found <cloneSize> clones, of which <invalids> were incorrect and missing <misseds>");
}
	