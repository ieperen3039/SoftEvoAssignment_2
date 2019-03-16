module StringBasedCloneDetector

import DataTypes;
import steps::stringbased::FileReader;
import steps::stringbased::Filtering;
import steps::stringbased::Type1Detection;
import steps::stringbased::Type2Detection;
import steps::stringbased::Type3Detection;
import steps::stringbased::RemoveTypeDuplicates;
import steps::evaluation::ResultsCalculator;

import IO;
import Map;
import String;
import List;
import GoldenStandardReader;

void detectClonesUsingStringsOnSmallSet() = detectClonesUsingStrings(|project://assignment2/data/small|);
void detectClonesUsingStringsOnLargeSet() = detectClonesUsingStrings(|project://assignment2/data/large|);

str createOutput(Clone input){
	str output = "fragment1,fragment2,cloneType,lineSimilarity\n";
	for(<f1, f2, typedef, similarity> <- input){
		output += "\"<f1>\",\"<f2>\",<typedef>,<similarity>\n";
	}
	return output;
}

Clone typeDetection(MethodContent methods){
	int total = size(methods);
	int count = 0;
	int percent = 0;
	Clone found = {};
	print("         0% |          |");
	for(f1 <- methods){
		for(f2 <- methods, f2 != f1){
			if(type1Detection(methods[f1], methods[f2])){
				found += <f1, f2, type1(), 100>;
				found += <f2, f1, type1(), 100>;
			} else if(type2Detection(methods[f1], methods[f2])){
				found += <f1, f2, type2(), 100>;
				found += <f2, f1, type2(), 100>;
			}
		}
		count += 1;
		if(count % ((total / 10) + 1) == 0){
			percent += 1;
			print("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b");
			if(percent < 100){
				print(" ");
			}
			print("<percent * 10>% |");
			for(i <- [0..percent]){
				print("█");
			}
			for(i <- [0..10-percent]){
				print(" ");
			}
			print("|");
		}
	}
	print("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b");
	print(" 100% |██████████|\n");
	return found;
}


void detectClonesUsingStrings(loc dataDir) {
	println("Step 1: Reading files");
	MethodContent origMethods = readFiles(dataDir);
	
	println("Step 2: Filtering methods");
	MethodContent filtered = filterMethods(origMethods);
	
	// Turns out detecting all types and then trowing away the duplicates
	// is much faster than the method commented out below
	//println("Step 3: Type detection");
	//Clone typedetection = typeDetection(filtered);
	
	println("Step 3: Type1 detection");
	Clone typeone = type1Detection(filtered);
	
	println("Step 4: Type2 detection");
	Clone typetwo = type2Detection(filtered);

	println("Step 5: Type3 detection");
	Clone typethree = type3Detection(filtered);
	
	println("Step 6: Duplicate removal");
	Clone final = RemoveTypeDuplicates(typeone, typetwo, typethree);
	
	println("Step 7: Creating output file");
	writeFile(|project://assignment2/data/result.csv|, createOutput(final));
	
	println("Step 8: Showing results");
	Clone goldenresults = readGoldenStandardLarge();
	calculateConfusionMatrix(final, goldenresults);
	
	
	
	
	rel[loc f1, loc f2] golden = {};
	goldenresults = readGoldenStandardSmall();
	for(<f1, f2, typedef, similarity> <- goldenresults, typedef == type3()){
		golden += <f1, f2>;
		//println("<f1>, <f2>, <similarity>");
	}
	
	rel[loc f1, loc f2] typeIDs = {};
	for(<f1, f2, typedef, similarity> <- typethree){
		typeIDs += <f1, f2>;
	}
	
	// Print what is in the golden standard but not found
	for(<f1, f2> <- golden){
		if(<f1, f2> notin typeIDs){
			//println("<f1>, <f2>");
			;
		}
	}
	
	
	
	
	// Simple output writing to use in a diff tool with the given result data set
	//writeFile(|project://assignment2/data/result.csv|, createOutput(type1));
	
	
	//rel[loc f1, loc f2] typeoneIDs = {};
	//for(<f1, f2, typedef, similarity> <- typeone){
		//typeoneIDs += <f1, f2>;
	//}
	
	//println("<typeoneIDs>");
	
	
	
	//println("Step 4: Type2 detection");
	//Clone typetwo = type2Detection(filtered);
	
	//Clone finaltypetwo = {};
	//for(<f1, f2, typedef, similarity> <- typetwo){
			//if(<f1, f2> notin typeoneIDs){
				//finaltypetwo += <f1, f2, typedef, similarity>;
			//}
	//}
	
	//rel[loc f1, loc f2] typetwoIDs = {};
	//for(<f1, f2, typedef, similarity> <- typetwo){
		//typetwoIDs += <f1, f2>;
	//}
	
	//for(<f1, f2, typedef, similarity> <- finaltypetwo){
		//println("<f1>, <f2>");
	//}
	//writeFile(|project://assignment2/data/resultrec.csv|, createOutput(finaltypetwo));
	
	//println("Post processing");
	
	
	
	//smallresults = readGoldenStandardSmall();
	//for(<f1, f2, typedef, similarity> <- smallresults, typedef == type3()){
		//println("<f1>, <f2>, <similarity>");
	//}
	
	
	
	//rel[loc f1, loc f2] golden = {};
	//int sizegolden = 0;
	//largeresults = readGoldenStandardLarge();
	//for(<f1, f2, typedef, similarity> <- largeresults, typedef == type3()){
		//golden += <f1, f2>;
		//sizegolden += 1;
	//}
	
	// Print what in the golden standard but not found
	//for(<f1, f2> <- golden){
		//if(<f1, f2> notin typetwoIDs){
			//println("<f1>, <f2>");
			//;
		//}
	//}
	
	
	//int sizeone = 0;
	
	// Print what found but no in the standard
	//int count = 0;
	//for(<f1, f2, typedef, similarity> <- finaltypetwo){
		//sizeone += 1;
		//if(<f1, f2> notin golden){
			//println("<f1>, <f2>");
			//count += 1;
		//}
	//}
	//println("<sizeone> - <sizegolden> should be <count>");
	
	
	
	

}




