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

void detectClonesUsingStringsOnSmallSet() = detectClonesUsingStrings(|project://assignment2/data/small|, "small");
void detectClonesUsingStringsOnLargeSet() = detectClonesUsingStrings(|project://assignment2/data/large|, "large");

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


void detectClonesUsingStrings(loc dataDir, str indicator) {
	println("(1/8) Reading files");
	MethodContent origMethods = readFiles(dataDir);
	
	println("(2/8) Filtering methods");
	MethodContent filtered = filterMethods(origMethods);
	
	// Turns out detecting all types and then trowing away the duplicates
	// is much faster than the method commented out below
	//println("Step 3: Type detection");
	//Clone typedetection = typeDetection(filtered);
	
	println("(3/8) Type1 detection");
	Clone typeone = type1Detection(filtered);
	
	println("(4/8) Type2 detection");
	Clone typetwo = type2Detection(filtered);

	println("(5/8) Type3 detection");
	Clone typethree = type3Detection(filtered);
	
	println("(6/8) Duplicate removal");
	Clone final = RemoveTypeDuplicates(typeone, typetwo, typethree);
	
	println("(7/8) Creating output file");
	writeFile(|project://assignment2/data/result.csv|, createOutput(final));
	
	println("(8/8) Showing results");
	Clone goldenresults;
	if(indicator == "small"){
		goldenresults= readGoldenStandardSmall();
	} else{
		goldenresults = readGoldenStandardLarge();
	}
	calculateConfusionMatrix(final, goldenresults);
}