module steps::stringbased::Type1Detection


import steps::stringbased::CombineFragment;
import steps::stringbased::FileReader;
import DataTypes;

import String;

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

//bool type1Detection(Content method1, Content method2){
	//int similarity = stringCompare(combined[f1], combined[f2]);
	//if(similarity >= 50){
		//return true;
	//} else{
		//return false;
	//}
	// See comment of stringCompare() function
	// Besides that is it much faster
	
//	return combineFragment(method1) == combineFragment(method2);
//}

Clone type1Detection(MethodContent methods){
	Clone found = {};
	map[loc, str] combined = (f: replaceFirst(combineFragment(methods[f]), "@Override", "") | f <- methods);
	for(f1 <- combined){
		for(f2 <- combined, f2 != f1){
			if(combined[f1] == combined[f2]){
				found += <f1, f2, type1(), 100>;
				found += <f2, f1, type1(), 100>;
			}
		}
	}
	return found;
}