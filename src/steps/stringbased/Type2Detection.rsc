module steps::stringbased::Type2Detection

import steps::stringbased::FileReader;
import steps::stringbased::CombineFragment;
import steps::stringbased::FunctionReformating;
import DataTypes;

import String;
import List;

//bool type2Detection(Content method1, Content method2){
//	return combineFragment(replaceSIDnames(method1)) == combineFragment(replaceSIDnames(method2));
//}

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