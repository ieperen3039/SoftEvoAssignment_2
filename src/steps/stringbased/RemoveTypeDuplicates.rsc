module steps::stringbased::RemoveTypeDuplicates

import DataTypes;

import List;

Clone RemoveTypeDuplicates(Clone typeone, Clone typetwo, Clone typethree){
	MatchedIDs finalIDs = {};
	Clone final = {};
	for(<f1, f2, typedef, similarity> <- typeone){
		finalIDs += <f1, f2>;
		final += <f1, f2, typedef, similarity>;
	}
	
	for(<f1, f2, typedef, similarity> <- typetwo){
		if(<f1, f2> notin finalIDs){
			final += <f1, f2, typedef, similarity>;
			finalIDs += <f1, f2>;
		}
	}
	
	for(<f1, f2, typedef, similarity> <- typethree){
		if(<f1, f2> notin finalIDs){
			final += <f1, f2, typedef, similarity>;
		}
	}
	
	return final;
}