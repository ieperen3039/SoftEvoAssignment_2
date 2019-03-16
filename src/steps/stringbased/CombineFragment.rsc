module steps::stringbased::CombineFragment

import steps::stringbased::FileReader;

import String;
import Map;

str combineFragment(Content fragment){
	str combine = "";
	map[str, str] remove = (" " : "", "\r" : "");
	for(f <- fragment){
		combine += escape(f.line, remove);
	}
	return combine;
}