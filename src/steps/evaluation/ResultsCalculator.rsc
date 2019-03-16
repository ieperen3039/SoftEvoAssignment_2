module steps::evaluation::ResultsCalculator

import DataTypes;

import Set;
import List;
import util::Math;
import IO;

void calculateConfusionMatrix(Clone calculated, Clone golden) {
  list[int] count = [0 | i <- [0..15]];
  
  MatchedIDs goldentype1 = {};
  for(<f1, f2, typedef, similarity> <- golden, typedef == type1()){
  	goldentype1 += <f1, f2>;
  }
  
  MatchedIDs goldentype2 = {};
  for(<f1, f2, typedef, similarity> <- golden, typedef == type2()){
  	goldentype2 += <f1, f2>;
  }
  
  MatchedIDs goldentype3 = {};
  for(<f1, f2, typedef, similarity> <- golden, typedef == type3()){
  	goldentype3 += <f1, f2>;
  }
  
  MatchedIDs calculatedtype1 = {};
  for(<f1, f2, typedef, similarity> <- calculated, typedef == type1()){
  	calculatedtype1 += <f1, f2>;
  }
  
  MatchedIDs calculatedtype2 = {};
  for(<f1, f2, typedef, similarity> <- calculated, typedef == type2()){
  	calculatedtype2 += <f1, f2>;
  }
  
  MatchedIDs calculatedtype3 = {};
  for(<f1, f2, typedef, similarity> <- calculated, typedef == type3()){
  	calculatedtype3 += <f1, f2>;
  }
  
  for(<f1, f2> <- goldentype1){
  	if(<f1, f2> in calculatedtype1){
  		count[0] += 1;
  	} else if(<f1, f2> in calculatedtype2){
  		count[1] += 1;
  	} else if(<f1, f2> in calculatedtype3){
  		count[2] += 1;
  	} else {
  		count[3] += 1;
  	}
  }
  
  for(<f1, f2> <- goldentype2){
  	if(<f1, f2> in calculatedtype1){
  		count[4] += 1;
  	} else if(<f1, f2> in calculatedtype2){
  		count[5] += 1;
  	} else if(<f1, f2> in calculatedtype3){
  		count[6] += 1;
  	} else {
  		count[7] += 1;
  	}
  }
  
  for(<f1, f2> <- goldentype3){
  	if(<f1, f2> in calculatedtype1){
  		count[8] += 1;
  	} else if(<f1, f2> in calculatedtype2){
  		count[9] += 1;
  	} else if(<f1, f2> in calculatedtype3){
  		count[10] += 1;
  	} else {
  		count[11] += 1;
  	}
  }
  
  for(<f1, f2> <- calculatedtype1){
  	if(<f1, f2> notin goldentype1 && <f1, f2> notin goldentype2 &&<f1, f2> notin goldentype3){
  		count[12] += 1;
  	}
  }
  
  for(<f1, f2> <- calculatedtype2){
  	if(<f1, f2> notin goldentype1 && <f1, f2> notin goldentype2 &&<f1, f2> notin goldentype3){
  		count[13] += 1;
  	}
  }
  
  for(<f1, f2> <- calculatedtype3){
  	if(<f1, f2> notin goldentype1 && <f1, f2> notin goldentype2 &&<f1, f2> notin goldentype3){
  		count[14] += 1;
  	}
  }
  
  real recallt1 = precision(toReal(size(calculatedtype1)) / toReal(size(goldentype1)) * 100, 5);
  real recallt2 = precision(toReal(size(calculatedtype2)) / toReal(size(goldentype2)) * 100, 5);
  real recallt3 = precision(toReal(size(calculatedtype3)) / toReal(size(goldentype3)) * 100, 5);
  
  real prest1 = precision(toReal(count[0]) / toReal(size(calculatedtype1)) * 100, 5);
  real prest2 = precision(toReal(count[5]) / toReal(size(calculatedtype2)) * 100, 5);
  real prest3 = precision(toReal(count[10]) / toReal(size(calculatedtype3)) * 100, 5);
  
  real fmeas1 = precision(toReal(2) * ((recallt1 * prest1) / (recallt1 + prest1)), 5);
  real fmeas2 = precision(toReal(2) * ((recallt2 * prest2) / (recallt2 + prest2)), 5);
  real fmeas3 = precision(toReal(2) * ((recallt3 * prest3) / (recallt3 + prest3)), 5);
  
  println("                                               Pridicted by the method");
  println("\t\t\t\t\tType 1\t\tType 2\t\tType 3\t\tNot a clone pair");
  println("\t\tType 1\t\t\t <count[0]>\t\t <count[1]>\t\t <count[2]>\t\t <count[3]>");
  println("Predicted\tType 2\t\t\t <count[4]>\t\t <count[5]>\t\t <count[6]>\t\t <count[7]>");
  println("Manually\tType 3\t\t\t <count[8]>\t\t <count[9]>\t\t <count[10]>\t\t <count[11]>");
  println("\t\tNot a clone pair\t <count[12]>\t\t <count[13]>\t\t <count[14]>");
  println("");
  println("Recall:\t\t\t\t\t <recallt1>\t\t <recallt2>\t\t <recallt3>");
  println("Precision:\t\t\t\t <prest1>\t\t <prest2>\t\t <prest3>");
  println("F-measue:\t\t\t\t <fmeas1>\t\t <fmeas2>\t\t <fmeas3>");
}