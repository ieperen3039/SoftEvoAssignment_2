module StringBasedCloneDetector

import DataTypes;
import steps::stringbased::FileReader;

import IO;

void detectClonesUsingStringsOnSmallSet() = detectClonesUsingStrings(|project://assignment2/data/small|);
void detectClonesUsingStringsOnLargeSet() = detectClonesUsingStrings(|project://assignment2/data/large|);

void detectClonesUsingStrings(loc dataDir) {
   MethodContent origMethods = readFiles(dataDir);

  // TODO: implement some string based clone detection technique.
  //   HINT: Try to decompose in seperate steps (like in the previous assignment)
}