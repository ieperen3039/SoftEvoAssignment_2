module DataTypes


data CloneType
  = type1()
  | type2()
  | type3()
  ;

alias Clone = rel[loc fragment1, loc fragment2, CloneType cloneType, int lineSimilarity];

