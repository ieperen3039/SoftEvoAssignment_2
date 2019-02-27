module steps::astbased::FileParser

import lang::java::m3::AST;
import IO;
import Set;
import List;
import Node;

map[loc,Declaration] parseFiles(loc dir) 
  = (file : createAstFromFile(file, false) | file <- dir.ls) when exists(dir), isDirectory(dir);
  
bool couldSubTreeBeAClone(\method(_,_,_,_, Statement impl)) = impl.src.end.line - impl.src.begin.line + 1 > 6;
bool couldSubTreeBeAClone(\constructor(_,_,_, Statement impl)) = impl.src.end.line - impl.src.begin.line + 1 > 6;
default bool couldSubTreeBeAClone(Declaration d) = d.src.end.line - d.src.begin.line > 6;
bool couldSubTreeBeAClone(Statement stat) = stat.src.end.line - stat.src.begin.line > 6;
bool couldSubTreeBeAClone(Expression expr) = expr.src.end.line - expr.src.begin.line > 6;

@doc {
  Remove the annotations from a declaration
}
Declaration removeAnnotations(Declaration decl) { 
  if (decl has modifiers, a:annotation(_) <- decl.modifiers) {
    decl.modifiers -= [a]; 
  }
  
  return decl;
}

@doc {
  Recursively reset all the attached src locations to their original values (|unknown:///|)
}
Declaration removeSourceLocations(Declaration decl) = unsetRec(decl, "src");