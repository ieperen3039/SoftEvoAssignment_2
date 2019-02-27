module steps::stringbased::FileReader

import IO;
import String;
import List;
import Map;
import lang::java::m3::AST;

alias MethodContent = map[loc,Content];
alias Content = lrel[loc nr, str line];

MethodContent readFiles(loc dir) = (() | it + readAndSplitInMethods(file) | file <- dir.ls) when exists(dir), isDirectory(dir);
default MethodContent readFiles(loc dir) { throw "Unable to read files in dir \'<dir>\', does it exists and is it an directory?"; }

MethodContent readAndSplitInMethods(loc file) {
  Content wholeFile = readAndAnnotateFile(file);
  
  Content selectPart(loc fragment) 
    = [ln | ln <- wholeFile, ln.nr.begin.line >= fragment.begin.line && ln.nr.end.line <= fragment.end.line];
  
  Declaration root = createAstFromFile(file,false);
  set[Declaration] mAndC = {m | /m:method(_,_,_,_,_) := root} + {c | /c:constructor(_,_,_,_) := root};
  
  return (decl.src : selectPart(decl.src) | Declaration decl <- mAndC);
}

Content readAndAnnotateFile(loc file) {
  int offset = 0;
  list[str] lines = split("\n",readFile(file));

  Content result = [];
  
  for (int i <- [0..size(lines)]) {
    str current = lines[i];
    int length = size(current);
    
    result += <file[offset=offset][length=length][begin = <i+1,0>][end = <i+1,length>], current>;
    offset += length + 1;
  }  
  
  return result;
}

bool couldFragmentBeAClone(Content fragment) = fragment[-1].nr.end.line - fragment[0].nr.begin.line >= 5 when size(fragment) > 0;
default bool couldFragmentBeAClone(Content fragment) { throw "Unable to determine whether fragment could be a clone according to the definition"; }