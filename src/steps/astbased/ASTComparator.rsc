module steps::astbased::ASTComparator

import util::Math;
import DataTypes;
import lang::java::m3::AST;
import IO;
import List;

alias CloneReport = tuple[CloneType ctype, int numCloneElts, int totalElts];
CloneReport emptyClone = <type1(), 0, 0>;

CloneReport findClones(\parameter(Type t1, str name1, int dim1), \parameter(Type t2, str name2, int dim2)){
	return <(t1 == t2 && name1 == name2 && dim1 == dim2) ? type1() : type2(), 3, 3>;
}
	
CloneReport findClones(\variables(Type type1, list[Expression] frag1) ,\variables(Type type2, list[Expression] frag2)){
	return combine(findClones(type1, type2), findClones(frag1, frag2));
}

CloneReport findClones(\arrayAccess(Expression a1, Expression i1), \arrayAccess(Expression a2, Expression i2)) = 
	combine(findClones(a1, a2), findClones(i1, i2));
	
CloneReport findClones(\newArray(Type type1, list[Expression] dim1, Expression init1), 
	\newArray(Type type2, list[Expression] dim2, Expression init2)) = 
	combine(combine((init1 == init2) ? type1() : type2(), findClones(type1, type2)), findClones(dim1, dim2));
	
CloneReport findClones(\newArray(Type type1, list[Expression] dim1), \newArray(Type type2, list[Expression] dim2)) = 
	combine(findClones(type1, type2), findClones(dim1, dim2));
	
CloneReport findClones(\arrayInitializer(list[Expression] elts1), \arrayInitializer(list[Expression] elts2)) = findClones(elts1, elts2);
	
CloneReport findClones(\assignment(Expression lhs1, str op1, Expression rhs1), \assignment(Expression lhs2, str op2, Expression rhs2)){
	if (op1 != op2) return <type3(), 0, 1>;
	return combine(findClones(lhs1, lhs2), findClones(rhs1, rhs2));
}
	
CloneReport findClones(\cast(Type type1, Expression exp1), \cast(Type type2, Expression exp2)) = 
	combine(findClones(type1, type2), findClones(exp1, exp2));
	
CloneReport findClones(\newObject(Expression expr1, Type type1, list[Expression] args1, Declaration class1), 
		\newObject(Expression expr2, Type type2, list[Expression] args2, Declaration class2)){	
	reportClonesOf(class1, class2);
	CloneReport typeEq = findClones(type1, type2);
	CloneReport expEq = findClones(expr1, expr2);
	CloneReport argEq = findClones(args1, args2);
	CloneReport classEq = findClones(class1, class2);
	return combine(typeEq, expEq, argEq, classEq);
}

CloneReport findClones(\newObject(Expression expr1, Type type1, list[Expression] args1), 
		\newObject(Expression expr2, Type type2, list[Expression] args2)){
	
	CloneReport typeEq = findClones(type1, type2);
	CloneReport expEq = findClones(expr1, expr2);
	CloneReport argEq = findClones(args1, args2);
	return combine(typeEq, expEq, argEq);
}
	
CloneReport findClones(\newObject(Type type1, list[Expression] args1, Declaration class1), 
		\newObject(Type type2, list[Expression] args2, Declaration class2)){
	CloneReport typeEq = findClones(type1, type2);
	CloneReport argEq = findClones(args1, args2);
	CloneReport classEq = findClones(class1, class2);
	return combine(typeEq, argEq, classEq);
}

CloneReport findClones(\newObject(Type type1, list[Expression] args1), \newObject(Type type2, list[Expression] args2)){	
	return combine(findClones(type1, type2), findClones(args1, args2));
}
	
CloneReport findClones(\qualifiedName(Expression qual1, Expression exp1), \qualifiedName(Expression qual2, Expression exp2)) = 
	combine(findClones(qual1, qual2), findClones(exp1, exp2));

CloneReport findClones(\conditional(Expression guard1, Expression thenBranch1, Expression elseBranch1), 
		\conditional(Expression guard2, Expression thenBranch2, Expression elseBranch2)){
	CloneReport guardEq = findClones(guard1, guard2);
	CloneReport thenEq = findClones(thenBranch1, thenBranch2);
	CloneReport elseEq = findClones(elseBranch1, elseBranch2);

	return combine(guardEq, thenEq, elseEq);
}

CloneReport findClones(\fieldAccess(_, Expression exp1, str name1), \fieldAccess(_, Expression exp2, str name2)) = 
	combine((name1 == name2) ? type1() : type2(), findClones(exp1, exp2));
	
CloneReport findClones(\instanceof(Expression leftSide1, Type rightSide1), \instanceof(Expression leftSide2, Type rightSide2)) = 
	combine((rightSide1 == rightSide2) ? type2() : type3(), findClones(leftSide1, leftSide2));
	
CloneReport findClones(\methodCall(_, str name1, list[Expression] arg1), \methodCall(_, str name2, list[Expression] arg2)) =
	combine((name1 == name2) ? type1() : type2(), findClones(arg1, arg2));
	
CloneReport findClones(\methodCall(_, Expression rec1, str name1, list[Expression] arg1), \methodCall(_, Expression rec2, str name2, list[Expression] arg2)) = 
	combine(combine((name1 == name2) ? type1() : type2(), findClones(arg1, arg2)), findClones(rec1, rec2));
	
CloneReport findClones(\type(Type type1), \type(Type type2)) = findClones(type1, type2);
			
CloneReport findClones(\bracket(Expression exp1), \bracket(Expression exp2)) = findClones(exp1, exp2);
	
CloneReport findClones(\this(Expression exp1), \this(Expression exp2)) = findClones(exp1, exp2);
	
CloneReport findClones(\declarationExpression(Declaration dec1), \declarationExpression(Declaration dec2)) = findClones(dec1, dec2);
	
CloneReport findClones(\infix(Expression lhs1, str op1, Expression rhs1), \infix(Expression lhs2, str op2, Expression rhs2)) = 
	combine(combine((op1 == op2) ? type1() : type2(), findClones(lhs1, lhs2)), findClones(rhs1, rhs2));
	
CloneReport findClones(\postfix(Expression lhs1, str op1), \postfix(Expression lhs2, str op2)) = 
	combine((op1 == op2) ? type1() : type2(), findClones(lhs1, lhs2));
	
CloneReport findClones(\prefix(str op1, Expression rhs1), \prefix(str op2, Expression rhs2)) = 
	combine((op1 == op2) ? type1() : type2(), findClones(rhs1, rhs2));
	
CloneReport findClones(\memberValuePair(str name1, Expression value1), \memberValuePair(str name2, Expression value2)) = 
	combine((name1 == name2) ? type1() : type2(), findClones(value1, value2));

CloneReport findClone(\null(), \null()) = <type1(), 1, 1>;
CloneReport findClone(\this(), \this()) =  <type1(), 1, 1>;
CloneReport findClone(\super(), \super()) =  <type1(), 1, 1>;
CloneReport findClone(\number(str val1), \number(str val1)) =  (val1 == val2) ? <type1(), 1, 1> : <type2(), 1, 1>;
CloneReport findClone(\booleanLiteral(bool val1), \booleanLiteral(bool val1)) =  (val1 == val2) ? <type1(), 1, 1> : <type2(), 1, 1>;
CloneReport findClone(\stringLiteral(str val1), \stringLiteral(str val1)) =  (val1 == val2) ? <type1(), 1, 1> : <type2(), 1, 1>;
CloneReport findClone(\characterLiteral(str val1), \characterLiteral(str val1)) =  (val1 == val2) ? <type1(), 1, 1> : <type2(), 1, 1>;
CloneReport findClone(\simpleName(str val1), \simpleName(str val1)) =  (val1 == val2) ? <type1(), 1, 1> : <type2(), 1, 1>;
			
CloneReport findClone(\variable(str name1, int dim1), \variable(str name2, int dim2)) {
	return combine(
		(name1 == name2) ? <type1(), 1, 1> : <type2(), 1, 1>,
		(dim1 == dim2) ? <type1(), 1, 1> : <type2(), 1, 1>
	);
}
			
CloneReport findClone(\variable(str name1, int dim1, Expression init1), \variable(str name2, int dim2, Expression init2)) {
	return combine((name1 == name2) ? type1() : type2(), 
		combine((dim1 == dim2) ? type1() : type2(), findClones(init1, init2))
	);
}
			
CloneReport findClone(\fieldAccess(_, str name1), \fieldAccess(_, str name1)) {
	return (name1 == name2) ? <type1(), 1, 1> : <type2(), 1, 1>;
}
CloneReport findClones(\assert(Expression exp1), \assert(Expression exp2)) {
	return findClones(exp1, exp2);
}
CloneReport findClones(\assert(Expression exp1, Expression message1), \assert(Expression exp2, Expression message2)) {
	return combine(findClones(exp1, exp2), findClones(message1, message2));
}
CloneReport findClones(\block(list[Statement] st1), \block(list[Statement] st2)) {
	return findClones(st1, st2);
}
CloneReport findClones(\do(Statement body1, Expression cond1), \do(Statement body2, Expression cond2)) {
	return combine(findClones(body1, body2), findClones(cond1, cond2));
}
CloneReport findClones(\foreach(Declaration par1, Expression coll1, Statement body1), 
		\foreach(Declaration par2, Expression coll2, Statement body2)) {
	return combine(findClones(par1, par2), findClones(coll1, coll2), findClones(body1, body2));
}
CloneReport findClones(\for(list[Expression] inits1, Expression cond1, list[Expression] up1, Statement body1), 
		\for(list[Expression] inits2, Expression cond2, list[Expression] up2, Statement body2)) {
	return combine(findClones(inits1, inits2), findClones(cond1, cond2), findClones(up1, up2), findClones(body1, body2));
}
CloneReport findClones(\for(list[Expression] inits1, list[Expression] up1, Statement body1), 
		\for(list[Expression] inits2, list[Expression] up2, Statement body2)) {
	return combine(findClones(inits1, inits2), findClones(up1, up2), findClones(cody1, body2));
}
CloneReport findClones(\if(Expression cond1, Statement then1, Statement else1), \if(Expression cond2, Statement then2, Statement else2)) {
	return combine(findClones(cond1, cond2), findClones(then1, then2), findClones(else1, else2));
}
CloneReport findClones(\if(Expression cond1, Statement then1), \if(Expression cond2, Statement then2)) {
	return combine(findClones(cond1, cond2), findClones(then1, then2));
}
CloneReport findClones(\label(str name1, Statement body1), \label(str name2, Statement body2)) {
	return combine((name1 == name2) ? type1() : type2(), findClones(body1, body2));
}
CloneReport findClones(\return(Expression exp1), \return(Expression exp2)) {
	return findClones(exp1, exp2);
}
CloneReport findClones(\switch(Expression exp1, list[Statement] st1), \switch(Expression exp2, list[Statement] st2)) {
	return combine(findClones(exp1, exp2), findClones(st1, st2));
}
CloneReport findClones(\case(Expression exp1), \case(Expression exp2)) {
	return findClones(exp1, exp2);
}
CloneReport findClones(\synchronizedStatement(Expression lock1, Statement body1), \synchronizedStatement(Expression lock2, Statement body2)) {
	return combine(findClones(lock1, lock2), findClones(body1, body2));
}
CloneReport findClones(\throw(Expression exp1), \throw(Expression exp2)) {
	return findClones(exp1, exp2);
}
CloneReport findClones(\try(Statement body1, list[Statement] claus1), \try(Statement body2, list[Statement] claus2)) {
	return combine(findClones(body1, body2), findClones(claus1, claus2));
}
CloneReport findClones(\try(Statement body1, list[Statement] claus1, Statement fin1), \try(Statement body2, list[Statement] claus2, Statement fin2)) {
	return combine(findClones(body1, body2), findClones(claus1, claus2), findClones(fin1, fin2));
}
CloneReport findClones(\catch(Declaration ex1, Statement body1), \catch(Declaration ex2, Statement body2)) {
	return findClones(body1, body2);
}
CloneReport findClones(\declarationStatement(Declaration dec1), \declarationStatement(Declaration dec2)) {
	return findClones(dec1, dec2);
}
CloneReport findClones(\while(Expression con1, Statement body1), \while(Expression con2, Statement body2)) {
	return combine(findClones(con1, con2), findClones(body1, body2));
}
CloneReport findClones(\expStatement(Expression stmt1), \expStatement(Expression stmt2)) {
	return findClones(stmt1, stmt2);
}
CloneReport findClones(\constructorCall(_, Expression expr1, list[Expression] arg1), \constructorCall(_, Expression expr2, list[Expression] arg2)) {
	return combine(findClones(expr1, expr2), findClones(arg1, arg2));
}
CloneReport findClones(\constructorCall(_, list[Expression] arg1), \constructorCall(_, list[Expression] arg2)) {
	return findClones(arg1, arg2);
}

CloneReport findClone(\break(), \break()) = <type1(), 1, 1>;
CloneReport findClone(\continue(), \continue()) = <type1(), 1, 1>;
CloneReport findClone(\continue(str label), \continue(str label)) =  <type1(), 1, 1>;
CloneReport findClone(\break(str label), \break(str label)) =  <type1(), 1, 1>;
CloneReport findClone(\empty(), \empty()) = <type1(), 1, 1>;
CloneReport findClone(\return(), \return()) = <type1(), 1, 1>;
CloneReport findClone(\defaultCase(), \defaultCase()) = <type1(), 1, 1>;

CloneReport findClones(arrayType(Type t1), arrayType(Type t2)) = findClones(t1, t2);
CloneReport findClones(parameterizedType(Type t1), parameterizedType(Type t2)) = findClones(t1, t2);
CloneReport findClones(qualifiedType(Type t1, Expression name1), qualifiedType(Type t2, Expression name2))
	= combine(findClones(t1, t2), findClones(name1, name2));
CloneReport findClones(simpleType(Expression t1), simpleType(Expression t2)) = findClones(t1, t2);
CloneReport findClones(upperbound(Type t1), upperbound(Type t2)) = findClones(t1, t2);
CloneReport findClones(lowerbound(Type t1), lowerbound(Type t2)) = findClones(t1, t2);

CloneReport findClones(wildcard(), wildcard()) = <type1(), 1, 1>;
CloneReport findClones(\int()    , \int()    ) = <type1(), 1, 1>;
CloneReport findClones(short()   , short()   ) = <type1(), 1, 1>;
CloneReport findClones(long()    , long()    ) = <type1(), 1, 1>;
CloneReport findClones(float()   , float()   ) = <type1(), 1, 1>;
CloneReport findClones(double()  , double()  ) = <type1(), 1, 1>;
CloneReport findClones(char()    , char()    ) = <type1(), 1, 1>;
CloneReport findClones(string()  , string()  ) = <type1(), 1, 1>;
CloneReport findClones(byte()    , byte()    ) = <type1(), 1, 1>;
CloneReport findClones(\void()   , \void()   ) = <type1(), 1, 1>;
CloneReport findClones(\boolean(), \boolean()) = <type1(), 1, 1>;

CloneReport findClones(unionType(list[Type] t1s), unionType(list[Type] t2s)) {
	CloneType clone = emptyClone;
	
	for (int i <- [0..size(t1s)]){
		clone = combine(clone, findClones(t1s[i], t2s[i]));
	}
	
	return clone;
}


// any two elements not of the same type will end up here
CloneReport findClones(Statement a, Statement b) = <type3(), 0, max(numLinesOf(a.src), numLinesOf(b.src))>;
CloneReport findClones(Expression a, Expression b) = <type3(), 0, max(numLinesOf(a.src), numLinesOf(b.src))>;
CloneReport findClones(Declaration a, Declaration b) = <type3(), 0, max(numLinesOf(a.src), numLinesOf(b.src))>;
CloneReport findClones(Type a, Type b) = <type2(), 1, 1>;

CloneReport combine(CloneReport first, CloneReport second){
	CloneType ctype = isLess(first.ctype, second.ctype) ? second.ctype : first.ctype;
	
	int numTotalElts = first.totalElts + second.totalElts + 1;
	int numCloneElts = first.numCloneElts + second.numCloneElts;
	if (first.numCloneElts > 0 && second.numCloneElts > 0) numCloneElts += 1;
	
	// CloneType ctype, int numCloneElts, int totalElts
	return <ctype, numCloneElts, numTotalElts>;
}

CloneReport combine(CloneReport a, CloneReport b, CloneReport c){
	if (isLess(a.ctype, c.ctype)) {
		return combine(combine(a, b), c);
	} else {
		return combine(a, combine(b, c));
	}
}

CloneReport combine(CloneReport a, CloneReport b, CloneReport c, CloneReport d) = combine(combine(a, b), combine(c, d));
CloneReport combine(CloneReport rep, CloneType added) = combine(added, rep);

CloneReport combine(CloneType added, CloneReport clone){
	if (isLess(clone.ctype, added)) clone.ctype = added;
	return clone;
}

// uses dynamic programming to find best clones.
CloneReport findClones(list[Statement] alpha, list[Statement] beta) {
	int m = size(alpha);
	int n = size(beta);
	
	if (m == 0 || n == 0) return <type3(), 0, m + n>;
	if (m == 1 && n == 1) return findClones(alpha[0], beta[0]);
	if (m == 1) return findClones(alpha[0], beta);
	if (n == 1) return findClones(beta[0], alpha);
	
	// m > 1 && n > 1
	list[list[CloneReport]] cloneMat = [[emptyClone | l1 <- [0..n]] | l2 <- [0..m]];
	
	// prepare 0 - bounds
	for (int i <- [0..m]) {
		cloneMat[i][0] = findClones(alpha[i], beta[0]);
	}

	for (int j <- [1..n]) {
		cloneMat[0][j] = findClones(alpha[0], beta[j]);
	}

	// fill middle
	for (int i <- [1..m]) {
		for (int j <- [1..n]) {
			CloneReport result = findClones(alpha[i], beta[j]);
			
			CloneReport dSide = cloneMat[i - 1][j - 1];
			CloneReport iSide = cloneMat[i - 1][j];
			CloneReport jSide = cloneMat[i][j - 1];
			
			if (isLess(dSide.ctype, type3()) && isLess(result.ctype, type3())) {
				cloneMat[i][j] = combine(cloneMat[i - 1][j - 1], result);
			
			} else if (iSide.numCloneElts > dSide.numCloneElts && iSide.numCloneElts > jSide.numCloneElts) {
				cloneMat[i][j] = combine(cloneMat[i - 1][j], result);
			
			} else if (jSide.numCloneElts > dSide.numCloneElts){
				cloneMat[i][j] = combine(cloneMat[i][j - 1], result);
				
			} else {
				cloneMat[i][j] = combine(cloneMat[i - 1][j - 1], result);
			}
			
		}
	}
	
	return cloneMat[m-1][n-1];
}

// compares a list of statements with a single statement
CloneReport findClones(Statement alpha, list[Statement] beta) {
	CloneReport best = emptyClone;
	
	for (Statement s <- beta){
		CloneReport result = findClones(alpha, s);
		
		if (result.numCloneElts > best.numCloneElts){
			best = result;
		}
	}

	return <type3(), best.numCloneElts, best.totalElts>;
}

// finds clones between the two given lists of expressions
// does a shallow check for type 3 clones
CloneReport findClones(list[Expression] alpha, list[Expression] beta){
	if (size(alpha) == 0 || size(beta) == 0) return emptyClone;
	if (size(alpha) != size(beta)) return <type3(), 0, max(size(alpha), size(beta))>;
	
	CloneReport result = emptyClone;
	for (int i <- [1..size(alpha)]){
		result = combine(result, findClones(alpha[i], beta[i]));
	}
	
	return result;
}

int numLinesOf(loc elt){
	if (elt == |unknown:///|) return 0;
	
	<lastLine, _> = elt.end;
	<firstLine ,_> = elt.begin;
	
	return lastLine - firstLine + 1;
}


// redundant / unused functions


// traces the best clone that stretches the whole matrix
CloneReport backTrace(list[list[CloneReport]] mat, int r, int c){
	println("backTrace");
	CloneReport target = mat[r][c];
	
	if (r == 0 || c == 0) 
		return target;
	
	else if (isLess(target.ctype, type3()))
		return combine(backTrace(mat, r-1, c-1), target);
	
	else if (mat[r][c - 1] > mat[r - 1][c]) 
		return backTrace(mat, r,c-1);
	
	else 
		return backTrace(mat, r-1,c);
}

int linesBetween(loc a, loc b){
	println("linesBetween");
	if (a == b) return 0;
	if (a == |unknown:///|) return 0;
	if (b == |unknown:///|) return 0;

	<lastLine, _> = b.end;
	<firstLine ,_> = a.begin;
	return (lastLine - firstLine) + 1;
}

// returns a location from a to b, assuming a and b are in the same file
loc getLocRange(loc a, loc b){
	println("getLocRange");
	if (a == b) return a;
	if (a == |unknown:///| || a.length == 0) return b;
	if (b == |unknown:///| || b.length == 0) return a;
	
	loc newLoc = |unknown:///|(0, 0, <0, 0>, <0, 0>);
	
	newLoc.uri = a.uri;
	if (a.offset < b.offset) {
		newLoc.offset = a.offset;
		newLoc.length = (b.offset + b.length) - a.offset;
		newLoc.end = b.end;
		newLoc.begin = a.begin;
		return newLoc;
		
	} else {
		newLoc.offset = b.offset;
		newLoc.length = (a.offset + a.length) - b.offset;
		newLoc.end = a.end;
		newLoc.begin = b.begin;
		return newLoc;
	}
}