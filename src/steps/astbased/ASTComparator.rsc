module steps::astbased::ASTComparator

import util::Math;
import steps::astbased::FileParser;
import DataTypes;
import lang::java::m3::AST;
import IO;
import List;

alias CloneReport = tuple[CloneType ctype, loc leftLoc, loc rightLoc, int numCloneElts];

int CLONE_SIZE = 5;
real LINE_SIM_MINIMUM = 0.7;
bool DO_REPORT_LEVEL_3 = true;

void findClones(\enum(_, _, _, list[Declaration] body1), \enum(_, _, _, list[Declaration] body2)) = findClonesDecList(body1, body2);
void findClones(\class(_, _, _, list[Declaration] body1), \class(_, _, _, list[Declaration] body2)) = findClonesDecList(body1, body2);
void findClones(\class(list[Declaration] body1), \class(list[Declaration] body2)) = findClonesDecList(body1, body2);

void findClones(\initializer(Statement body1), \initializer(Statement body2)) = reportClone(findClones(body1, body2));
void findClones(\method(_, _, _, _, Statement impl1), \method(_, _, _, _, Statement impl2)) = reportClone(findClones(impl1, impl2));
void findClones(\constructor(_, _, _, Statement impl1), \constructor(_, _, _, Statement impl2)) = reportClone(findClones(impl1, impl2));

void findClones(\variables(Type type1, list[Expression] fragments1), \variables(Type type2, list[Expression] fragments2)) {
	reportClone(combine(findClones(type1, type2), findClones(fragments1, fragments2)));
}
	
void findClones(\compilationUnit(_, list[Declaration] types1), \compilationUnit(_, list[Declaration] types2)){
	findClonesDecList(types1, types2);
}
	
void findClones(\compilationUnit(_, _, list[Declaration] types1), \compilationUnit(_, _, list[Declaration] types2)){
	findClonesDecList(types1, types2);
}

void findClonesDecList(list[Declaration] alpha, list[Declaration] beta){
	int nrOfElts = min(size(alpha), size(beta));
	
	// offset = soffset of b relative to a
	for (int offset <- [-size(alpha) .. size(alpha)]){
		int minIndex = max(0, -offset);
		int maxIndex = min(nrOfElts, size(beta) - offset);
		if (maxIndex < minIndex) continue;
		
		// offset = index from start where clones are searched
		for (int index <- [minIndex .. maxIndex]) {
			findClones(alpha[index], beta[index + offset]);
		}
	}
}	
	
void findClones(Declaration a, Declaration b){
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
	if (op1 != op2) return <type3(), lhs1.src, lhs2.src, 0>;
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
	if (type1 != type2) return <type3(), |unknown:///|, |unknown:///|, 0>;
	
	return findClones(args1, args2);
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
	(rightSide1 == rightSide2) ? findClones(leftSide1, leftSide2) : <type3(), a.src, b.src, 0>;
	
CloneReport findClones(\methodCall(_, str name1, list[Expression] arg1), \methodCall(_, str name2, list[Expression] arg2)) =
	combine((name1 == name2) ? type1() : type2(), findClones(arg1, arg2));
	
CloneReport findClones(\methodCall(_, Expression rec1, str name1, list[Expression] arg1), \methodCall(_, Expression rec2, str name2, list[Expression] arg2)) = 
	combine(combine((name1 == name2) ? type1() : type2(), findClones(arg1, arg2)), findClones(rec1, rec2));
	
CloneReport findClones(\type(Type type1), \type(Type type2)) = findClones(type1, type2);
			
CloneReport findClones(\bracket(Expression exp1), \bracket(Expression exp2)) = findClones(exp1, exp2);
	
CloneReport findClones(\this(Expression exp1), \this(Expression exp2)) = findClones(exp1, exp2);
	
CloneReport findClones(\declarationExpression(Declaration dec1), \declarationExpression(Declaration dec2)) {
	findClones(dec1, dec2);
	return <type3(), dec1.src, dec2.src, 0>;
}
	
CloneReport findClones(\infix(Expression lhs1, str op1, Expression rhs1), \infix(Expression lhs2, str op2, Expression rhs2)) = 
	combine(combine((op1 == op2) ? type1() : type2(), findClones(lhs1, lhs2)), findClones(rhs1, rhs2));
	
CloneReport findClones(\postfix(Expression lhs1, str op1), \postfix(Expression lhs2, str op2)) = 
	combine((op1 == op2) ? type1() : type2(), findClones(lhs1, lhs2));
	
CloneReport findClones(\prefix(str op1, Expression rhs1), \prefix(str op2, Expression rhs2)) = 
	combine((op1 == op2) ? type1() : type2(), findClones(rhs1, rhs2));
	
CloneReport findClones(\memberValuePair(str name1, Expression value1), \memberValuePair(str name2, Expression value2)) = 
	combine((name1 == name2) ? type1() : type2(), findClones(value1, value2));
			
// any two elements not of the same ctype will end up here
// additionally the sink for types that are never clones:
// compilationUnit, interface, import, package, annotations
CloneReport findClones(Expression a, Expression b) {
	tuple[Expression, Expression] asTuple = <a, b>;
	
	switch (asTuple){
		case <\null(), \null()>: 
			return <type1(), a.src, b.src, 1>;
		case <\this(), \this()>: 
			return <type1(), a.src, b.src, 1>;
		case <\super(), \super()>: 
			return <type1(), a.src, b.src, 1>;
			
		case <\number(str val1), \number(str val2)>:
			return (val1 == val2) ? <type1(), a.src, b.src, 1> : <type2(), a.src, b.src, 1>;
			
		case <\booleanLiteral(bool val1), \booleanLiteral(bool val2)>:
			return (val1 == val2) ? <type1(), a.src, b.src, 1> : <type2(), a.src, b.src, 1>;
			
		case <\stringLiteral(str val1), \stringLiteral(str val2)>:
			return (val1 == val2) ? <type1(), a.src, b.src, 1> : <type2(), a.src, b.src, 1>;
			
		case <\characterLiteral(str val1), \characterLiteral(str val2)>:
			return (val1 == val2) ? <type1(), a.src, b.src, 1> : <type2(), a.src, b.src, 1>;
			
		case <\simpleName(str val1), \simpleName(str val2)>:
			return (val1 == val2) ? <type1(), a.src, b.src, 1> : <type2(), a.src, b.src, 1>;
			
		case <\variable(str name1, int dim1), \variable(str name2, int dim2)>:
			return combine(
				(name1 == name2) ? <type1(), a.src, b.src, 1> : <type2(), a.src, b.src, 1>,
				(dim1 == dim2) ? <type1(), a.src, b.src, 1> : <type2(), a.src, b.src, 1>
			);
			
		case <\variable(str name1, int dim1, Expression init1), \variable(str name2, int dim2, Expression init2)>:
			return combine((name1 == name2) ? type1() : type2(), 
				combine((dim1 == dim2) ? type1() : type2(), findClones(init1, init2))
			);
			
			case <\fieldAccess(_, str name1), \fieldAccess(_, str name2)>:
				return (name1 == name2) ? <type1(), a.src, b.src, 1> : <type2(), a.src, b.src, 1>;
			
		default: 
			return <\type3(), a.src, b.src, 0>;
	}
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
		\for(list[Expression] inits2, Expression cond2, list[Expression] up2, Statement body2)) {
	return combine(findClones(inits1, inits2), findClones(up1, up2), findClones(cody1, body2));
}
CloneReport findClones(\if(Expression cond1, Statement then1, Statement else1), 
		\if(Expression cond2, Statement then2, Statement else2)) {
	return combine(findClones(cond1, cond2), findClones(then1, then2), findClones(else1, else2));
}
CloneReport findClones(\if(Expression cond1, Statement then1), 
		\if(Expression cond2, Statement then2, Statement else2)) {
	return combine(findClones(cond1, cond2), findClones(then1, then2));
}
CloneReport findClones(\label(str name1, Statement body1), \label(str name2, Statement body1)) {
	return combine(findClones(name1, name2), findClones(body1, body2));
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
	findClones(ex1, ex2);
	return findClones(body1, body2);
}
CloneReport findClones(\declarationStatement(Declaration declaration1), \declarationStatement(Declaration declaration2)) {
	findClones(declaration1, declaration2);
	return <type3(), declaration1.src, declaration2.src, 0>;
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

CloneReport findClones(Statement a, Statement b){
	tuple[Statement, Statement] asTuple = <a, b>;
	
	switch (asTuple){
		case <\break(), \break()>:
			return <type1(), a.src, b.src, 1>;
		case <\continue(), \continue()>:
			return <type1(), a.src, b.src, 1>;
		case <\continue(str label), \continue(str label)>:
			return <type1(), a.src, b.src, 1>;
		case <\break(str label), \break(str label)>:
			return <type1(), a.src, b.src, 1>;
		case <\empty(), \empty()>:
			return <type1(), a.src, b.src, 1>;
		case <\return(), \return()>:
			return <type1(), a.src, b.src, 1>;
		case <\defaultCase(), \defaultCase()>:
			return <type1(), a.src, b.src, 1>;
			default:
		return <type3(), a.src, b.src, 0>;
	}
}

CloneType findClones(Type a, Type b){
	return (type1 == type2) ? type1() : type2();
}

CloneReport combine(CloneReport first, CloneReport second){
	// ignore non-clones on any side
	if (first.numCloneElts == 0 && second.numCloneElts == 0) {
		return <type3(), |unknown:///|, |unknown:///|, 0>;
		
	} else if (first.numCloneElts == 0) {
		reportClone(second);
	
	} else if (second.numCloneElts == 0) {
		reportClone(first);
	}
		
	loc locLeft = getLocRange(first.leftLoc, second.leftLoc);
	loc locRight = getLocRange(first.rightLoc, second.rightLoc); 
	
	// this assumes that the overlapping parts are clones in both situations, which may not be the case
	int numCloneElts = first.numCloneElts + second.numCloneElts; 
	
	CloneType ctype;
	tuple[CloneType, CloneType] typePair = <first.ctype, second.ctype>;
	// basically ctype = min(first.ctype, second.ctype);
	switch(typePair){
		case <type1(), type1()>:
			ctype = type1();
		case <type1(), type2()>:
			ctype = type2();
		case <type2(), type1()>:
			ctype = type2();
		case <type2(), type2()>:
			ctype = type2();
		default: ctype = type3();
	}
	
	// CloneType ctype, loc leftLoc, loc rightLoc, int numCloneElts
	return <ctype, locLeft, locRight, numCloneElts>;
}

CloneReport combine(CloneReport a, CloneReport b, CloneReport c){
	return combine(a, combine(b, c));
}

CloneReport combine(CloneReport a, CloneReport b, CloneReport c, CloneReport d){
	return combine(combine(a, b), combine(b, c));
}

CloneReport combine(CloneReport rep, CloneType added){
	return combine(added, rep);
}

CloneReport combine(CloneType added, CloneReport rep){
	tuple[CloneType, CloneType] typePair = <added, rep.ctype>;
	// basically ctype = min(first.ctype, second.ctype);
	switch(typePair){
		case <type1(), type1()>:
			rep.ctype = type1();
		case <type1(), type2()>:
			rep.ctype = type2();
		case <type2(), type1()>:
			rep.ctype = type2();
		case <type2(), type2()>:
			rep.ctype = type2();
		default: rep.ctype = type3();
	}
	
	return rep;
}

// a type 1 clone of the Expression variant
CloneReport findClones(list[Statement] alpha, list[Statement] beta) {
	if (size(alpha) == 0 || size(beta) == 0) return <type3(), |unknown:///|, |unknown:///|, 0>;

	CloneReport best = <type3(), |unknown:///|, |unknown:///|, 0>; // clone of the whole list
	int nrOfElts = min(size(alpha), size(beta));
	int maxAlphaStart = size(alpha);
	
	// offset = soffset of b relative to a
	for (int offset <- [-maxAlphaStart .. maxAlphaStart]){
		int minIndex = max(0, -offset);
		int maxIndex = min(nrOfElts, size(beta) - offset);
		if (maxIndex < minIndex) continue;
		
		CloneReport collect = <type3(), |unknown:///|, |unknown:///|, 0>;
		
		// offset = index from start where clones are searched
		for (int index <- [minIndex .. maxIndex]) {
			CloneReport result = findClones(alpha[index], beta[index + offset]);
			
			if (result.numCloneElts > 0){	
				collect = combine(collect, result);
				
			} else {
				if (collect.numCloneElts > best.numCloneElts){
					best = collect;
				}
				collect = result;
			}
			
			if (collect.numCloneElts > best.numCloneElts){
				best = collect;
			}
		}
	}
	
	// report unless list is a full clone
	if (best.numCloneElts < nrOfElts) {
		reportClone(best);
	}
	
	return best;
}

// finds clones between the two given lists
CloneReport findClones(list[Expression] alpha, list[Expression] beta){
	if (size(alpha) == 0 || size(beta) == 0) return <type1(), |unknown:///|, |unknown:///|, 0>;

	CloneReport best = <type3(), |unknown:///|, |unknown:///|, 0>; // clone of the whole list
	int nrOfElts = min(size(alpha), size(beta));
	int maxAlphaStart = size(alpha);
	
	// offset = soffset of b relative to a
	for (int offset <- [-maxAlphaStart .. maxAlphaStart]){
		int minIndex = max(0, -offset);
		int maxIndex = min(nrOfElts, size(beta) - offset);
		if (maxIndex < minIndex) continue;
		
		CloneReport collect = <type3(), |unknown:///|, |unknown:///|, 0>;
		
		// offset = index from start where clones are searched
		for (int index <- [minIndex .. maxIndex]) {
			CloneReport result = findClones(alpha[index], beta[index + offset]);
			
			if (result.numCloneElts > 0){
				println("\"<result.leftLoc>\", \"<result.rightLoc>\" ,<result.numCloneElts>, <result.ctype>\n");
				collect = combine(collect, result);
				
			} else {
				if (collect.numCloneElts > best.numCloneElts){
					best = collect;
				}
				collect = result;
			}
			
			if (collect.numCloneElts > best.numCloneElts){
				best = collect;
			}
		}
	}
	
	// report unless list is a full clone
	if (best.numCloneElts < nrOfElts) {
		reportClone(best);
		return combine(type3(), best);
		
	} else {
		return best;
	}
}


CloneReport findType3Clones(list[Expression] alpha, list[Expression] beta){
	int nrOfClones;
	int cloneType;
	
	int m = size(alpha);
	int n = size(beta);
	list[list[int]] cMat = []; // initialized at 0

	for (int i <- [1..m]) {
		for (int j <- [1..n]) {
			CloneReport result = findClones(alpha[i], beta[i]);
			
			if (result.isClone) {
				cMat[i][j] = cMat[i - 1][j - 1] + 1;

			} else {
				cMat[i][j] = Math.max(cMat[i][j - 1], cMat[i - 1][j]);
			}
		}
	}
	
	int lcs = cMat[m][n];
	int minSize = min(m, n);
	
	return <type3(), |unknown:///|, |unknown:///|, 0>;
}

void reportClone(CloneReport clone){
	if (clone.numCloneElts < CLONE_SIZE) return;
	if (clone.ctype == type3()) return;
	
	int totalLines = numLinesOf(clone);
	//int similarity = (100 * totalLines) / clone.numCloneElts;
	int similarity = 100;
	
	//println("\"<clone.leftLoc>\", \"<clone.rightLoc>\" ,<clone.numCloneElts>, <clone.ctype>, <similarity>\n");
	
}

// returns a location from a to b, assuming a and b are in the same file
loc getLocRange(loc a, loc b){
	if (a == b) return a;
	if (a == |unknown:///| || b > a) return b;
	if (b == |unknown:///| || a > b) return a;
	
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

int getOverlap(loc a, loc b) {
	<astart, _> = a.begin;
	<bstart, _> = b.begin;
	<aend, _> = a.end;
	<bend, _> = b.end;
	
	return max(0, (astart <= bstart) ? (aend - bstart + 1) : (bend - astart + 1));
}

int numLinesOf(CloneReport elt){
	loc leftLoc = elt.leftLoc;
	loc rightLoc = elt.rightLoc;
	
	return max(numLinesOf(leftLoc), numLinesOf(rightLoc));
}

int numLinesOf(loc elt){
	if (elt == |unknown:///|) return 0;
	
	<leftEnd, _> = elt.end;
	<leftBegin ,_> = elt.begin;
	
	return leftEnd - leftBegin + 1;
}