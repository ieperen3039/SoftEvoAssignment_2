module steps::astbased::ASTComparator

import util::Math;

int CLONE_SIZE = 10;
alias CloneInt = int;

void reportClonesOf(Declaration a, Declaration b) {
	tuple[Declaration, Declaration] asTuple = {<a, b>};
	
	switch(asTuple){
		case <\enum(_, _, _, list[Declaration] body1), \enum(_, _, _, list[Declaration] body2)>:
			findClones(body1, body2);
			
		case <\class(_, _, _, list[Declaration] body1), \class(_, _, _, list[Declaration] body2)>:
			findClones(body1, body2);
			
		case <\class(list[Declaration] body1), \class(list[Declaration] body2)>:
			findClones(body1, body2);
			
		case <\interface(_, _, _, list[Declaration] body1), \interface(_, _, _, list[Declaration] body2)>:
			findClones(body1, body2);

		case <\field(Type type1, list[Expression] fragments1), \field(Type type2, list[Expression] fragments2)>:
			findClones(fragments1, fragments2);

		case <\initializer(Statement body1), \initializer(Statement body2)>:
			findClones(body1, body2);

		case <\method(_, _, _, _, Statement impl1), \method(_, _, _, _, Statement impl2)>:
			findClones(impl1, impl2);

		case <\constructor(_, _, _, Statement impl1), \constructor(_, _, _, Statement impl2)>:
			findClones(impl1, impl2);

		case <\variables(Type type1, list[Expression] fragments1), \variables(Type type2, list[Expression] fragments2)>:
			findClones(fragments1, fragments2);
	}
}

void reportClone(CloneInt typeID, list[Expression] exp, int startInd, int endInd){
	if (typeID == 0) return;
	
	CloneType ctype = getType(typeID);
}

// any two elements not of the same type will end up here
// additionally the sink for types that are never clones:
// compilationUnit, interface, import, package, annotations
CloneInt findClones(Expression a, Expression b){
	tuple[Expression, Expression] asTuple = {<a, b>};
	
	switch(asTuple) {
		case <\arrayAccess(Expression a1, Expression i1), \arrayAccess(Expression a2, Expression i2)>:
			return max(findClones(a1, a2), findClones(i1, i2));
			
		case <\newArray(Type type1, list[Expression] dim1, Expression init1), 
				\newArray(Type type2, list[Expression] dim2, Expression init2)>:
			return max(findClones(type1, type2), findClones(dim1, dim2), findClones(init1, init2));
			
		case <\newArray(Type type1, list[Expression] dim1), \newArray(Type type2, list[Expression] dim2)>:
			return max(findClones(type1, type2), findClones(dim1, dim2));
			
		case <\arrayInitializer(list[Expression] elts1), \arrayInitializer(list[Expression] elts2)>:
			return findClones(elts1, elts2);
			
		case <\assignment(Expression lhs1, str op1, Expression rhs2), \assignment(Expression lhs2, str op2, Expression rhs2)>: {
			if (op1 != op2) return 3;
			return max(findClones(lhs1, lhs2), findClones(rhs1, rhs2));
		}
			
		case <\cast(Type type1, Expression exp1), \cast(Type type2, Expression exp2)>:
			return max(findClones(type1, type2), findClones(exp1, exp2));
			
		case <\characterLiteral(str charValue1), \characterLiteral(str charValue2)>:
			return findClones(charValue1, charValue2);
			
		case <\newObject(Expression expr1, Type type1, list[Expression] args1, Declaration class1), 
				\newObject(Expression expr2, Type type2, list[Expression] args2, Declaration class2)>: {
			if (!areEqual(type1, type2)) return 3;
			
			reportClonesOf(class1, class2);
			CloneInt expEq = findClone(expr1, expr2);
			CloneInt argEq = findClone(args1, args2);
			return max(max(expEq, argEq), class1 == class2 ? 1 : 3);
		}
		
		case <\newObject(Expression expr1, Type type1, list[Expression] args1), 
				\newObject(Expression expr2, Type type2, list[Expression] args2)>: {
			if (!areEqual(type1, type2)) return 0;
			
			CloneInt expEq = findClone(expr1, expr2);
			CloneInt argEq = findClone(args1, args2);
			return max(expEq, argEq);
		}
			
		case <\newObject(Type type1, list[Expression] args1, Declaration class1), 
				\newObject(Type type2, list[Expression] args2, Declaration class2)>:{
			if (!areEqual(type1, type2)) return 3;
			
			CloneInt argEq = findClone(args1, args2);
			CloneInt classEq = findClone(class1, class2);
			return max(argEq, classEq);
		}
		
		case <\newObject(Type type1, list[Expression] args1), \newObject(Type type2, list[Expression] args2)>:{
			if (!areEqual(type1, type2)) return 3;
			
			return findClone(args1, args2);
		}
			
		case <\qualifiedName(Expression qual1, Expression exp1), \qualifiedName(Expression qual2, Expression exp2)>:
			return max(findClones(qual1, qual2), findClones(exp1, exp2));
		
		case <\conditional(Expression guard1, Expression thenBranch1, Expression elseBranch1), 
				\conditional(Expression guard2, Expression thenBranch2, Expression elseBranch2)>:{
			CloneInt guardEq = findClone(guard1, guard2);
			CloneInt thenEq = findClone(thenBranch1, thenBranch2);
			CloneInt elseEq = findClone(elseBranch1, elseBranch2);

			return max(max(guardEq, thenEq), elseEq);
		}
		
		case <\fieldAccess(_, Expression exp1, str name1), \fieldAccess(_, Expression exp2, str name2)>:
			return max(findClones(exp1, exp2), findClones(name1, name2));
			
		case <\fieldAccess(_, str name1), \fieldAccess(_, str name2)>:
			return findClones(name1, name2);
			
		case <\instanceof(Expression leftSide1, Type rightSide1), \instanceof(Expression leftSide2, Type rightSide2)>:
			return areEqual(rightSide1, rightSide2) ? findClones(leftSide1, leftSide2) : 3;
			
		case <\methodCall(_, str name1, list[Expression] arg1), \methodCall(_, str name2, list[Expression] arg2)>:
			return max(findClones(name1, name2), findClones(arg1, arg2));
			
		case <\methodCall(_, Expression rec1, str name1, list[Expression] arg1), 
				\methodCall(_, Expression rec2, str name2, list[Expression] arg2)>:
			return max(max(findClones(name1, name2), findClones(arg1, arg2), findClones(rec1, rec2);
			
		case <\null(), \null()>:
			return 1;
			
		case <\number(str a), \number(str b)>:
			return findClones(a, b);
			
		case <\booleanLiteral(bool a), \booleanLiteral(bool b)>:
			return (a == b) ? 1 : 2;
			
		case <\stringLiteral(str a), \stringLiteral(str b)>:
			return findClones(a, b);
			
		case <\type(Type type1), \type(Type type2)>:
			return areEqual(type1, type2) ? 1 : 2;
			
		case <\variable(str name1, int dim1), \variable(str name2, int dim2)>:
			return max((dim1 == dim2) ? 1 : 2, findClones(name1, name2));
			
		case <\variable(str name, int extraDimensions, Expression init1), 
				\variable(str name, int extraDimensions, Expression init2)>:
			return max(max((dim1 == dim2) ? 1 : 2, findClones(name1, name2)), findClones(init1, init2);
			
		case <\bracket(Expression exp1), \bracket(Expression exp2)>:
			return findClones(exp1, exp2);
			
		case <\this(), \this()>:
			return 1;
			
		case <\this(Expression exp1), \this(Expression exp2)>:
			return findClones(exp1, exp2);
			
		case <\super(), \super()>:
			return 1;
			
		case <\declarationExpression(Declaration dec1), \declarationExpression(Declaration dec2)>:
			return findClones(dec1, dec2);
			
		case <\infix(Expression lhs1, str op1, Expression rhs1), \infix(Expression lhs2, str op2, Expression rhs2)>:
			return max(max(findClones(lhs1, lhs2), findClones(op1, op2)), findClones(rhs1, rhs2));
			
		case <\postfix(Expression lhs1, str operator), \postfix(Expression lhs2, str operator)>:
			return max(findClones(lhs1, lhs2), findClones(op1, op2));
			
		case <\prefix(str operator, Expression rhs1), \prefix(str operator, Expression rhs2)>:
			return max(findClones(op1, op2), findClones(rhs1, rhs2));
			
		case <\simpleName(str name1), \simpleName(str name2)>:
			return findClones(name1, name2);
			
		case <\memberValuePair(str name, Expression value), \memberValuePair(str name, Expression value)>:
			return max(findClones(name1, name2), findClones(value1, value2));
			
		default:
			return 0;
	}
}
			//return max(max(findClones(), findClones()), findClones());

// TODO: additional level 2 clone detection
// returns clone type, NOT number of clones
CloneInt findClones(list[Expression] alpha, list[Expression] beta){
	int nrOfClones;
	int cloneType;
	int nrOfExp = length(alpha);
	
	for (int i <- [0 .. nrOfExp]){
		int expType = findClones(alpha[i], beta[i]);
		if (expType != 0 && expTyp == cloneType){
			nrOfClones++;
			
		} else {
			// report blocks of clones at once
			if (nrOfClones >= CLONE_SIZE) {
				reportClone(cloneType, alpha, i - clones, i);
			}
			
			nrOfClones = 0;
			cloneType = 0;
		}
	}
	
	// report if the last few also form a clone (but do not report if list is a full clone)
	if (nrOfClones > 5 && nrOfClones < nrOfExp) {
		reportClone(alpha, nrOfExp - nrOfClones, nrOfExp);
	}
	
	// could be the case
	if (clones == nrOfExp) return cloneType;
	return 0;
}

CloneInt findClones(str charValue1, str charValue2){
	return (charValue1 == charValue2) ? 1 : 2;
}

boolean areEqual(Type alpha, Type beta){
	if (alpha == beta) return true;

	tuple[Type a, Type b] asTuple = {<alpha, beta>};
	
	switch(asTuple){
		case <arrayType(Type a)                   , arrayType(Type b)                   >: return areEqual(a, b);
		case <parameterizedType(Type a)           , parameterizedType(Type b)           >: return areEqual(a, b);
		case <qualifiedType(Type a, Expression e1), qualifiedType(Type b, Expression e2)>: return areEqual(a, b) && areEqual(e1, e2);
		case <simpleType(Expression typeName)     , simpleType(Expression typeName)     >: return areEqual(a, b);
		case <upperbound(Type a)                  , upperbound(Type b)                  >: return areEqual(a, b);
		case <lowerbound(Type a)                  , lowerbound(Type b)                  >: return areEqual(a, b);
		
		default: return false;
	}
}

CloneType getType(int cloneNr){
	switch(cloneNr){
		case 1:
		return type1();
		case 2:
		return type2();
		case 3:
		return type3();
		//case 0:
		//return nullptr
	}
}