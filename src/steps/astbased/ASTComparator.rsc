module steps::astbased::ASTComparator

import util::Math;

// any two elements not of the same type will end up here
// additionally the sink for types that are never clones:
// compilationUnit, interface, import, package, annotations
boolean areClones(Declaration a, Declaration b){
	return nrOfClones(a, b) > 1;
}

int nrOfClones(
	\class(str name1, list[Type] extends1, list[Type] implements1, list[Declaration] body1)
	\class(str name2, list[Type] extends2, list[Type] implements2, list[Declaration] body2)
) {
	return nrOfClones(body1, body2);
}

int nrOfClones(list alpha, list beta){
	int clones = 0;
	
	for (int i <- [0 .. length(alpha)]){
		clones += nrOfClones(alpha[i], beta[i]);
	}
	
	return clones;
}

int areEqual(Type alpha, Type beta){
	tuple[Type a, Type b] asTuple = {<alpha, beta>};
	
	switch(asTuple){
		case <arrayType(Type a)                   , arrayType(Type b)                   >: return areEqual(a, b);
		case <parameterizedType(Type a)           , parameterizedType(Type b)           >: return areEqual(a, b);
		case <qualifiedType(Type a, Expression e1), qualifiedType(Type b, Expression e2)>: return areEqual(a, b) && areEqual(e1, e2);
		case <simpleType(Expression typeName)     , simpleType(Expression typeName)     >: return areEqual(a, b);
		case <upperbound(Type a)                  , upperbound(Type b)                  >: return areEqual(a, b);
		case <lowerbound(Type a)                  , lowerbound(Type b)                  >: return areEqual(a, b);

		case<wildcard(), wildcard()>: return true;
		case<\int()    , \int()    >: return true;
		case<short()   , short()   >: return true;
		case<long()    , long()    >: return true;
		case<float()   , float()   >: return true;
		case<double()  , double()  >: return true;
		case<char()    , char()    >: return true;
		case<string()  , string()  >: return true;
		case<byte()    , byte()    >: return true;
		case<\void()   , \void()   >: return true;
		case<\boolean(), \boolean()>: return true;
		
		default: return false;
	}
	
}