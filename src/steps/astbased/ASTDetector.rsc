module steps::astbased::ASTDetector

import util::Math;
import steps::astbased::FileParser;
import steps::astbased::ASTComparator;
import DataTypes;
import lang::java::m3::AST;
import IO;
import List;

// alias Clone = rel[loc fragment1, loc fragment2, CloneType cloneType, int lineSimilarity];
alias Childs = tuple[list[Statement] stats, list[Expression] exps];

int MIN_SIMILARITY_PERC = 25;

Clone compare(Declaration a, Declaration b){
	list[Statement] alpha = extractFuncs(a);
	list[Statement] beta = extractFuncs(b);
	Clone results = {};
	
	for (Statement method1 <- alpha, couldSubTreeBeAClone(method1)){
		for (Statement method2 <- beta, couldSubTreeBeAClone(method2)){
			results += compare(method1, method2);
		}
	}
	
	return results;
}

Clone compare(Statement a, Statement b){
	CloneReport result = findClones(a, b);
	
	if (result.numCloneElts > 0){
		int similarity = (100 * result.numCloneElts) / result.totalElts;
		
		if (similarity > MIN_SIMILARITY_PERC) {
			println("<a.src>, <b.src>, <result.ctype>, <similarity>");
			return {<a.src, b.src, result.ctype, similarity>};
		}
	}
	Clone clones = {};
	
	Childs aChilds = extract(a);
	Childs bChilds = extract(b);
	list[Statement] aStats = aChilds.stats;
	list[Statement] bStats = bChilds.stats;
	
	for (Statement bChild <- bChilds.stats, couldSubTreeBeAClone(bChild)){
		clones += compare(a, bChild);
	}
	for (Statement aChild <- aChilds.stats, couldSubTreeBeAClone(aChild)){
		clones += compare(aChild, b);
	}
	
	return clones;
}

// break down declaration to a list of methods as statements
list[Statement] extractFuncs(\compilationUnit(_, _, list[Declaration] types)) = extractFuncs(types);
list[Statement] extractFuncs(\compilationUnit(_, list[Declaration] types)) = extractFuncs(types);
list[Statement] extractFuncs(\enum(_, _, _, list[Declaration] body)) = extractFuncs(body);
list[Statement] extractFuncs(\class(_, _, _, list[Declaration] body)) = extractFuncs(body);
list[Statement] extractFuncs(\class(list[Declaration] body)) = extractFuncs(body);
list[Statement] extractFuncs(list[Declaration] declarations) = [s | d <- declarations, s <- extractFuncs(d)];

list[Statement] extractFuncs(\initializer(Statement body)) = [body];
list[Statement] extractFuncs(\method(_, _, _, _, Statement impl)) = [impl];
list[Statement] extractFuncs(\constructor(_, _, _, Statement impl)) = [impl];
list[Statement] extractFuncs(Declaration other) = [];

Childs extract(\assert(Expression expression)) = <[], [expression]>;
Childs extract(\assert(Expression expression, Expression message)) = <[], [expression, message]>;
Childs extract(\block(list[Statement] statements)) = <statements, []>;
Childs extract(\do(Statement body, Expression condition)) = <[body], [condition]>;
Childs extract(\foreach(Declaration parameter, Expression collection, Statement body)) = <[body], [collection]>;
Childs extract(\for(list[Expression] initializers, Expression condition, list[Expression] updaters, Statement body)) = <[body], initializers + condition + updaters>;
Childs extract(\for(list[Expression] initializers, list[Expression] updaters, Statement body)) = <[body], initializers + updaters>;
Childs extract(\if(Expression condition, Statement thenBranch)) = <[thenBranch], [condition]>;
Childs extract(\if(Expression condition, Statement thenBranch, Statement elseBranch)) = <[thenBranch, elseBranch], [condition]>;
Childs extract(\label(str name, Statement body)) = <[body], []>;
Childs extract(\return(Expression expression)) = <[], [expression]>;
Childs extract(\switch(Expression expression, list[Statement] statements)) = <statements, [expression]>;
Childs extract(\case(Expression expression)) = <[], [expression]>;
Childs extract(\synchronizedStatement(Expression lock, Statement body)) = <[body], [lock]>;
Childs extract(\throw(Expression expression)) = <[], [expression]>;
Childs extract(\try(Statement body, list[Statement] catchClauses)) = <catchClauses + body, []>;
Childs extract(\try(Statement body, list[Statement] catchClauses, Statement \finally)) = <catchClauses + body + \finally, []>;
Childs extract(\catch(Declaration exception, Statement body)) = <[body], []>;
Childs extract(\while(Expression condition, Statement body)) = <[body], [condition]>;
Childs extract(\expressionStatement(Expression stmt)) = <[], [stmt]>;
Childs extract(\constructorCall(bool isSuper, Expression expr, list[Expression] arguments)) = <[], arguments + expr>;
Childs extract(\constructorCall(bool isSuper, list[Expression] arguments)) = <[], arguments>;
Childs extract(Statement other) = <[], []>;