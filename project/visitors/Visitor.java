package project.visitors;

import project.parser.Season;
import project.parser.ast.Block;
import project.parser.ast.Exp;
import project.parser.ast.Stmt;
import project.parser.ast.StmtSeq;
import project.parser.ast.VarIdent;


public interface Visitor<T> {
	T visitAdd(Exp left, Exp right);

	T visitAssignStmt(VarIdent ident, Exp exp);

	T visitIntLiteral(int value);

	T visitEq(Exp left, Exp right);
	
	T visitMoreStmt(Stmt first, StmtSeq rest);

	T visitMul(Exp left, Exp right);

	T visitPrintStmt(Exp exp);

	T visitProg(StmtSeq stmtSeq);

	T visitSign(Exp exp);

	T visitVarIdent(VarIdent id); // the only corner case ...

	T visitSingleStmt(Stmt stmt);

	T visitVarStmt(VarIdent ident, Exp exp);

	T visitNot(Exp exp);

	T visitAnd(Exp left, Exp right);

	T visitBoolLiteral(boolean value);

	T visitIfStmt(Exp exp, Block thenBlock, Block elseBlock);

	T visitBlock(StmtSeq stmtSeq);

	T visitPairLit(Exp left, Exp right);

	T visitFst(Exp exp);

	T visitSnd(Exp exp);

	T visitForStmt(VarIdent id, Exp exp, Block stmt);
	
	T visitSeason(Season value); //added
	
	T visitSeasonOf(Exp exp); //added

	T visitOrdinalSeason(Exp exp); //added
	
	T visitLower(Exp left, Exp right); //added
	

	
	
}
