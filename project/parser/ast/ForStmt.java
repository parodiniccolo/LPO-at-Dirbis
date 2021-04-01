package project.parser.ast;

import static java.util.Objects.requireNonNull;

import project.visitors.Visitor;

public class ForStmt implements Stmt {
	private final VarIdent ident;
	private final Exp exp;
	private final Block stmt; //check  

	public ForStmt(VarIdent id, Exp exp, Block stmt) {
		this.ident = requireNonNull(id);
		this.exp = requireNonNull(exp);
		this.stmt = requireNonNull(stmt);
	}

	   @Override
    public String toString() {
        return getClass().getSimpleName() + "(" + ident + "," + exp + "," + stmt + ")";
    }

	@Override
	public <T> T accept(Visitor<T> visitor) {
		return visitor.visitForStmt(ident, exp, stmt);
	}
}
