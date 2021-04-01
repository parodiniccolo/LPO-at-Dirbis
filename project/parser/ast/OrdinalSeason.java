package project.parser.ast;

import project.visitors.Visitor;

public class OrdinalSeason extends UnaryOp {

	public OrdinalSeason(Exp exp) {
		super(exp);
	}

	@Override
	public <T> T accept(Visitor<T> visitor) {
		return visitor.visitOrdinalSeason(exp);
	}
}
