package project.parser.ast;

import project.visitors.Visitor;

public class Lower extends BinaryOp {
	public Lower(Exp left, Exp right) {
		super(left, right);
	}

	@Override
	public <T> T accept(Visitor<T> visitor) {
		return visitor.visitLower(left, right); 
	}
}
