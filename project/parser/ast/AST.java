package project.parser.ast;

import project.visitors.Visitor;

public interface AST {
	<T> T accept(Visitor<T> visitor);
}
