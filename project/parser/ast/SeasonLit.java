package project.parser.ast;

import project.parser.Season;
import project.visitors.Visitor;

public class SeasonLit extends PrimLiteral<Season> {

	public SeasonLit(Season n) {
		super(n);
	}

	@Override
	public <T> T accept(Visitor<T> visitor) {
		return visitor.visitSeason(value);
	}
}
