package project.visitors.evaluation;

import project.parser.Season;

public class SeasonValue extends PrimValue<Season> {

	public SeasonValue(Season value) {
		super(value);
	}


	@Override
	public final boolean equals(Object obj) {
		if (this==obj)
			return true;
		if(!(obj instanceof SeasonValue))
			return false;
		return value.equals(((SeasonValue) obj).value);
	}
	
	
	public boolean smaller(Object obj) {
		try {
		    return(value.compareTo(((SeasonValue) obj).value) < 0);
		} catch (ClassCastException e) {
		    throw new EvaluatorException("Expecting a season");
		}
	}
	
	@Override
    public Season toSeason() {
        return value;
    }	
}