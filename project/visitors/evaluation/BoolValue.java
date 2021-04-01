package project.visitors.evaluation;

public class BoolValue extends PrimValue<Boolean> {

	public BoolValue(Boolean value) {
		super(value);
	}

	@Override
	public final boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!(obj instanceof BoolValue))
			return false;
		return value.equals(((BoolValue) obj).value);
	}

		
	@Override
	public final boolean smaller(Object obj) {
		try {
			return(value.compareTo(((BoolValue) obj).value) < 0);
		} catch (ClassCastException e) {
			throw new EvaluatorException("Expecting a boolean");
		}
	}
	
	@Override
	public boolean toBool() {
		return value;
	}

}