package project.visitors.evaluation;


public class IntValue extends PrimValue<Integer> {

	public IntValue(Integer value) {
		super(value);
	}
	
	@Override
	public final boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!(obj instanceof IntValue))
			return false;
		return value.equals(((IntValue) obj).value);
	}
	
	@Override
	public boolean smaller(Object obj) {
		try {
			return(value.compareTo(((IntValue) obj).value) < 0);
		} catch (ClassCastException e) {
			throw new EvaluatorException("Expecting an integer");
		}
	}
	
	@Override
	public int toInt() {
		return value;
	}
	


}
