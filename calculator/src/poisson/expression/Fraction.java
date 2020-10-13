package poisson.expression;

import java.util.List;

public class Fraction extends Function {

    public static final int NUMERATOR_INDEX = 0;
    public static final int DENOMINATOR_INDEX = 1;

    public Fraction(final Expression numerator, final Expression denominator) {
        super(List.of(numerator, denominator));
    }

    @Override
    public Expression defferentiateAgainstParameter(final int parameterIndex, final Expression argument) {
        switch (parameterIndex) {
            case NUMERATOR_INDEX:
                return new Power(getArgument(DENOMINATOR_INDEX), -1);

            case DENOMINATOR_INDEX:
                return new Negation(new Power(getArgument(DENOMINATOR_INDEX), -2));

            default:
                throw new RuntimeException("Unknown parameter");
        }
    }

    @Override
    public Value evaluateFunction(final List<Value> argumentValues) {
        return new Value(argumentValues.get(NUMERATOR_INDEX).getValue() / argumentValues.get(DENOMINATOR_INDEX).getValue());
    }

    @Override
    public String toString() {
        return "(" + getArgument(NUMERATOR_INDEX) + ") / (" + getArgument(DENOMINATOR_INDEX) + ")";
    }
}
