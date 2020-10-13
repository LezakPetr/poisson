package poisson.expression;

import java.util.List;

public class Power extends Function {

    private final int exponent;

    public Power(final Expression base, final int exponent) {
        super(List.of(base));

        this.exponent = exponent;
    }

    @Override
    public Expression defferentiateAgainstParameter(final int parameterIndex, final Expression argument) {
        return Expressions.product(new Value(exponent), new Power(getArgument(0), exponent - 1));
    }

    @Override
    public Value evaluateFunction(final List<Value> argumentValues) {
        return new Value(Math.pow(argumentValues.get(0).getValue(), exponent));
    }

    @Override
    public String toString() {
        if (exponent >= 0)
            return "(" + getArgument(0) + ")^" + exponent;
        else
            return "(" + getArgument(0) + ")^(" + exponent + ")";
    }
}
