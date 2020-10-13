package poisson.expression;

import java.util.List;

public class Negation extends Function {

    protected Negation(final Expression argument) {
        super(List.of(argument));
    }

    @Override
    Expression defferentiateAgainstParameter(final int parameterIndex, final Expression argument) {
        return Value.MINUS_ONE;
    }

    @Override
    Value evaluateFunction(final List<Value> argumentValues) {
        return new Value(-argumentValues.get(0).getValue());
    }

    @Override
    public String toString() {
        return "-(" + getArgument(0) + ")";
    }
}
