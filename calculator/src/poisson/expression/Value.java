package poisson.expression;

import java.util.Arrays;
import java.util.Map;
import java.util.stream.Collectors;

public class Value implements Expression {

    private final double value;

    public static final Value ZERO = new Value(0);
    public static final Value ONE = new Value(1);
    public static final Value MINUS_ONE = new Value(-1);

    public Value(final double value) {
        this.value = value;
    }

    @Override
    public Value evaluate(Map<Variable, Value> variableValueMap) {
        return this;
    }

    @Override
    public boolean dependsOn(Variable variable) {
        return false;
    }

    @Override
    public Expression differentiate(Variable variable) {
        return ZERO;
    }

    public double getValue() {
        return value;
    }

    @Override
    public String toString() {
        return Double.toString(value);
    }
}
