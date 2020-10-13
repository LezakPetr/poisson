package poisson.expression;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class Sum extends Function {
    public Sum(List<? extends Expression> args) {
        super(args);
    }

    @Override
    public Expression defferentiateAgainstParameter (final int parameterIndex, final Expression argument) {
        return Value.ONE;
    }

    @Override
    public Value evaluateFunction (final List<Value> argumentValues) {
        return argumentValues.stream()
                .reduce(Value.ZERO, (a, b) -> new Value (a.getValue() + b.getValue()));
    }

    @Override
    public String toString() {
        return getArguments().stream()
                .map(arg -> "(" + arg + ")")
                .collect(Collectors.joining(" + "));
    }

}
