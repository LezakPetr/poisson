package poisson.expression;

import java.util.List;
import java.util.Map;

public class Plus extends Function {
    public Plus(List<? extends Expression> args) {
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

}
