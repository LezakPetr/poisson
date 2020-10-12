package poisson.expression;

import java.util.List;
import java.util.stream.IntStream;

public class Times extends Function {
    public Times(List<? extends Expression> args) {
        super(args);
    }

    @Override
    public Expression defferentiateAgainstParameter (final int parameterIndex, final Expression argument) {
        return IntStream.range(0, getArgumentCount())
                .filter(i -> i != parameterIndex)
                .mapToObj(this::getArgument)
                .reduce(Value.ONE, Expressions::times);
    }

    @Override
    public Value evaluateFunction (final List<Value> argumentValues) {
        return argumentValues.stream()
                .reduce(Value.ONE, (a, b) -> new Value (a.getValue() * b.getValue()));
    }
}
