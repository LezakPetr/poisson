package poisson.expression;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Product extends Function {
    public Product(List<? extends Expression> args) {
        super(args);
    }

    @Override
    public Expression defferentiateAgainstParameter (final int parameterIndex, final Expression argument) {
        return IntStream.range(0, getArgumentCount())
                .filter(i -> i != parameterIndex)
                .mapToObj(this::getArgument)
                .reduce(Value.ONE, Expressions::product);
    }

    @Override
    public Value evaluateFunction (final List<Value> argumentValues) {
        return argumentValues.stream()
                .reduce(Value.ONE, (a, b) -> new Value (a.getValue() * b.getValue()));
    }

    @Override
    public String toString() {
        return getArguments().stream()
                .map(arg -> "(" + arg + ")")
                .collect(Collectors.joining(" * "));
    }
}
