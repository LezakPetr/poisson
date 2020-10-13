package poisson.expression;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static poisson.expression.Expressions.*;

public abstract class Function implements Expression {
    private final List<? extends Expression> arguments;

    protected Function(final List<? extends Expression> arguments) {
        this.arguments = List.copyOf(arguments);
    }

    abstract Expression defferentiateAgainstParameter (final int parameterIndex, final Expression argument);
    abstract Value evaluateFunction (final List<Value> argumentValues);

    public boolean dependsOn (final Variable variable) {
        return arguments.stream().anyMatch(arg -> arg.dependsOn(variable));
    }

    public Expression differentiate (final Variable variable) {
        List<Expression> argumentDerivatives = new ArrayList<>();

        for (int i = 0; i < arguments.size(); i++) {
            final Expression arg = arguments.get(i);

            if (arg.dependsOn(variable)) {
                argumentDerivatives.add(
                  product(defferentiateAgainstParameter(i, arg), arg.differentiate(variable))
                );
            }
        }

        return sum(argumentDerivatives);
    }

    @Override
    public Value evaluate(final Map<Variable, Value> variableValueMap) {
        final List<Value> argumentValues = arguments.stream()
                .map(arg -> arg.evaluate(variableValueMap))
                .collect(Collectors.toUnmodifiableList());

        return evaluateFunction(argumentValues);
    }

    public Expression getArgument (final int index) {
        return arguments.get(index);
    }

    public int getArgumentCount() {
        return arguments.size();
    }

    public List<? extends Expression> getArguments() {
        return arguments;
    }

}
