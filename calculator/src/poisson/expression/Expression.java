package poisson.expression;

import java.util.Map;

public interface Expression {
    public Value evaluate (final Map<Variable, Value> variableValueMap);
    public boolean dependsOn (final Variable variable);
    public Expression differentiate (final Variable variable);
}
