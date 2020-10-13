package poisson.expression;

import java.util.Objects;

public class Variable {
    private final String name;

    public Variable(final String name) {
        this.name = Objects.requireNonNull(name);
    }

    @Override
    public String toString() {
        return name;
    }
}
