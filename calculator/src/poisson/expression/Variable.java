package poisson.expression;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicLong;

public class Variable {
    private final String name;

    public Variable(final String name) {
        this.name = Objects.requireNonNull(name);
    }
}
