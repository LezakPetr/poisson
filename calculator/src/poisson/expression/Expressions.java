package poisson.expression;

import java.util.List;

public class Expressions {
    public static Expression sum(final Expression ...args) {
        return sum(List.of(args));
    }

    public static Expression sum(final List<? extends Expression> args) {
        return new Sum(args);
    }

    public static Expression product(final Expression ...args) {
        return product(List.of(args));
    }

    private static Expression product(final List<? extends Expression> args) {
        return new Product(args);
    }

}
