package poisson.expression;

import java.util.List;

public class Expressions {
    public static Expression plus (final Expression ...args) {
        return plus(List.of(args));
    }

    public static Expression plus(final List<? extends Expression> args) {
        return new Plus(args);
    }

    public static Expression times (final Expression ...args) {
        return times(List.of(args));
    }

    private static Expression times(final List<? extends Expression> args) {
        return new Times(args);
    }

}
