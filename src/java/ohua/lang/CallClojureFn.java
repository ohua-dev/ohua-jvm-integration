/*
 * ohua : CallClojureFunc.java
 *
 * Copyright (c) Sebastian Ertel, Justus Adam 2016. All Rights Reserved.
 *
 * This source code is licensed under the terms described in the associated LICENSE.TXT file.
 */

package ohua.lang;

import clojure.java.api.Clojure;
import clojure.lang.IFn;
import java.util.Arrays;

/**
 * Created by justusadam on 30/08/16.
 */
public final class CallClojureFn {
    private final IFn apply_ = Clojure.var("clojure.core/apply");

    @defsfn
    public Object __callClojureFn(IFn function, Object... args) {
        try {
            return apply_.invoke(function, args);   
        } catch (Throwable e) {
            throw new ClojureCallException(function, args, e);
        }
    }

    public final static class ClojureCallException extends RuntimeException {
        private final IFn function;
        private final Object[] arguments;
        public ClojureCallException(IFn function, Object[] arguments, Throwable e) {
            super(e);
            this.function = function;
            this.arguments = arguments;
        }

        @Override
        public String getLocalizedMessage() {
            return "Calling function " + function + " on " + Arrays.deepToString(arguments) + " caused " + super.getLocalizedMessage();
        }
    }
}
