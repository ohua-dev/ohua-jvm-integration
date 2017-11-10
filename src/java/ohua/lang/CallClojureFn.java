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

/**
 * Created by justusadam on 30/08/16.
 */
public final class CallClojureFn {
    private final IFn apply_ = Clojure.var("clojure.core/apply");

    @defsfn
    public Object __callClojureFn(IFn function, Object... args) {
        return apply_.invoke(function, args);
    }
}
