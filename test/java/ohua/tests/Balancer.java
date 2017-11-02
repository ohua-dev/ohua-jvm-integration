/*
 * Copyright (c) Sebastian Ertel 2014. All Rights Reserved.
 * 
 * This source code is licensed under the terms described in the associated LICENSE.TXT file.
 */

package ohua.tests;

import java.util.Arrays;
import ohua.lang.defsfn;

// import com.ohua.lang.compile.analysis.qual.ReadOnly;

/**
 * Use this operator in combination with a macro in order to replicate code and produce more
 * opportunities for data parallelism.
 * 
 * @author sertel
 *
 */
public class Balancer {
  private int _last = -1;
  
  @defsfn
  public Object[] balance(/* @ReadOnly */ Object[] arg, long count, long restriction) {
    _last = (_last + 1) % (int) restriction;
    Object[] result = new Object[(int) count];
    // this might not even be necessary
    Arrays.fill(result, null);
    // TODO this is a good example for the support of multiple dispatch inside an operator!
    result[_last] = arg.length == 1 ? arg[0] : arg;
    return result;
  }
  
}
