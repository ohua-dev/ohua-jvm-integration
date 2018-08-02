package ohua.tests;

import ohua.lang.defsfn;

/**
 * Created by sertel on 8/23/16.
 */
public class Fail {
  @defsfn
  public boolean fail(){
    throw new RuntimeException("FAIL!");
  }
}
