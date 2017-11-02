package ohua.tests;

import ohua.lang.defsfn;

public abstract class TestProduceOperator {
  
  public static class SimpleProducer {
    @defsfn
    public int produce() {
        return  111 ;
    }
  }
  
  public static class FnProducer extends SimpleProducer {
    @defsfn
    public Object[] produceFn() {
      int r = super.produce();
      return new Object[] {r , new TestConsumeOperator() };
    }
  }
  
}
