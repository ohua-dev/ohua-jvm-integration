package ohua.tests;

import ohua.lang.defsfn;

public abstract class MathOps
{ 
  public static class AddOperator{
    @defsfn
    public long add(int value, long s){
//		System.out.println("Called AddOperator: " + value + " + " + s + " = "  + (value + s));
      return value + s;
    }
  }

  public static class MultOperator{
    @defsfn
    public long mult(int value, long s){
//      System.out.println("Called MultOperator: " + value + " + " + s + " = "  + (value + s));
      return value * s;
    }
  }
  
  public static class SubtractOperator {
    @defsfn
    public long subtract(int value, long s){
//		System.out.println("Called SubtractOperator: " + value + " - " + s + " = " + (value - s));
      return value - s;
    }
  }

  public static class GreaterThan {
    @defsfn
    public boolean greaterThan(int value, long s){
      return value < s;
    }
  }
}
