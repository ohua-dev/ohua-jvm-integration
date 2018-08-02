package ohua.tests;

import ohua.lang.defsfn;

public class TestCollectOperator
{ 
  private int _index = 0;
  
  @defsfn
  public Object[] collect(long value, long[] values){
    values[_index++] = value;
	System.out.print("Called TestCollectOperator: ");
	for (long l : values) {
		System.out.print(l + ",");
	}
	System.out.println("");
    return new Object[] {};
  }
}
