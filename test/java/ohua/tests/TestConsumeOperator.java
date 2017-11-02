package ohua.tests;

import ohua.lang.defsfn;
//import aua.analysis.qual.Untainted;

public class TestConsumeOperator {
	
	@defsfn
	public Object[] consume(int value, int[] capture) {
	  capture[0] = value;
//		capture[0] = result(value);
		return new Object[] {};
	}
	
	//@Untainted
	private int result(int value){
	  return value;
	}
	
}
