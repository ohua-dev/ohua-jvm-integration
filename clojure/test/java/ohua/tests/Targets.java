package ohua.tests;

import java.util.List;
import ohua.lang.defsfn;

public abstract class Targets
{ 
  public static class ListCollect{
    @defsfn
    public Object[] lCollect(Object value, List output){
      output.add(value);
      return new Object[0];
    }
  }
  
}
