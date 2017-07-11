package net.sf.clipsrules.jni;

import java.util.List;

public interface UserFunction 
  {
   public PrimitiveValue evaluate(List<PrimitiveValue> arguments);
   
   public static final int UNBOUNDED = -1;
  }
