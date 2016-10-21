package net.sf.clipsrules.jni;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Properties;

public class UserFunctionExamples 
  {  
   public static void addUserFunctions(
     Environment theEnv)
     {
      // (upcase "ßuzäöü")
      theEnv.removeUserFunction("upcase");
      theEnv.addUserFunction("upcase","11j",
            new UserFunction()
              {
               public PrimitiveValue evaluate(List<PrimitiveValue> arguments)
                 {
                  LexemeValue rv = (LexemeValue) arguments.get(0);
                                   
                  String urv = rv.getValue().toUpperCase();
                  if (rv.isString())
                    { return new StringValue(urv); }
                  else if (rv.isSymbol())
                    { return new SymbolValue(urv); }
                  else if (rv.isInstanceName())
                    { return new InstanceNameValue(urv); }
                                                             
                  return null;
                 }
              });
                               
      theEnv.removeUserFunction("lowcase");
      theEnv.addUserFunction("lowcase","11j",
            new UserFunction()
              {
               public PrimitiveValue evaluate(List<PrimitiveValue> arguments)
                 {
                  LexemeValue rv = (LexemeValue) arguments.get(0);
                                   
                  String lrv = rv.getValue().toLowerCase();
                  if (rv.isString())
                    { return new StringValue(lrv); }
                  else if (rv.isSymbol())
                    { return new SymbolValue(lrv); }
                  else if (rv.isInstanceName())
                    { return new InstanceNameValue(lrv); }
                                                             
                  return null;
                 }
              });

      theEnv.addUserFunction("cbrt","11n",
            new UserFunction()
              {
               public PrimitiveValue evaluate(List<PrimitiveValue> arguments)
                 {
                  NumberValue rv = (NumberValue) arguments.get(0);
                  return new FloatValue(Math.cbrt(rv.doubleValue()));
                 }
              });

       theEnv.addUserFunction("get-year","00",
             new UserFunction()
               {
                public PrimitiveValue evaluate(List<PrimitiveValue> arguments)
                  { 
                   Calendar theCalendar = new GregorianCalendar();
                   return new IntegerValue(theCalendar.get(theCalendar.YEAR));
                  }
               });
      
      theEnv.addUserFunction("get-properties","00",
            new UserFunction()
              {
               public PrimitiveValue evaluate(List<PrimitiveValue> arguments)
                 {
                  List<PrimitiveValue> values = new ArrayList<PrimitiveValue>();

                  Properties props = System.getProperties();
                  for (String key : props .stringPropertyNames())
                    { values.add(new SymbolValue(key)); }

                  return new MultifieldValue(values);
                 }
              });
      
      theEnv.addUserFunction("hello","00",
            new UserFunction()
              {
               public PrimitiveValue evaluate(List<PrimitiveValue> arguments)
                 {
                  theEnv.println("Hello World!");
                  return null;
                 }
              });

      theEnv.addUserFunction("make-instoid","00",
            new UserFunction()
              {
               public PrimitiveValue evaluate(List<PrimitiveValue> arguments)
                 {
                  try
                    {
                     return theEnv.eval("(instance-address (make-instance [hello] of INITIAL-OBJECT))");
                    }
                  catch (Exception e)
                    { return new SymbolValue("FALSE"); }
                 }
              });

      theEnv.addUserFunction("make-factoid","00",
            new UserFunction()
              {
               public PrimitiveValue evaluate(List<PrimitiveValue> arguments)
                 {
                  try
                    {
                     return theEnv.eval("(assert (hello world))");
                    }
                  catch (Exception e)
                    { return new SymbolValue("FALSE"); }
                 }
              });
     }
  }