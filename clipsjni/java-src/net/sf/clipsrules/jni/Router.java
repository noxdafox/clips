package net.sf.clipsrules.jni;

public interface Router
  {
   public static final String STANDARD_OUTPUT = "stdout";
   public static final String STANDARD_INPUT = "stdin";
   public static final String WARNING = "wwarning";
   public static final String ERROR = "werror";

   public int getPriority();
   public String getName();
   public boolean query(String logicalName);
   public void print(String logicalName,String printString);
   public int getchar(String logicalName);
   public int ungetchar(String logicalName,int theChar);
   public void exit(boolean failure);
  }
