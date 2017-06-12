package net.sf.clipsrules.jni;

public interface Router
  {
   public static final String STANDARD_OUTPUT = "stdout";
   public static final String STANDARD_INPUT = "stdin";
   public static final String WARNING = "wwarning";
   public static final String ERROR = "werror";

   public int getPriority();
   public String getName();
   public boolean query(String routerName);
   public void print(String routerName,String printString);
   public int getchar(String routerName);
   public int ungetchar(String routerName,int theChar);
   public boolean exit(int exitCode);
  }
