package net.sf.clipsrules.jni;

public interface Router
  {
   public static final String STDOUT = "stdout";
   public static final String STDIN = "stdin";
   public static final String STDWRN = "stdwrn";
   public static final String STDERR = "stderr";

   public int getPriority();
   public String getName();
   public boolean query(String logicalName);
   public void write(String logicalName,String writeString);
   public int read(String logicalName);
   public int unread(String logicalName,int theChar);
   public void exit(boolean failure);
  }
