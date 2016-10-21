package net.sf.clipsrules.jni;

import java.util.EventObject;

public class CommandExecutionEvent extends EventObject
  {
   public static final String START_EVENT = "start";
   public static final String PERIODIC_EVENT = "periodic";
   public static final String FINISH_EVENT = "finish";

   private String executingCommand;
   private String executionEvent;

   public CommandExecutionEvent(
     Object source,
     String executingCommand,
     String executionEvent)
     {
      super(source);
      this.executingCommand = executingCommand;
      this.executionEvent = executionEvent;
     }
  
   public String getExecutingCommand()
     { return executingCommand; }
     
   public String getExecutionEvent()
     { return executionEvent; }
  }
