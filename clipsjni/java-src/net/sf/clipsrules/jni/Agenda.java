package net.sf.clipsrules.jni;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class Agenda implements Iterable<Activation>
  {
   private List<Activation> activations;
   
   /***********/
   /* Agenda: */
   /***********/
   public Agenda()
     {
      this.activations = new ArrayList<Activation>();
     }

   /***********/
   /* Agenda: */
   /***********/
   public Agenda(
     List<Activation> activations)
     {
      this.activations = activations;
     }

   /*******************/
   /* getActivations: */
   /*******************/
   public List<Activation> getActivations()
     {
      return this.activations;
     }

   /*************/
   /* hashCode: */
   /*************/
   @Override
   public int hashCode()
     {
      if (activations == null) return 0;
      return activations.hashCode();
     }
     
   /***********/
   /* equals: */
   /***********/
	@Override
	public boolean equals(Object obj) 
	  {
	   if (this == obj) return true;
	   if (obj == null) return false;
	   if (this.getClass() != obj.getClass()) return false;
	   
	   Agenda ca = (Agenda) obj;
	   if (this.activations == null) return (ca.activations == null);
	   return this.activations.equals(ca.activations);
      }
      
   /********/
   /* get: */
   /********/
   public Activation get(
     int index)
     {
      final List<Activation> activations = getActivations();
      
      return activations.get(index);
     }
     
   /*********/
   /* size: */
   /*********/
   public int size()
     {
      final List<Activation> activations = getActivations();
      
      return activations.size();
     }
     
   /************/
   /* iterator */
   /************/ 
   public Iterator<Activation> iterator() 
     {
      return getActivations().iterator();
     }
  }