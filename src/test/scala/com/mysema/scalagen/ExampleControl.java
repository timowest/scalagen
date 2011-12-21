package com.mysema.scalagen;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

public class ExampleControl {
    
    List<Integer> integers = Arrays.asList(1,2,3,4,5);
    
    public void whileLoop() {
        int i = 0;
        while (i < integers.size()) {
            System.err.println(integers.get(i));
            i++;
        }
    }
    
    public void whileLoop2() {
        Iterator<Integer> i = integers.iterator();
        while (i.hasNext()) {
            System.err.println(i.next());
        }
    }
    
    public void forLoop() {
        for (int i = 0; i < integers.size(); i++) {
            System.err.println(integers.get(i));
        }
    }
    
    public void forLoop2() {
        for (Integer i : integers) {
            System.err.println(i);
        }
    }
    
    public void forLoop3() {
        for (Iterator<Integer> i = integers.iterator(); i.hasNext(); ) {
            System.err.println(i.next());
        }
    }
    
    public void forLoopWithIf() {
        for (Integer i : integers) {
            if (i > 0) {
                System.err.println(i);    
            }            
        }
    }
        
        

}
