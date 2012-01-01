package com.mysema.scalagen;

public class ExampleReturns {
    
    public int shouldPrintReturns(int start, int n) {
        for (int i = start; i < n; ++i)
            if (i / 5 > 1)
                return i;
        return -1;
    }
    
    public void shouldPrintReturns2(int start, int n) {
        for (int i = start; i < n; ++i)
            if (i / 5 > 1) {
                 doSomething(i);
                return;
            }               
    }
    
    public void doSomething(int n) {
        
    }

    public boolean shouldNotPrintReturns(boolean bool) {
        return bool;
    }
}