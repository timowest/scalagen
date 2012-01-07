package com.mysema.examples;

public class IfElse {
    
    public void ifElse() {
        String property = "x";
        if (System.currentTimeMillis() > 0) {
            property = "y";
        } else {
            property = "z";
        }
        System.out.println(property);
    }

}
