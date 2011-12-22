package com.mysema.scalagen;

public class ExampleInitializers {
    
    static final String staticValue;
    
    static {
        staticValue = "xx";
    }
    
    final String value;
    
    {
        value = "x";
    }

}
