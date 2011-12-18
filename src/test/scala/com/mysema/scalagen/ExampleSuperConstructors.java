package com.mysema.scalagen;

public class ExampleSuperConstructors extends SuperClass {
    
    public ExampleSuperConstructors() {
        this("first", "last");
    }
    
    public ExampleSuperConstructors(String first, String last) {
        super(first);
    }

}

class SuperClass {
    
    public SuperClass(String first) {
    }
}
