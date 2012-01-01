package com.mysema.scalagen;

public class ExampleArrays {

    int[] foo() { 
        return new int[2]; 
    }

    void bar() {
//        var foo:Array[Object] = Array[Object](new Object())
//        var bar:Array[char] = Array('f', 'o', 'o')
        Object[] foo = new Object[] { new Object() };
        char[] bar = new char[] { 'f', 'o', 'o' };
    }
    
}
