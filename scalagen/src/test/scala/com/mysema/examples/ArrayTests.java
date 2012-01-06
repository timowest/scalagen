package com.mysema.examples;

public class ArrayTests {

    int[] foo() { 
        return new int[2]; 
    }

    void bar() {
//        var foo:Array[Object] = Array[Object](new Object())
//        var bar:Array[char] = Array('f', 'o', 'o')
        Object[] foo = new Object[] { new Object() };
        char[] bar = new char[] { 'f', 'o', 'o' };
    }
    
    void bar2() {
        String el1s[] = new String[]{"a","b"};
        String el2s[] = new String[]{"a","b","c"};
    }
}
