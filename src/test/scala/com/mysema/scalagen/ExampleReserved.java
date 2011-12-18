package com.mysema.scalagen;

public class ExampleReserved {

    String type = "a";
    String var = "b";
    String val = "c";
    String object = "d";
    
    public void reservedWords() {
        String type = "a";
        String var = "b";
        String val = "c";
        String object = "d";
        System.err.println(type + var + val + object);
    }
    
}
