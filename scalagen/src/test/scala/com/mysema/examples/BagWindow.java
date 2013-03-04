package com.mysema.examples;

public class BagWindow extends BagWindowSuper {

    Bag<?> bag;
    
    String str;
    
    public BagWindow(Bag<?> b, String title) {
        super(title);
        bag = b;
        str = "abc";
    }
    
}

class Bag<T> {
    
}

class BagWindowSuper {
    
    public BagWindowSuper(String title) {
        // do nothing
    }
}