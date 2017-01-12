package com.mysema.examples;

public class Java8MultiCatch {
    public void multiCatch() {
        try {
            throw new IllegalArgumentException("foo");
        } catch(IllegalArgumentException | IllegalAccessError e) {
            System.out.println("Got some error: " + e);
        }
    }
}
