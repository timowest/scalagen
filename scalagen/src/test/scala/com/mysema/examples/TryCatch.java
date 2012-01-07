package com.mysema.examples;

import java.util.Arrays;

public class TryCatch {
    
    public void run() {
        try {
            for (String str : Arrays.asList("a","b","c")) {
                System.out.println(str);
            }
        } catch (IllegalArgumentException e) {
            throw new RuntimeException(e);
        } catch (NullPointerException e) {
            System.err.println(e.getMessage());
        } catch (Exception e) {
            System.err.println(e.getClass().getName());
            System.err.println(e.getMessage());
        } finally {
            System.out.println("done");
        }
    }

    void foo() {
        try {

        } finally {

        }
    }

    void bar() {
        try {
        } catch (Exception e) {
        }
    }

    void baz() {
        try {
            bar();
        } catch (Exception e) {
        } finally {
            foo();
        }
    }

    void buzz() {
        try {

        } finally {
            baz();
        }
    }
    
    void simplified() {
        try {
            System.getProperty("xxx").substring(1);
        } catch (NullPointerException n) {
            throw new RuntimeException(n);
        }
    }
    
    void simplified2() {
        try {
            System.getProperty("xxx").substring(1);
        } catch (IllegalArgumentException n) {
            throw new RuntimeException(n);
        } catch (NullPointerException n) {
            throw new RuntimeException(n);
        }
    }

}
