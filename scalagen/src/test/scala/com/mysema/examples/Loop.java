package com.mysema.examples;

public class Loop {

    boolean method(String str) {
        for (String b : java.util.Arrays.asList("a", "b")) {
            if (str.startsWith(b)) {
                return true;
            }
        }
        return false;
    }

}
