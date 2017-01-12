package com.mysema.examples;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
/**
 * compile: -Xexperimental
 */
public class Java8Stream {

    public static void main(String[] args) {

        List<Integer> myList = new ArrayList<>();

        Stream<Integer> stream = myList.stream();

        // Lambda with explicit type
        Stream<Integer> highNums = stream.filter((Integer p) -> p > 90);
        // Lambda without explicit type
        highNums.forEach(p -> System.out.println("High Nums parallel="+p));

        // Method reference
        Stream<Integer> highNumsSeq = stream.filter(Java8Stream::greatherThan90);
        Function3 <String, Integer, Double, Double> multiAdder = (a, b, c) -> {
            return Double.parseDouble (a) + b + c;
        };
    }

    @FunctionalInterface
    interface Function3 <A, B, C, R> {
        //R is like Return, but doesn't have to be last in the list nor named R.
        public R apply (A a, B b, C c);
    }

    private static boolean greatherThan90(Integer i) {
        return i > 90;
    }
}
