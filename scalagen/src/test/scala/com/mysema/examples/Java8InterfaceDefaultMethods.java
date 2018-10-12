package com.mysema.examples;

import java.util.Iterator;
import java.util.Objects;
import java.util.function.Consumer;

public class Java8InterfaceDefaultMethods {
    public interface Printable {
        void print();
        default void printMe() {
            System.out.println("It's me!");
        }
    }
}
