package com.mysema.scalagen;

public class ExampleImmutable {
    
    public static void main(String[] args) {
        ExampleImmutable immutable = new ExampleImmutable("John", "Doe");
        System.out.println(immutable.getFirstName());
        System.out.println(immutable.getLastName());
    }
    
    private final String firstName;
    
    private final String lastName;

    public ExampleImmutable(String firstName, String lastName) {
        this.firstName = firstName;
        this.lastName = lastName;
    }

    public String getFirstName() {
        return firstName;
    }

    public String getLastName() {
        return lastName;
    }
    
}
