package com.mysema.scalagen;

public class ExampleImmutable2 {
    
    private final String firstName;
    
    private final String lastName;
    
    public ExampleImmutable2(String f, String l) {
        firstName = f;
        lastName = l;
    }

    public String getFirstName() {
        return firstName;
    }

    public String getLastName() {
        return lastName;
    }
    
}
