package com.mysema.scalagen;

public class ExampleImmutable {
    
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
