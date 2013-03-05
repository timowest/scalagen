package com.mysema.examples;

public final class Concept extends Item {

    private Object term;    
    private Object memory;

    public Concept(Object tm, Object memory) {
        super(tm.toString());
        term = tm;
        this.memory = memory;
    }

    public Object getTerm() {
        return term;
    }

    public void setTerm(Object term) {
        this.term = term;
    }

    public Object getMemory() {
        return memory;
    }

    public void setMemory(Object memory) {
        this.memory = memory;
    }
    
}    

class Item {
    
    public Item(String str) {
        // TODO Auto-generated constructor stub
    }
}