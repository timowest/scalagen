package com.mysema.scalagen;

import java.util.List;

public class ExampleWildcard {
    
    int foo(List<?> list) { 
        return list.size(); 
    }

    int bar(List<? extends CharSequence> list) { 
        return list.size();
    }
}
