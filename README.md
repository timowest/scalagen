**Scalagen - Java to Scala conversion**

Scalagen provides direct Maven support via a plugin.

You can use it directly via the command line like this

    mvn com.mysema.scalagen:scalagen-maven-plugin:0.1.3:main -DtargetFolder=target/scala
    
and for test sources

    mvn com.mysema.scalagen:scalagen-maven-plugin:0.1.3:test -DtargetFolder=target/scala

Here is the snippet for an explicit configuration in a POM:

    <plugin>
      <groupId>com.mysema.scalagen</groupId>
      <artifactId>scalagen-maven-plugin</artifactId>
      <version>0.1.3</version>
    </plugin>
    
To convert main sources run

    mvn scalagen:main
    
and to convert test sources run 

    mvn scalagen:test

The conversion results are to be seen as a starting point for the Java to Scala conversion. 
Some elements are not transformed correctly for various reasons and will need manual intervention.
