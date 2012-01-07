Scalagen - Java to Scala conversion

Add the following snippet to your Maven config to use Scalagen

    <plugin>
      <groupId>com.mysema.scalagen</groupId>
      <artifactId>scalagen-maven-plugin</artifactId>
      <version>0.1.0</version>
    </plugin>
    
To convert main sources run

    mvn scalagen:scalagen
    
and to convert test sources run 

    mvn scalagen:scalagentest

The conversion results are to be seen as a starting point for the Java to Scala conversion. 
Some elements are not transformed correctly for various reasons and will need manual intervention.
