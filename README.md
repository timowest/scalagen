= Scalagen - Java to Scala conversion =

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