## Scalagen 

**Java to Scala conversion**

Scalagen is a Java to Scala conversion tool. It uses a Java based parser for Java sources and provides modular 
transformation of the AST to match Scala idioms. The resulting transformed AST is serialized into Scala format.

Here is a list of example Java sources which have been successfully converted by Scalagen:
https://github.com/mysema/scalagen/tree/master/scalagen/src/test/scala/com/mysema/examples

Scalagen has also been tested on our own projects such as Querydsl, RDFBean, Codegen and some customer projects.

### Usage

Scalagen provides direct Maven support via a plugin. You can use it directly via the command line like this

    mvn com.mysema.scalagen:scalagen-maven-plugin:0.3.2:main -DtargetFolder=target/scala
    
and for test sources

    mvn com.mysema.scalagen:scalagen-maven-plugin:0.3.2:test -DtargetFolder=target/scala

Here is the snippet for an explicit configuration in a POM:

    <plugin>
      <groupId>com.mysema.scalagen</groupId>
      <artifactId>scalagen-maven-plugin</artifactId>
      <version>0.3.2</version>
    </plugin>
    
To convert main sources run

    mvn scalagen:main
    
and to convert test sources run 

    mvn scalagen:test

The conversion results are to be seen as a starting point for the Java to Scala conversion. 
Some elements are not transformed correctly for various reasons and will need manual intervention.

### Development

Scalagen development instructions are here https://github.com/mysema/scalagen/wiki/Scalagen-development

Basic, common instructions when developing (installs dev versions into the local maven repo):

    cd scalagen
    mvn -Pscala-2.11.x -DskipTests install
    cd ..
    cd scalagen-maven-plugin
    mvn -Pscala-2.11.x -Dscala.repoVersion=2.11 -DskipTests install

#### Building

To create the jar run

    mvn -Pscala-2.11.x clean package

To import the project in to eclipse run

    mvn -Pscala-2.11.x eclipse:eclipse

The maven profiles (the part after the `-P`) can be:

 * `scala-2.11.x`
 * `scala-2.10.x`
 * `scala-2.9.x`

