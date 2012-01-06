package com.mysema.scalagen;

import java.io.File;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;

/**
 * @phase generate-sources
 * @goal scalagentest
 *
 */
public class ScalagenTestMojo extends AbstractMojo {
    
    /**
    * @parameter expression="${project}" readonly=true required=true
    */
    private MavenProject project;

    /**
     * @parameter default-value="src/test/scala"
     */
    private String targetFolder;
    
    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        File in = new File(project.getBuild().getTestSourceDirectory());
        if (in.exists()) {
            File out = new File(targetFolder); 
            Converter.instance().convert(in, out);    
        } else {
            throw new MojoFailureException(in.getPath() + " doesn't exist");
        }
        
    }

}
