/*
 * Copyright (C) 2011, Mysema Ltd
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 */
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
