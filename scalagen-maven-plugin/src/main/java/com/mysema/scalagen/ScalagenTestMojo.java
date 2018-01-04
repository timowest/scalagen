/*
 * Copyright (C) 2011, Mysema Ltd
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.mysema.scalagen;

import java.io.File;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Parameter;

@Mojo( name = "test", defaultPhase = LifecyclePhase.GENERATE_SOURCES )
public class ScalagenTestMojo extends AbstractMojo {

    @Parameter( defaultValue = "${project}", readonly = true, required = true )
    private MavenProject project;

    @Parameter( defaultValue = "src/test/scala" )
    private String targetFolder;

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        final File in = new File(project.getBuild().getTestSourceDirectory());
        if (in.exists()) {
            final File out = new File(targetFolder);
            Converter.instance().convert(in, out);
        } else {
            throw new MojoFailureException(in.getPath() + " doesn't exist");
        }
    }

    public void setProject(final MavenProject project) {
        this.project = project;
    }

    public void setTargetFolder(final String targetFolder) {
        this.targetFolder = targetFolder;
    }
}
