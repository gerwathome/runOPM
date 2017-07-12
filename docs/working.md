### Who Am I?
I am a reservoir engineer with a number of years experience building reservoir simulation models in a large corporate environment.  The models have ranged from simple to complex, and have been used for a variety of purposes from optimizing injection patterns and picking development locations to helping to justify major capital expense projects.

One of the problems of a working reservoir engineer is that you are always busy trying to get the trash out, and never have enough time to think more clearly about how to do the job better.  I am currently free of organizational encumberances, which means I have time to pursue interesting projects, but with limited resources to pursue them.  This makes working on open source tools an obvious choice, but expense is certainly not the only reason.

Oil companies often closely guard their internal software in a mistaken understanding of which parts of the business actually add value (probably because they have hired too many attorneys).  Large projects always have partners, and the primary goal is always to get both internal and partner support for a project.  Proprietary software is usually a hinderance, because each of the partners has a different view of the issues and problems.  A transparent work process powered with open source tools provides a better path for both reaching agreement, and for each of the partners to add their particular expertise in a way that can make the project move forward more efficiently and quickly.

The software tools which I have started working on are intended to provide an open source computer assisted methodolgy for history matching a reservoir simulation model.  In addition they are intended to support an open workflow to allow transparent collaboration among partners towards reaching understanding and agreement.

### Just What Do You Think You're Doing?
How should time be apportioned between developing, documenting and thinking?  The process of building a history matching workflow, taking advantage of existing tools, will require learning more about numerous modeling related subjects.  Creating tools will require learning more about programming in general, and R in particular.  Some C/C++/Fortran work may also be necessary when, performance becomes a problem.  My background and experience have been more as a user than as a developer.  Documenting the work as it proceeds will probably make progress slower, but will force clearer thinking.  Hopefully, this will result in a better product.

Documentation is a multilevel problem/opportunity.  The lowest level is to document functions as they are being developed  Using ROxygen eases this process to allow standard R Style help files to be quickly and easily generated.  The next level is to document workflows.  This will be an iterative process during tool creation using RMarkdown, and will be in the form of R Vignettes.  The highest level will be documentation of the research, thought processes, experimentation and problems during tool development.  That is the purpose of this blog.

### Possible Blog Topics
1. Introduction to the OPM project and tools
    - eclipse style io
    - ERT overlap
    - simulator types
    - QC against Eclipse
    - scale up tools
    - ResInsight
    - developer versus user documentation
    - issues with allowing unsupported features to run with unexpected results
    - High quality tools under rapid development, but with traps for the unwary    
2. A History Match Tool Development Project in the R Statistical Language
    - why--the rationale
    - first pass at the history match work process
    - the DICE tools
    - simplification of HM model
    - simplification of ED
    - variable selection and computational overhead
    - meta-modeling and optimization
    - plans
3. Which Space Filling Experimental Design?
4. Error Definition and Meta-Modeling
5. Sensitivity and Variable Selection
6. Expected Improvement (EI) and Efficient Global Optimization (EGO)
7. Multi-Objective Optimization and Pareto Modeling:  Getting GPareto and rgenoud to work
8. Testing of Scale-Up Tools
9. Reproducible Research and Modeling
10. Code Profiling and Optimization
11. More Challenging History Match Problems
12. Open Source Geologic Modeling Tools
13. Open Source Reservoir Engineering Tools

