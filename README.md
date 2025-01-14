
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Introduction to R for Applied Epidemiology

<img src = "https://github.com/appliedepi/epiRhandbook_eng/raw/master/images/Applied_Epi_logo.png" height = "200" align = "right">

[This website](https://appliedepi.github.io/intro_course) hosts training
materials for “Introduction to R for Applied Epidemiology”. This course
teaches the fundamentals of R for applied epidemiologists and public
health practitioners.

[**Applied Epi**](www.appliedepi.org) is a nonprofit organization
supporting frontline practitioners through open-course analytical tools,
training, and support. Our [Epidemiologist R
Handbook](www.epirhandbook.com) is a free R reference manual which has
been used by 130,000 people around the world.

# Initial setup

Follow these step-by-step instructions to download the course files,
setup an RStudio project, and to download and begin the course’s
interactive exercises.

<!--
NOTE: everything inside the details tag will be collapsed and effectively
hidden from the user
-->
<details markdown=1>
<summary markdown="span" style='text-decoration: underline'>

1.  Download course files
    </summary>

[**Click
here**](https://minhaskamal.github.io/DownGit/#/home?url=https://github.com/appliedepi/intro_course/tree/main/intro_course)
to download a zipped folder to use in the course exercises.

**Unzip the folder and save it on your computer’s desktop (not on a
shared drive).** To “unzip” a folder once it is downloaded, right-click
on the folder and select “Extract All”. If offered a choice of location
to save the unzipped folder, save it to your desktop.

</details>
<details markdown=1>
<summary markdown="span" style='text-decoration: underline'>

2.  Create a new RStudio project
    </summary>

<!-- -->

1)  Open RStudio. Ensure that you open *RStudio* and not just *R*.

2)  In RStudio click *File -\> New Project*. In the pop-up window,
    select **“Existing directory”**.

<img src="images/README_images/create_project.png" width="50%" />

3)  Click “browse” and select the “intro_course” folder on your desktop,
    that you downloaded earlier, which contains the course materials.

4)  Click “Create project”

Voila! This will be the project for ALL of your work in this course.

</details>
<details markdown=1>
<summary markdown="span" style='text-decoration: underline'>

3.  Download Applied Epi course exercises
    </summary>

The rest of the course will utilize the R package {learnr} to provide
interactive exercises *within* your RStudio window. To access these
custom exercises, follow these instructions below. If you need help,
notify your breakout facilitator.

**Copy and paste this command into your R Console as shown below, then
press the “Enter” key to run the command:**

**`remotes::install_github("appliedepi/introexercises", dep = TRUE, force = TRUE)`**

<img src="images/README_images/install_exercises.png" width="75%" />

Text will begin to appear in the Console, below the command. Do not
worry, R is simply downloading the tutorials from the internet and
printing updates. *Watch the print-out for any questions that R may ask
you:*

-   If you see a printed warning about needing “RTools”, do not worry.
    This is simply a warning message and the install should proceed
    without error.  
-   If prompted to update some R packages, select “All” to update all of
    the packages.  
-   If you continue to be prompted for package updates, you can select
    “None” and continue without further updates.

</details>
<details markdown=1>
<summary markdown="span" style='text-decoration: underline'>

4.  Begin the first exercise
    </summary>

The course exercises will appear *within your RStudio*. Each course
module has a corresponding exercise, which can be accessed through the
“Tutorials” pane in RStudio (upper-left). The gif below introduces you
to the exercise environment (you do not need to follow the steps shown
right now).

<img src="images/README_images/exercise_demo_short_words.gif" width="100%" />

1)  Click on the “Tutorial” tab in the upper-right RStudio pane (which
    also contains a tab holding your “Environment”).

2)  Select the exercise “Applied Epi - R setup, syntax, data import”

-   If you do not see any “Applied Epi” exercises, notify your
    instructor. They make some time to appear.  
-   The exercise will load. Once you see the Applied Epi logo appear in
    the Tutorials pane, you can begin the exercise.  
-   To see the sidebar in the exercise, you may need to adjust the
    Tutorials pane to be wider. You can also adjust the zoom from the
    “View” menu.  
-   You can view the exercise in this pane, or click the small icon in
    the upper-left to pop-out into a separate window.

</details>

# Modules

<!-- badges: start -->
<!-- badges: end -->

## Module 1: Introduction to R

We welcome you to the course and dive into the basics of how to interact
with R and RStudio, basic R syntax, and how to organize your analytical
projects using public health examples. We then cover R functions and
packages, and introduce the core functions used to import data. Using
these, we import the Ebola case study surveillance linelist, and begin
to inspect and review it.

-   [Slides: Welcome, course logistics, RStudio, and basic R
    syntax](https://appliedepi.github.io/intro_course/modules/module_1/slides/course_introduction/slides_course_introduction.html)

-   Live demonstration ([instructor
    guide](https://appliedepi.github.io/intro_course/modules/module_1/guides/rstudio_tour.html))

-   Exercise:

<img src="images/README_images/pointer_tutorial_setup.png" width="50%" />

## Module 2: Data cleaning

Now that we have our surveillance linelist in R, we cover what “data
cleaning” steps are necessary and how to execute these in R. Along the
way, we introduce many of the core R functions including adjusting
column names, deduplicating and filtering rows, selecting and modifying
columns, recoding values, and more. Together, we write a sequence of
“pipes” to clean the linelist step-by-step in a clear, reproducible
manner… so that our dataset is ready for preliminary analysis!

-   [Slides: Tidy data and data
    cleaning](https://appliedepi.github.io/intro_course/modules/module_2/slides/slides_cleaning_dplyr/slides_cleaning_dplyr.html)

-   Exercise:

<img src="images/README_images/pointer_tutorial_cleaning.png" width="50%" />

## Module 3: Grouping data and making summary tables

Informative tables are the bedrock of epidemiological and public health
practice. In this module we introduce three tools to produce tables of
summary statistics: {dplyr} for flexibility, {janitor} for speed, and
{gtsummary} for beauty. Finally, we explore {flextable}, which can be
used to beautify any of the above approaches, add colors and highlights,
and save tables to Word, PNG, HTML, etc.

-   [Slides: Grouping and summarizing
    data](https://appliedepi.github.io/intro_course/modules/module_3/slides/slides_summary_tables/slides_grouping_summaries.html)

-   Exercise:

<img src="images/README_images/pointer_tutorial_grouping.png" width="50%" />

## Module 4: Data visualization with {ggplot2}

Using the {ggplot2} package to maximum effect rests upon understanding
how to apply its “grammar of graphics” to build a plot layer-by-layer.
We tackle this by introducing the grammer piece-by-piece so that you
build upon previous knowledge to construct informative and colorful bar
plots, scatter plots, histograms, line plots, text plot labels that
automatically refresh with updated data (very useful for epidemiological
reports!), and more.

-   [Slides: Data visualization with
    ggplot2](https://appliedepi.github.io/intro_course/modules/module_4/slides/slides_ggplot_basics/slides_ggplot_basics.html)

-   Exercise:

<img src="images/README_images/pointer_tutorial_ggplot_basics.png" width="50%" />

-   [Slides: Scales, themes, and
    labels](https://appliedepi.github.io/intro_course/modules/module_4/slides/slides_ggplot_scales_labels/slides_ggplot_scales_labels.html)

-   Exercise:

<img src="images/README_images/pointer_tutorial_ggplot_scales.png" width="50%" />

## Module 5: Transforming data

Public health analytics rarely involves just one data set, so now we
practice joining data by adding hospital, laboratory, and case
investigation data to our surveillance linelist. We ingrain best
practices for conducting joins, and prepare you for doing data
transformations independently. In the second part of this module, we
address *pivoting*, which in R means transforming data between “long”
and “wide” formats. This is particularly relevant in public health,
where each format has distinct benefits.

-   [Slides: Joining
    data](https://appliedepi.github.io/intro_course/modules/module_5/slides/slides_joins/slides_joins.html)

-   Exercise:

<img src="images/README_images/pointer_tutorial_joining.png" width="50%" />

-   [Slides: Pivoting
    data](https://appliedepi.github.io/intro_course/modules/module_5/slides/slides_pivots/slides_pivots.html)

-   Exercise:

<img src="images/README_images/pointer_tutorial_pivoting.png" width="50%" />

## Module 6: More data visualization with {ggplot2}

In this second data visualization module we encourage you to practice
learning R independently (a necessary skill once you leave the class!)
but with our support. We tackle visualizations that are central to
descriptive epidemiology: the intricacies of crafting an accurate
epidemic curve, conveying patterns in three variables using a heat plot,
and creating age/sex pyramids to describe demographics. If there is
time, we finish with a demonstration of R’s GIS/geospatial capabilities.

-   [Slides: GIS
    demonstration](https://appliedepi.github.io/intro_course/modules/module_6/slides/slides_gis_demo/slides_gis_demo.html)  

-   [R script: GIS
    demonstration](https://appliedepi.github.io/intro_course/modules/module_6/demos/demo_gis/demo_gis.R)

-   Exercise:

<img src="images/README_images/pointer_tutorial_adv_ggplot.png" width="50%" />

## Module 7: Routine reports with R Markdown

In this module, we take the R code on the Ebola case study that you have
been building throughout the course and convert it into a reproducible,
automated report (Word, PDF, HTML, etc.). We teach you the variations in
syntax and opportunities that lie in being able to produce documents
that update when incoming data is refreshed, that look professional, and
can be sent to inform public health partners and stakeholders.

-   Live demonstration [Instructor
    guide](https://appliedepi.github.io/intro_course/modules/module_7/slides/demo_rmd/demo_rmd_guide.html)

-   [Slides: R markdown and routine
    reports](https://appliedepi.github.io/intro_course/modules/module_7/slides/slides_rmd/slides_rmd.html)

-   Exercise:

<img src="images/README_images/pointer_tutorial_rmarkdown.png" width="50%" />

## Module 8: Final exercise and code review

In this last module, your skills are tested as you have to produce an R
Markdown report using a COVID-19 case linelist. Unlike with the Ebola
case study, you will not have the answer code available to you. When you
finish, we perform “code reviews”, simultaneously improving your coding
skills and teaching you how to review others’ code. Before closing, we
touch upon how to find your particular community of R users, resources
available to you for questions, and close with a feedback survey.

-   [Slides: COVID case
    study](https://appliedepi.github.io/intro_course/modules/module_8/slides/slides_covid_case_study/slides_covid_case_study.html)  
-   Exercise materials: See the folder
    “learning_materials/covid_case_study” for the Word document report
    to replicate, the data, and a tip sheet.  
-   [Slides: Code review]() TODO  
-   Code review guide TODO

## Sustained support

Our instructors *know* public health. One of the signature features of
Applied Epi’s training is that we provide follow-up support to your
team, to help you apply your new skills to your work context.

We schedule five 1.5-hour sessions with your team at in the 3 months
post-training. In these sessions, we help you troubleshoot code, advise
you on analytical strategies, or guide you in new learning that you
need.

## Notes

-   Please note that all of our case study training materials use *fake
    example data* in which no person is identifiable and the actual
    values have been scrambled.  
-   Modifications are possible so that the course uses data from *your*
    jurisdiction. Email us at <contact@appliedepi.org> us to discuss.

# Acknowledgements

Authors and contributors to this course curriculum from **Applied Epi**
include:

-   Neale Batra  

-   Arran Hamlet  

-   Mathilde Mousset  

-   Alex Spina  

-   Paula Blomquist  

-   Amy Mikhail

-   The Fulton County Board of Health graciously provided example data
    (anonymized and scrambled) for a case study.  

-   The {outbreaks} package formed the basis for the fake dataset in the
    Ebola case study.

# Terms of Use and License

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />This
work is licensed under a
<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative
Commons Attribution-NonCommercial-ShareAlike 4.0 International
License</a>.

Please email <contact@appliedepi.org> if you could like to use these
materials for an academic course or epidemiologist training program.

# Contribution

If you would like to make a content contribution, please contact with us
first via Github issues or by email. We are implementing a schedule for
updates and are creating a contributor guide.

Please note that the Epi R Handbook project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
