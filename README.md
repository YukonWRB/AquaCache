---
editor_options: 
  markdown: 
    wrap: sentence
---

<!-- badges: start -->

[![R-CMD-check](https://github.com/YukonWRB/AquaCache/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/YukonWRB/AquaCache/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

# What's the AquaCache package about?

The AquaCache R package is a collection of functions that facilitate the creation of a postgres database (the AquaCache), loads data to this database, retrieves it, and performs checks on the data.
It integrates the Water Survey of Canada's realtime (5 minute) hydrometric data with published historical daily means, and facilitates the automated import of data from a variety of sources.

In addition, the package provides functions for the creation and administration of a snow survey database as well as a database intended for instrument maintenance/calibration records.
The later works with the Shiny application packaged with the WRBcalibrates package, created by the Yukon Department of Environment Water Resources Branch.

# Installation

The AquaCache package is not available on CRAN and likely never will be, but you can install it from GitHub with: `devtools::install_github("YukonWRB/AquaCache")`

Once the package is installed you attempt to initialize the AquaCache database on a running postgres server using the AquaCacheInit function, which creates tables, relationships, functions, triggers, and pre-populates some tables with standard information.
Note that we state "attempt to initialize" here: it's been a long while since anyone's created the database from scratch, so there's a really good chance that this function will not run cleanly because of some SQL syntax errors.

An alternative to initializing the database is to restore a backup of the AquaCache database.
This is the preferred method, as it will ensure that the database is in a known state.
The backup file is available by contacting the Water Resources Branch data scientist, Ghislain de Laplante, who can also help you out with the installation process in general.

# Collaboration and contributions

Are you part of an organization that needs to store and manage water or climate related data, documents, images, and instrument metadata in one location?
If so, you may find the AquaCache package useful and the alternatives lacking.
We designed this R package and associated database to meet the needs of the Yukon Department of Environment Water Resources Branch, but we believe that it could be useful to other organizations as well.

We welcome contributions to the package, and encourage you to contact the project creator and coordinator, Ghislain de Laplante if you wish to collaborate on the development of the package.
We are particularly interested in collaborating with organizations that have a need for a database like AquaCache, and that are willing to contribute to the development of the package.

## Contribution guidelines

Contributions to this package should be done via GitHub pull requests, and should address the following guidelines:

-   All code should be written in a clear and concise manner, with comments where necessary.

-   Avoid adding new dependencies to the package, unless absolutely necessary.
    If a new dependency is required, it should be discussed with the project coordinator before being added.

-   Package functions that are meant to be called repeatedly (getting data, comparing to remote stores, etc) should have a heavy emphasis on speed and efficiency.
    We've used the data.table package to great effect in this regard, for example.

-   Avoid using hard-coded values in the code, and instead use variables or functions to store and retrieve values.
    Credentials should be stored in a .Renviron file, while values such as parameter codes, source functions, etc. should be fetched from the database itself.

-   All functions should be documented in the usual R package manner and should include examples of how to use the function if possible (so far we're not doing so well on this one).

-   Any contributions should pass devtools::check() without any errors, warnings, and if possible notes.

-   ANY changes to the database schema should be accompanied by the necessary SQL code to create or modify the database.
    This is to ensure that the package and database remain in sync; see below for more information.

## R package changes

Changes to the R software that *do not* require a corresponding change to the database schema should be made in a branch and merged to *main* with a pull request.
Continuous integration is set up to ensure that each pull request passes devtools::check(), and approval from the repository owner/maintainer and (if specified) file owner(s) is required as well.

In general we ask that contributors follow this process:

1.  Check with other contributors/developers to see if your suggested modification is

    a) necessary;

    b) already being worked on;

    c) the best way to go

2.  Identify collaborators, if any.

3.  Make changes in a new branch (you can't push to *main*!)

4.  Thoroughly test your changes. We recommend doing this on a development version of your database that identically mirrors the production version's schema.

5.  Once everything is good, increment the version number (major/minor/patch is up to your judgement).

6.  Write down what you did in the NEWS.md file.

7.  If you want to enforce a review of the code you worked or function you added before future changes take place, add relevant entries to the .github/CODEOWNERS file (see GitHub help pages for how to do this).

8.  Make a pull request.

9.  Advise other package/database users that a new version is available.

For changes that require modifications to the database schema please read on.

###      Database schema changes

As this R package is designed to work directly with a postgres database, any additions or modifications to the package code that require modifications to the database schema should be accompanied by the necessary SQL code to create or modify the database.
This is to ensure that the package and database remain in sync.

To facilitate this synchronization we have a process to check for and apply patches to the database schema every time this package is updated.
Upon every connection to the database using function AquaConnect() the function AquaPatchCheck() is run; this function checks the database schema version and applies any necessary patches.
If the user does not have write privileges to the required tables then an error is raised and the user is instructed to contact the database administrator.

**If making changes to the database schema, the following steps MUST be taken in addition to the normal contribution process. Lots of things need to be checked and potentially adjusted, and it can be a complicated process.**

1.  UNDERSTAND the process; review the part of AquaConnect() where patch status is checked, look at existing patches, and review the patch_template.R file in the /inst folder. Talk to others if anything is unclear. ***Then*** you're ready to start.
2.  CHECK with other developers to ensure that the changes are necessary and that they will not conflict with other changes that are being made.
3.  COMMUNICATE the changes you plan to make to ensure that these are compatible with changes by others. Give others the opportunity to provide feedback on your changes.
4.  WRITE an R script that contains the necessary SQL code to modify the database. This script should be saved in the `inst/patches` directory of the package, and a template file exists in the same directory to help with this.
5.  CHECK and re-check that your patch works as intended and that ALL of the steps in the template script have been followed.
6.  ENSURE that your patch can be applied when called from function AquaPatchCheck().
7.  COMMUNICATE with other collaborators to ensure that they are aware of the changes you have made.
8.  ENSURE that other packages that depend on the database aren't adversely affected, and if they are, suggest or make changes to these packages for compatibility. At present this is limited to the *YGwater* and *WRBcalibrates* packages.
9.  Write down what you did in the NEWS.md file.
