<!-- badges: start -->

[![R-CMD-check](https://github.com/YukonWRB/AquaCache/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/YukonWRB/AquaCache/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

# What's the AquaCache package about?

The AquaCache R package is a collection of functions that facilitate the creation of a postgres database (the aquacache), loads data to this database, retrieves it, and performs checks on the data.
It integrates the Water Survey of Canada's realtime (5 minute) hydrometric data with published historical daily means, and facilitates the automated import of data from a variety of sources.

In addition, the package provides functions for the creation and administration of a snow survey database as well as a database intended for instrument maintenance/calibration records.
The later works with the Shiny application packaged with the WRBcalibrates package, created by the Yukon Department of Environment Water Resources Branch.

# Installation

The AquaCache package is not available on CRAN and likely never will be, but you can install it from GitHub with: `devtools::install_github("YukonWRB/AquaCache")`

Once the package is installed you attempt to initialize the aquacache database on a running postgres server using the AquaCacheInit function, which creates tables, relationships, functions, triggers, and pre-populates some tables with standard information.
Note that we state "attempt to initialize" here: it's been a long while since anyone's created the database from scratch, so there's a really good chance that this function will not run cleanly because of some SQL syntax errors.

An alternative to initializing the database is to restore a backup of the aquacache database.
This is the preferred method, as it will ensure that the database is in a known state.
The backup file is available by contacting the Water Resources Branch data scientist, Ghislain de Laplante, who can also help you out with the installation process in general.

# Collaboration and contributions

Are you part of an organization that needs to store and manage water or climate related data, documents, images, and instrument metadata in one location?
If so, you may find the AquaCache package useful and the alternatives lacking.
We designed this R package and associated database to meet the needs of the Yukon Department of Environment Water Resources Branch, but we believe that it could be useful to other organizations as well.

We welcome contributions to the package, and encourage you to contact the project creator and coordinator, Ghislain de Laplante if you wish to collaborate on the development of the package.
We are particularly interested in collaborating with organizations that have a need for a database like aquacache, and that are willing to contribute to the development of the package.

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

    a)  necessary;

    b)  already being worked on;

    c)  the best way to go

2.  Identify collaborators, if any.

3.  Make changes in a new branch (you can't push to *main*!)

4.  Thoroughly test your changes.
    We recommend doing this on a development version of your database that identically mirrors the production version's schema.

5.  Once everything is good, increment the version number (major/minor/patch is up to your judgement).

6.  Write down what you did in the NEWS.md file.

7.  If you want to enforce a review of the code you worked or function you added before future changes take place, add relevant entries to the .github/CODEOWNERS file (see GitHub help pages for how to do this).

8.  Make a pull request.

9.  Advise other package/database users that a new version is available.

For changes that require modifications to the database schema please read on.

### Database schema changes

As this R package is designed to work directly with a postgres database, any additions or modifications to the package code that require modifications to the database schema should be accompanied by the necessary SQL code to create or modify the database.
This is to ensure that the package and database remain in sync.

To facilitate this synchronization we have a process to check for and apply patches to the database schema every time this package is updated.
Upon every connection to the database using function AquaConnect() checks the database schema version and applies any necessary patches.
If the user does not have write privileges to the required tables then an error is raised and the user is instructed to contact the database administrator.

**If making changes to the database schema, the following steps MUST be taken in addition to the normal contribution process. Lots of things need to be checked and potentially adjusted, and it can be a complicated process.**

1.  UNDERSTAND the process; review the part of AquaConnect() where patch status is checked, look at existing patches, and review the patch_template.R file in the /inst folder. Talk to others if anything is unclear. ***Then*** you're ready to start.
2.  CHECK with other developers to ensure that the changes are necessary and that they will not conflict with other changes that are being made.
3.  COMMUNICATE the changes you plan to make to ensure that these are compatible with changes by others. Give others the opportunity to provide feedback on your changes.
4.  WRITE an R script that contains the necessary SQL code to modify the database. This script should be saved in the `inst/patches` directory of the package, and a template file exists in the same directory to help with this.
5.  CHECK and re-check that your patch works as intended and that ALL of the steps in the template script have been followed.
6.  ENSURE that your patch can be applied when called from function AquaConnect().
7.  COMMUNICATE with other collaborators to ensure that they are aware of the changes you have made.
8.  ENSURE that other packages that depend on the database aren't adversely affected, and if they are, suggest or make changes to these packages for compatibility. At present this is limited to the *YGwater* and *WRBcalibrates* packages.
9.  Write down what you did in the NEWS.md file.

# Package usage information

This functions in this package are mostly intended to be run automatically (i.e. not interactively) to bring in new data from remote data stores, to check for updates to data on remotes, and to calculate daily statistics.
The main functions can be thought of as being in four groups: 'get', 'daily', 'download', 'insert', 'add' functions.
Other functions are for the most part called by these functions to perform some discrete tasks.

## 'get' functions

Functions 'getNewContinuous' and 'getNewDiscrete' work on all timeseries present in the 'timeseries' table to bring in new data.
To do so, they call the 'download' function specified in the 'timeseries' table's 'source_fx' column, process the data if necessary, and append this new information to the database.
Ancilliary functions are also called that adjust records in the owners, contributors, grades, approvals, and qualifiers tables based on that provided by the 'download' functions.

'getNewImages', 'getNewRasters' work image and raster series specified in the 'images_index' and 'raster_series_index' tables.
In a similar process to other 'get' functions, new data is fetched with the 'download' function specified in the xxx_index tables, some pre-processing is done if necessary, and insertion to the database is made.
In contrast however, the insertion of rasters and images to the database is then done using either functions 'insertACImage' or 'insertACModelRaster' as this functionality can also be used to insert data outside of the 'get' functions.

## 'daily' function

There is only one function in this group, dailyUpdate.
This function first checks if the Water Survey of Canada has published a new version of the HYDAT database and incorporates that if necessary, then calls the 'get' functions with cascading effects.
For each new continuous-type timeseries, new daily means and statistics are calculated for the table 'measurements_calculated_daily'.
In the event of new hydat daily means, recalculation takes place from the time of first discrepancy between HYDAT means and existing means in the database.

## 'download' functions

All 'downloadXXXX' functions fetch data from remote stores based on their parameters.
They are primarily designed to be called form the 'get' functions but can also be used independently; we advise against bypassing the 'get' functions for database entry, but many use cases exist for using these for fetching data alone.
In particular, the 'downloadWSC' function is dramatically faster than the data fetch functions in the 'tidyhydat' package, at the expense of decreased convenience.

## 'insert' functions

These functions create new records in the database for slightly esoteric formats, such as rasters, vectors, images, and documents.
Adding these to a postgres database is not as simple as adding new rows or appending a data table; conversion to formatsnative to postgres or postGIS is necessary first.

## 'add' functions

These functions are designed to simplify the process of adding new locations or timeseries to the database.
Adding a timeseries is not a straightforward affair and requires creation and modification of records in the 'location', 'timeseries', and other associated tables.
For the Water Survey of Canada it is also necessary to merge data from the HYDAT daily means database with up-to-date 5-minute data points.

In all cases these functions ensure that all relevant entries are populated, that database conflicts will not arise, and that future data fetching operations can be performed.
