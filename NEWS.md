# AquaCache 2.1.1

## Summary

This version holds significant changes, all implemented by patch 3. The short summary is that there is a new workflow to deal with 'qualifiers' (ice, draw-down, etc) which were previously not differentiated from the 'grades'. In addition, storage of grades, approvals, qualifiers, owners, and contributors was moved out of the 'measurements_continuous' table and into separate tables to reduce redundancy and improve data integrity.

## Major changes (all implemented in the DB by patch 3)

-   Adjusted several 'downloadXXX' functions to report qualifiers, grades, approvals, owners, and contributors where previously they all reported only grades, approvals.
-   Created new tables to hold information about a timeseries' grades, approvals, qualifiers, owners, and contributors as they change through time with a single entry (row) until the attribute changes. These are linked to 'grade_types', 'approval_types', 'qualifier_types', 'owners_contributors' tables.
-   Created new family of functions, 'update_xxxx' which are used to update the 'grade_types', 'approval_types', 'qualifier_types', 'owners_contributors' tables and called from the 'getXXXX' functions.
-   Modified the 'getXXXX' and 'update_hydat' functions to use the new tables and functions.
-   Added an attribute to connections made with 'AquaConnect' to record if the connection is in a transaction or not. This is used to prevent starting a transaction within a transaction.
-   Modified many functions to make use of the new transaction attribute in the connection object. If a transaction is already in progress, the function will not start a new transaction and instead work within the existing transaction.
-   table measurements_calculated_daily now takes data from the measurements_continuous_corrected view table if any corrections need to be applied, ensuring that daily means represent the final trace. If no corrections apply it defaults to the table measurements_continuous to result in no speed penalty.

## Minor changes

-   Speed improvements throughout by replacing code such as if (object == TRUE) to if (object).
-   Improved the patch template for clarity and to include a 'rollback' function that can be used to undo the changes made by the patch.
-   Simplified connection to 'dev' database by adding parameter to AquaConnect
-   Improved how connections are passed (or not) to functions that require a connection to better ensure that all connections are properly closed.
-   Minor bug fix in calculate_stats affecting a minor subset of WSC locations.
-   Modifications to downloadAquarius to properly map grades, approvals, qualifiers to the WRB's version.
