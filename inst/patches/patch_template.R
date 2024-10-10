# TEMPLATE FOR PATCHES

# Reminder: patches are applied sequentially by AquaPatchCheck(). The patch number is stored in the database and is used to determine which patches have already been applied. The patch number should be incremented in the database after a patch is successfully applied. 

# Step 1: Ensure user has necessary privileges. This usually includes write or update privileges to tables but could also be the ability to create a new schema.

    # Example: 
    # check <- DBI::dbGetQuery(con, "SELECT has_database_privilege(current_user, 'CREATE') AS can_create")
    # if (!check$can_create) {
    # stop("You do not have the necessary privileges to create a new schema in this database.")
    # }


# Step 2: If possible do some basic checks. Has the patch already been applied and the version number not incremented? **Can** the changes be made give the structure that exists? 


# Step 3: Make the necessary changes. This could include simple operations like creating new columns or tables, or more complex operations like creating new functions and triggers, changing how data is stored, etc. This could involve operations directly in the DB, calculations or other done in R, or a combination of these. Make sure to consider any system limitations, especially memory and processing time.


# Step 4: If not successful, the patch should stop execution and give an error message which will get caught by AquaPatchCheck().

# Example:
# stop("Patch 1 failed: could not create new schema 'information' and table 'version_info'.")


# Step 5: Give the user a message indicating the patch was successful and INCREMENT THE PATCH NUMBER IN THE DATABASE.

      # Example:
      # DBI::dbExecute(con, "UPDATE information.version_info SET version = '2' WHERE item = 'Last patch number';")
      # message("Patch 1 applied successfully: created new schema 'information' and table 'version_info'.")



