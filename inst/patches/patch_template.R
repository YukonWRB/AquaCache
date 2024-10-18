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

# !IMPORTANT! Do this within a BEGIN, COMMIT, ROLLBACK transaction block as such:

# Begin a transaction
# DBI::dbExecute(con, "BEGIN;")
# attr(con, "active_transaction") <- TRUE
# tryCatch({
#   # Do the things
#   #
#   #
#   # Commit the transaction
#   DBI::dbExecute(con, "COMMIT;")
#   attr(con, "active_transaction") <- FALSE
# }, error = function(e) {
#   DBI::dbExecute(con, "ROLLBACK;")
#   attr(con, "active_transaction") <<- FALSE
#   stop("Patch 3 failed and the DB has been rolled back to its earlier state. ", e$message)
# })

# Note the use of the 'active_transaction' attribute on the connection; this is important as it ensures that the patch is applied atomically. Normally, other functions which may be called from within the patch use transactions, but transactions within transactions don't work in PostgreSQL. By using this attribute, the functions run without their own transactions, but the patch as a whole is atomic.


# Step 4: If not successful, the patch should stop execution, roll back the entire transaction, and give an error message which will get caught by AquaPatchCheck().

# Example:
# stop("Patch 1 failed: could not create new schema 'information' and table 'version_info'.")


# Step 5: Give the user a message indicating the patch was successful and INCREMENT THE PATCH NUMBER IN THE DATABASE.

      # Example:
      # DBI::dbExecute(con, "UPDATE information.version_info SET version = '2' WHERE item = 'Last patch number';")
      # message("Patch 1 applied successfully: created new schema 'information' and table 'version_info'.")



