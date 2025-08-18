# Create a test database and connection for testing purposes

create_test_con <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE organizations (organization_id INTEGER PRIMARY KEY, name TEXT)")
  DBI::dbExecute(con, "INSERT INTO organizations VALUES (1, 'Environment and Climate Change Canada')")
  DBI::dbExecute(con, "INSERT INTO organizations VALUES (2, 'Water Survey of Canada')")
  DBI::dbExecute(con, "CREATE TABLE grade_types (grade_type_id INTEGER, grade_type_code TEXT)")
  DBI::dbExecute(con, "INSERT INTO grade_types VALUES (1, 'UNS')")
  DBI::dbExecute(con, "CREATE TABLE approval_types (approval_type_id INTEGER, approval_type_code TEXT)")
  DBI::dbExecute(con, "INSERT INTO approval_types VALUES (1, 'UNS')")
  DBI::dbExecute(con, "CREATE TABLE qualifier_types (qualifier_type_id INTEGER, qualifier_type_code TEXT)")
  DBI::dbExecute(con, "INSERT INTO qualifier_types VALUES (1, 'UNS')")
  con
}
