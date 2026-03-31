insert_test_org <- function(con, prefix) {
  org_name <- paste0(prefix, "_", as.integer(stats::runif(1, 1, 1e9)))
  inserted <- DBI::dbGetQuery(
    con,
    sprintf(
      "INSERT INTO organizations (name) VALUES ('%s') RETURNING organization_id;",
      org_name
    )
  )
  list(id = inserted$organization_id[1], name = org_name)
}

test_that("adjust_owner maps organization names and updates owner segments", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  DBI::dbExecute(con, "BEGIN;")

  ts_id <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM timeseries ORDER BY timeseries_id LIMIT 1;"
  )[1, 1]

  org_a <- insert_test_org(con, "owner_test_a")
  org_b <- insert_test_org(con, "owner_test_b")

  DBI::dbExecute(
    con,
    sprintf(
      paste0(
        "INSERT INTO owners (timeseries_id, organization_id, start_dt, end_dt) VALUES ",
        "(%d, %d, '2099-01-01 00:00:00+00', '2099-01-04 00:00:00+00'),",
        "(%d, %d, '2099-01-04 00:00:00+00', '2099-01-10 00:00:00+00');"
      ),
      ts_id,
      org_a$id,
      ts_id,
      org_b$id
    )
  )

  update_data <- data.frame(
    datetime = as.POSIXct(
      c(
        "2099-01-01 00:00:00",
        "2099-01-02 00:00:00",
        "2099-01-03 00:00:00",
        "2099-01-04 00:00:00"
      ),
      tz = "UTC"
    ),
    owner = c(org_a$name, org_a$name, org_b$name, org_b$name),
    stringsAsFactors = FALSE
  )

  adjust_owner(con, ts_id, update_data)

  owners_out <- DBI::dbGetQuery(
    con,
    sprintf(
      paste0(
        "SELECT organization_id, start_dt::text AS start_dt, end_dt::text AS end_dt ",
        "FROM owners WHERE timeseries_id = %d ",
        "AND start_dt >= '2099-01-01 00:00:00+00' ",
        "AND end_dt <= '2099-01-10 00:00:00+00' ORDER BY start_dt;"
      ),
      ts_id
    )
  )

  expect_equal(nrow(owners_out), 2)
  expect_equal(owners_out$organization_id, c(org_a$id, org_b$id))
  expect_equal(
    owners_out$start_dt,
    c("2099-01-01 00:00:00+00", "2099-01-03 00:00:00+00")
  )
  expect_equal(
    owners_out$end_dt,
    c("2099-01-03 00:00:00+00", "2099-01-10 00:00:00+00")
  )
})

test_that("adjust_contributor accepts date column and delete removes later segments", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  DBI::dbExecute(con, "BEGIN;")

  ts_id <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM timeseries ORDER BY timeseries_id LIMIT 1;"
  )[1, 1]

  org_a <- insert_test_org(con, "contrib_test_a")
  org_b <- insert_test_org(con, "contrib_test_b")

  DBI::dbExecute(
    con,
    sprintf(
      paste0(
        "INSERT INTO contributors (timeseries_id, organization_id, start_dt, end_dt) VALUES ",
        "(%d, %d, '2099-02-01 00:00:00+00', '2099-02-03 00:00:00+00'),",
        "(%d, %d, '2099-02-03 00:00:00+00', '2099-02-10 00:00:00+00');"
      ),
      ts_id,
      org_a$id,
      ts_id,
      org_b$id
    )
  )

  update_data <- data.frame(
    date = as.Date(c("2099-02-02", "2099-02-03", "2099-02-04")),
    contributor = c(org_a$name, org_a$name, org_a$name),
    stringsAsFactors = FALSE
  )

  adjust_contributor(con, ts_id, update_data, delete = TRUE)

  contributors_out <- DBI::dbGetQuery(
    con,
    sprintf(
      paste0(
        "SELECT organization_id, start_dt::text AS start_dt, end_dt::text AS end_dt ",
        "FROM contributors WHERE timeseries_id = %d ",
        "AND start_dt >= '2099-02-01 00:00:00+00' ",
        "AND end_dt <= '2099-02-10 00:00:00+00' ORDER BY start_dt;"
      ),
      ts_id
    )
  )

  expect_equal(nrow(contributors_out), 2)
  expect_equal(contributors_out$organization_id, c(org_a$id, org_b$id))
  expect_equal(
    contributors_out$start_dt,
    c("2099-02-01 00:00:00+00", "2099-02-04 00:00:00+00")
  )
  expect_equal(
    contributors_out$end_dt,
    c("2099-02-04 00:00:00+00", "2099-02-10 00:00:00+00")
  )
})

test_that("adjust_grade splits an existing contiguous period when inserting a mid-period grade", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  DBI::dbExecute(con, "BEGIN;")

  ts_id <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM timeseries ORDER BY timeseries_id LIMIT 1;"
  )[1, 1]

  grade_types <- DBI::dbGetQuery(
    con,
    "SELECT grade_type_id FROM grade_types ORDER BY grade_type_id LIMIT 2;"
  )

  expect_gte(nrow(grade_types), 2)

  old_grade <- grade_types$grade_type_id[1]
  new_grade <- grade_types$grade_type_id[2]

  DBI::dbExecute(
    con,
    sprintf(
      paste0(
        "INSERT INTO grades (timeseries_id, grade_type_id, start_dt, end_dt) VALUES ",
        "(%d, %d, '2099-01-01 00:00:00+00', '2099-04-01 00:00:00+00');"
      ),
      ts_id,
      old_grade
    )
  )

  update_data <- data.frame(
    datetime = as.POSIXct(
      c("2099-02-01 00:00:00", "2099-02-15 00:00:00", "2099-03-01 00:00:00"),
      tz = "UTC"
    ),
    grade = c(new_grade, new_grade, new_grade)
  )

  adjust_grade(con, ts_id, update_data)

  grades_out <- DBI::dbGetQuery(
    con,
    sprintf(
      paste0(
        "SELECT grade_type_id, start_dt::text AS start_dt, end_dt::text AS end_dt ",
        "FROM grades WHERE timeseries_id = %d ",
        "AND start_dt >= '2099-01-01 00:00:00+00' ",
        "AND end_dt <= '2099-04-01 00:00:00+00' ORDER BY start_dt;"
      ),
      ts_id
    )
  )

  expect_equal(nrow(grades_out), 3)
  expect_equal(grades_out$grade_type_id, c(old_grade, new_grade, old_grade))
  expect_equal(
    grades_out$start_dt,
    c(
      "2099-01-01 00:00:00+00",
      "2099-02-01 00:00:00+00",
      "2099-03-01 00:00:00+00"
    )
  )
  expect_equal(
    grades_out$end_dt,
    c(
      "2099-02-01 00:00:00+00",
      "2099-03-01 00:00:00+00",
      "2099-04-01 00:00:00+00"
    )
  )
})
