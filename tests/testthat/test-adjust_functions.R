insert_test_org <- function(con, prefix) {
  org_name <- paste0(prefix, "_", as.integer(stats::runif(1, 1, 1e9)))
  inserted <- DBI::dbGetQuery(
    con,
    sprintf(
      "INSERT INTO public.organizations (name) VALUES ('%s') RETURNING organization_id;",
      org_name
    )
  )
  list(id = inserted$organization_id[1], name = org_name)
}

test_that("identical qualifier segments merge without affecting other types", {
  segments <- data.frame(
    qualifier_id = c(NA, 30L, 40L, NA),
    timeseries_id = rep(183L, 4),
    qualifier_type_id = c(18L, 18L, 1L, 1L),
    start_dt = as.POSIXct(
      c(
        "2024-06-07 07:00:00",
        "2023-09-13 16:00:00",
        "2024-10-19 11:15:00",
        "2025-05-08 08:00:00"
      ),
      tz = "UTC"
    ),
    end_dt = as.POSIXct(
      c(
        "2024-07-31 19:45:00",
        "2025-10-28 12:30:00",
        "2025-01-01 22:00:00",
        "2025-10-28 10:45:00"
      ),
      tz = "UTC"
    )
  )

  result <- merge_overlapping_same_value_segments(
    segments,
    value_col = "qualifier_type_id",
    id_col = "qualifier_id"
  )

  expect_equal(nrow(result$segments), 3)
  expect_equal(
    result$segments$qualifier_type_id,
    c(1L, 1L, 18L)
  )
  expect_equal(
    result$segments[result$segments$qualifier_type_id == 18L, "qualifier_id"],
    30L
  )
  expect_equal(
    result$segments[result$segments$qualifier_type_id == 18L, "start_dt"],
    as.POSIXct("2023-09-13 16:00:00", tz = "UTC")
  )
  expect_equal(
    result$segments[result$segments$qualifier_type_id == 18L, "end_dt"],
    as.POSIXct("2025-10-28 12:30:00", tz = "UTC")
  )
  expect_length(result$delete_ids, 0)
})

test_that("merging qualifier segments reports redundant existing IDs", {
  segments <- data.frame(
    qualifier_id = c(10L, 11L),
    timeseries_id = rep(183L, 2),
    qualifier_type_id = rep(18L, 2),
    start_dt = as.POSIXct(
      c("2024-01-01", "2024-02-01"),
      tz = "UTC"
    ),
    end_dt = as.POSIXct(
      c("2024-03-01", "2024-04-01"),
      tz = "UTC"
    )
  )

  result <- merge_overlapping_same_value_segments(
    segments,
    value_col = "qualifier_type_id",
    id_col = "qualifier_id"
  )

  expect_equal(nrow(result$segments), 1)
  expect_equal(result$segments$qualifier_id, 10L)
  expect_equal(result$delete_ids, 11L)
})

test_that("adjust_qualifier merges a qualifier repeated across rank streams", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  DBI::dbExecute(con, "BEGIN;")

  ts_id <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM continuous.timeseries ORDER BY timeseries_id LIMIT 1;"
  )[1, 1]
  qualifier_types <- DBI::dbGetQuery(
    con,
    "SELECT qualifier_type_id
       FROM public.qualifier_types
      ORDER BY qualifier_type_id
      LIMIT 3;"
  )$qualifier_type_id

  expect_length(qualifier_types, 3)
  qualifier_a <- qualifier_types[1]
  qualifier_b <- qualifier_types[2]
  qualifier_c <- qualifier_types[3]

  DBI::dbExecute(
    con,
    "INSERT INTO continuous.qualifiers
       (timeseries_id, qualifier_type_id, start_dt, end_dt)
     VALUES
       ($1, $2, '2099-01-01 00:00:00+00', '2099-01-10 00:00:00+00'),
       ($1, $3, '2099-01-01 00:00:00+00', '2099-01-10 00:00:00+00');",
    params = list(ts_id, qualifier_b, qualifier_c)
  )

  update_data <- data.frame(
    datetime = as.POSIXct(
      c(
        "2099-01-02 00:00:00",
        "2099-01-03 00:00:00",
        "2099-01-04 00:00:00",
        "2099-01-05 00:00:00"
      ),
      tz = "UTC"
    ),
    qualifier = c(
      qualifier_c,
      qualifier_b,
      paste(qualifier_a, qualifier_b, sep = ","),
      paste(qualifier_a, qualifier_b, sep = ",")
    )
  )

  expect_warning(
    adjust_qualifier(con, ts_id, update_data),
    NA
  )

  qualifier_b_out <- DBI::dbGetQuery(
    con,
    "SELECT start_dt, end_dt
       FROM continuous.qualifiers
      WHERE timeseries_id = $1
        AND qualifier_type_id = $2
        AND start_dt >= '2099-01-01 00:00:00+00'
        AND end_dt <= '2099-01-10 00:00:00+00'
      ORDER BY start_dt;",
    params = list(ts_id, qualifier_b)
  )

  expect_equal(nrow(qualifier_b_out), 1)
  expect_equal(
    qualifier_b_out$start_dt,
    as.POSIXct("2099-01-01 00:00:00", tz = "UTC")
  )
  expect_equal(
    qualifier_b_out$end_dt,
    as.POSIXct("2099-01-10 00:00:00", tz = "UTC")
  )
})

test_that("adjust_owner maps organization names and updates owner segments", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  DBI::dbExecute(con, "BEGIN;")

  ts_id <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM continuous.timeseries ORDER BY timeseries_id LIMIT 1;"
  )[1, 1]

  org_a <- insert_test_org(con, "owner_test_a")
  org_b <- insert_test_org(con, "owner_test_b")

  DBI::dbExecute(
    con,
    sprintf(
      paste0(
        "INSERT INTO continuous.owners (timeseries_id, organization_id, start_dt, end_dt) VALUES ",
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
        "FROM continuous.owners WHERE timeseries_id = %d ",
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
    "SELECT timeseries_id FROM continuous.timeseries ORDER BY timeseries_id LIMIT 1;"
  )[1, 1]

  org_a <- insert_test_org(con, "contrib_test_a")
  org_b <- insert_test_org(con, "contrib_test_b")

  DBI::dbExecute(
    con,
    sprintf(
      paste0(
        "INSERT INTO continuous.contributors (timeseries_id, organization_id, start_dt, end_dt) VALUES ",
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
        "FROM continuous.contributors WHERE timeseries_id = %d ",
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
    "SELECT timeseries_id FROM continuous.timeseries ORDER BY timeseries_id LIMIT 1;"
  )[1, 1]

  grade_types <- DBI::dbGetQuery(
    con,
    "SELECT grade_type_id FROM public.grade_types ORDER BY grade_type_id LIMIT 2;"
  )

  expect_gte(nrow(grade_types), 2)

  old_grade <- grade_types$grade_type_id[1]
  new_grade <- grade_types$grade_type_id[2]

  DBI::dbExecute(
    con,
    sprintf(
      paste0(
        "INSERT INTO continuous.grades (timeseries_id, grade_type_id, start_dt, end_dt) VALUES ",
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
        "FROM continuous.grades WHERE timeseries_id = %d ",
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
