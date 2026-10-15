# Rollback-only PostgreSQL checks for cross-instance cases and failure paths.
test_that("Patch 66 handles absent targets and rejects destructive reinterpretation", {
  skip_if(Sys.getenv("AQUACACHE_PATCH66_DATABASE")!="testdb","testdb integration opt-in required")
  con <- DBI::dbConnect(RPostgres::Postgres(),dbname="testdb",host="10.250.12.154",
    port=Sys.getenv("aquacachePort","5432"),user=Sys.getenv("aquacacheAdminUser"),
    password=Sys.getenv("aquacacheAdminPass"),connect_timeout=5,
    options="-c statement_timeout=120000 -c lock_timeout=5000")
  active <- FALSE
  on.exit(DBI::dbDisconnect(con))
  on.exit(if(active) try(DBI::dbExecute(con,"ROLLBACK"),silent=TRUE),add=TRUE,after=FALSE)
  identity <- DBI::dbGetQuery(con,
    "SELECT current_database() AS db,inet_server_addr()::TEXT AS host,
     (SELECT version FROM information.version_info WHERE item='Last patch number') AS patch")
  stopifnot(identity$db=="testdb",identity$host=="10.250.12.154/32",identity$patch=="65")
  directory <- system.file("patches","patch_66",package="AquaCache")
  env <- new.env(parent=asNamespace("AquaCache"))
  sys.source(file.path(directory,"cleanup.R"),envir=env)
  manifest <- data.table::fread(file.path(directory,"parameters.tsv"),sep="\t",quote="",na.strings="")
  plan_file <- tempfile(fileext=".tsv")
  on.exit(unlink(plan_file),add=TRUE)
  p <- DBI::dbGetQuery(con,"SELECT parameter_id,lower(param_name) AS name FROM public.parameters")
  id <- stats::setNames(p$parameter_id,p$name)
  sample <- DBI::dbGetQuery(con,"SELECT min(sample_id) AS id FROM discrete.samples")$id
  liquid <- DBI::dbGetQuery(con,"SELECT matrix_state_id AS id FROM public.matrix_states WHERE matrix_state_code='liquid'")$id
  mg <- DBI::dbGetQuery(con,"SELECT min(unit_id) AS id FROM public.units WHERE lower(unit_name)='mg/l'")$id
  stopifnot(!is.na(sample))
  trigger_state <- DBI::dbGetQuery(con,
    "SELECT tgrelid,tgname,tgenabled::TEXT FROM pg_trigger WHERE NOT tgisinternal ORDER BY tgrelid,tgname")
  before <- DBI::dbGetQuery(con,"SELECT * FROM public.parameters ORDER BY parameter_id")

  # No source present: do nothing, including no chemistry catalogue insertion.
  active <- AquaCache:::dbTransBegin(con)
  row <- data.table::copy(manifest[1])
  row$source <- paste0("Patch66 absent source ",Sys.getpid())
  row$target <- paste0("Patch66 absent target ",Sys.getpid())
  data.table::fwrite(row,plan_file,sep="\t",quote=FALSE,na="")
  result <- env$patch66_parameter_cleanup(con,plan_file)
  expect_equal(result$action,"source absent; skipped")
  DBI::dbExecute(con,"ROLLBACK");active <- FALSE

  # A source-only instance gets a rename, without creating an unrelated target
  # row or requiring any mapping source/profile fixtures.
  active <- AquaCache:::dbTransBegin(con)
  source <- DBI::dbGetQuery(con,
    "INSERT INTO public.parameters(param_name,sample_fraction,plot_default_y_orientation,units_liquid)
     VALUES ($1,FALSE,'normal',$2) RETURNING parameter_id",params=list(row$source,mg))$parameter_id
  result <- env$patch66_parameter_cleanup(con,plan_file)
  expect_equal(result$action,"renamed")
  expect_equal(DBI::dbGetQuery(con,"SELECT param_name FROM public.parameters WHERE parameter_id=$1",
    params=list(source))$param_name,row$target)
  DBI::dbExecute(con,"ROLLBACK");active <- FALSE

  # Numbers under an ng/L source cannot simply acquire an mg/L unit label.
  active <- AquaCache:::dbTransBegin(con)
  data.table::fwrite(manifest[1],plan_file,sep="\t",quote=FALSE,na="")
  DBI::dbExecute(con,
    "INSERT INTO discrete.results(sample_id,parameter_id,matrix_state_id,result_type,result)
     VALUES ($1,$2,$3,2,123)",params=list(sample,id[["benzo{b&j}fluoranthene"]],liquid))
  expect_error(env$patch66_parameter_cleanup(con,plan_file),"Separate numeric migration required")
  DBI::dbExecute(con,"ROLLBACK");active <- FALSE

  # Same final result identity with different values must abort, never dedupe.
  active <- AquaCache:::dbTransBegin(con)
  data.table::fwrite(manifest[source=="benzo[b,j,k]fluoranthene"],plan_file,sep="\t",quote=FALSE,na="")
  for (name in c("benzo[b,j,k]fluoranthene",
    "benzo[b]fluoranthene, mixed with benzo[j]fluoranthene and benzo[k]fluoranthene")) {
    DBI::dbExecute(con,
      "INSERT INTO discrete.results(sample_id,parameter_id,matrix_state_id,result_type,result)
       VALUES ($1,$2,$3,2,$4)",params=list(sample,id[[name]],liquid,match(name,names(id))))
  }
  # RPostgres can warn while closing the deliberately failed query before the
  # shared migrator rolls back its savepoint. Other warnings remain visible.
  withCallingHandlers(
    expect_error(env$patch66_parameter_cleanup(con,plan_file),"duplicate key"),
    warning=function(w) {
      if (startsWith(conditionMessage(w),"Closing open result set")) invokeRestart("muffleWarning")
    })
  DBI::dbExecute(con,"ROLLBACK");active <- FALSE
  expect_identical(DBI::dbGetQuery(con,"SELECT * FROM public.parameters ORDER BY parameter_id"),before)
  expect_identical(DBI::dbGetQuery(con,
    "SELECT tgrelid,tgname,tgenabled::TEXT FROM pg_trigger WHERE NOT tgisinternal ORDER BY tgrelid,tgname"),trigger_state)
})
