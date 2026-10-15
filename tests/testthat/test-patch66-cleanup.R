patch66_environment <- new.env(parent=asNamespace("AquaCache"))
patch66_directory <- system.file("patches","patch_66",package="AquaCache")
sys.source(file.path(patch66_directory,"cleanup.R"),envir=patch66_environment)

test_that("Patch 66 units and manifest preserve scientific distinctions", {
  unit <- patch66_environment$patch66_unit_definition
  expect_equal(unit(" MG/L ")$scale,1)
  expect_equal(unit("\u00b5g/L")$scale,.001)
  expect_equal(unit("\u03bcg/L")$scale,.001)
  expect_equal(unit("ng/L")$scale,1e-6)
  expect_equal(unit("%",solid=TRUE)$scale,10000)
  expect_identical(unit("CU"),unit("TCU"))
  expect_identical(unit("mbTOC"),unit("m"))
  expect_error(unit("%"),"Unreviewed")
  expect_error(unit("MPN/100mL"),"Unreviewed")
  expect_error(unit(NA_character_),"Unreviewed")
  plan <- data.table::fread(file.path(patch66_directory,"parameters.tsv"),sep="\t",quote="",na.strings="")
  expect_false(anyDuplicated(tolower(trimws(plan$source)))>0)
  expect_true(all(vapply(plan$conditions[!is.na(plan$conditions)],jsonlite::validate,logical(1))))
  expect_true(all(plan[grepl("surrogate recovery",target),mode]=="rename"))
  expect_equal(plan[source=="Acidity to pH 4.5",endpoint_ph],4.5)
  expect_equal(plan[source=="Acidity to pH 8.3",endpoint_ph],8.3)
  expect_equal(plan[source=="Se-IV",speciation_code],"selenium_iv")
  expect_equal(plan[source=="Se-VI",speciation_code],"selenium_vi")
  expect_true(all(plan[grepl("13C12-labelled",target),mode]=="rename"))
})

# Explicit testdb opt-in; replace the actual patch's final COMMIT, then roll
# back all fixtures and migrations. Identity sequences can still advance.
test_that("Patch 66 migrates testdb and preserves historical mapping identities", {
  skip_if(Sys.getenv("AQUACACHE_PATCH66_DATABASE")!="testdb","testdb integration opt-in required")
  con <- DBI::dbConnect(RPostgres::Postgres(),dbname="testdb",host="10.250.12.154",
    port=Sys.getenv("aquacachePort","5432"),user=Sys.getenv("aquacacheAdminUser"),
    password=Sys.getenv("aquacacheAdminPass"),connect_timeout=5,
    options="-c statement_timeout=120000 -c lock_timeout=5000")
  on.exit(DBI::dbDisconnect(con))
  on.exit(try(DBI::dbExecute(con,"ROLLBACK"),silent=TRUE),add=TRUE,after=FALSE)
  identity <- DBI::dbGetQuery(con,
    "SELECT current_database() AS db,inet_server_addr()::TEXT AS host,
     (SELECT version FROM information.version_info WHERE item='Last patch number') AS patch")
  stopifnot(identity$db=="testdb",identity$host=="10.250.12.154/32",identity$patch=="65")
  tables <- c("public.parameters","discrete.results","discrete.import_parameter_mappings",
    "discrete.import_mapping_sets","discrete.result_speciations","criteria.guidelines",
    "criteria.guideline_rule_inputs","information.version_info")
  snapshot <- function() stats::setNames(lapply(tables,function(table)
    DBI::dbGetQuery(con,paste("SELECT * FROM",table,"ORDER BY 1"))),tables)
  before <- snapshot()
  triggers <- function() DBI::dbGetQuery(con,
    "SELECT tgrelid,tgname,tgenabled::TEXT FROM pg_trigger WHERE NOT tgisinternal ORDER BY tgrelid,tgname")
  triggers_before <- triggers()
  fixture <- new.env(parent=emptyenv())
  patch66_fixture <- function() {
    # Keep relation locks across the verification savepoint. Concurrent UI
    # sessions may edit testdb; compare rollback state before releasing locks.
    for (table in c("public.parameters",sort(setdiff(tables,"public.parameters")))) {
      DBI::dbExecute(con,paste("LOCK TABLE",table,"IN SHARE ROW EXCLUSIVE MODE"))
    }
    before <<- snapshot()
    triggers_before <<- triggers()
    DBI::dbExecute(con,"SAVEPOINT patch66_verification")
    p <- DBI::dbGetQuery(con,"SELECT parameter_id,lower(param_name) AS name FROM public.parameters")
    id <- stats::setNames(p$parameter_id,p$name)
    sample <- DBI::dbGetQuery(con,"SELECT min(sample_id) AS id FROM discrete.samples")$id
    liquid <- DBI::dbGetQuery(con,"SELECT matrix_state_id AS id FROM public.matrix_states WHERE matrix_state_code='liquid'")$id
    stopifnot(length(sample)==1L,!is.na(sample))
    fixture$results <- vapply(c("acidity to ph 4.5","acidity to ph 8.3"),function(name)
      DBI::dbGetQuery(con,
        "INSERT INTO discrete.results(sample_id,parameter_id,matrix_state_id,result_type,result)
         VALUES ($1,$2,$3,2,12.5) RETURNING result_id",
        params=list(sample,id[[name]],liquid))$result_id,integer(1))
    source <- DBI::dbGetQuery(con,
      "INSERT INTO discrete.import_sources(source_code,source_name) VALUES ($1,$1) RETURNING import_source_id",
      params=list(paste0("PATCH66_FIXTURE_",Sys.getpid())))$import_source_id
    set <- DBI::dbGetQuery(con,
      "INSERT INTO discrete.import_mapping_sets(import_source_id,version,status)
       VALUES ($1,1,'draft') RETURNING import_mapping_set_id",params=list(source))$import_mapping_set_id
    fixture$mappings <- vapply(seq_len(2L),function(i) DBI::dbGetQuery(con,
      "INSERT INTO discrete.import_parameter_mappings
       (import_mapping_set_id,source_match,parameter_id,result_type,result_value_type,matrix_state_id,conversion)
       VALUES ($1,$2::JSONB,$3,2,1,$4,$5) RETURNING import_mapping_id",
      params=list(set,jsonlite::toJSON(list(input_param=paste0("bj_fixture_",i),input_unit="ng/L"),auto_unbox=TRUE),
        id[["benzo{b&j}fluoranthene"]],liquid,c(1,1e-6)[[i]]))$import_mapping_id,integer(1))
    DBI::dbExecute(con,
      "UPDATE discrete.import_mapping_sets SET status='published',published_at=CURRENT_TIMESTAMP WHERE import_mapping_set_id=$1",
      params=list(set))
    fixture$mapping_rows <- DBI::dbGetQuery(con,
      "SELECT * FROM discrete.import_parameter_mappings WHERE import_mapping_id IN ($1,$2) ORDER BY import_mapping_id",
      params=unname(as.list(fixture$mappings)))
  }
  lines <- readLines(system.file("patches","DEV_patch_66.R",package="AquaCache"),warn=FALSE)
  commit <- grep('DBI::dbExecute(con, "COMMIT")',lines,fixed=TRUE)
  begin <- grep('active <- AquaCache:::dbTransBegin(con)',lines,fixed=TRUE)
  stopifnot(length(commit)==1L,length(begin)==1L)
  lines[commit] <- 'DBI::dbExecute(con,"SET CONSTRAINTS ALL IMMEDIATE")'
  lines <- append(lines,"patch66_fixture()",after=begin)
  env <- new.env(parent=environment());env$con <- con
  capture.output(eval(parse(text=lines),envir=env))
  expect_true(AquaCache:::dbTransCheck(con))
  expect_true(any(env$patch66_report$action=="source absent; skipped"))
  expect_equal(nrow(DBI::dbGetQuery(con,"SELECT * FROM public.parameters")),
    nrow(before[["public.parameters"]])-sum(env$patch66_report$action=="merged"))
  reviewed <- unique(data.table::fread(file.path(patch66_directory,"catalogue_review.tsv"),
    sep="\t",quote="",strip.white=FALSE,encoding="UTF-8")[,.(parameter_id=canonical_parameter_id,param_name=canonical_name)])
  current <- DBI::dbGetQuery(con,"SELECT parameter_id,param_name FROM public.parameters")
  expect_equal(current$param_name[match(reviewed$parameter_id,current$parameter_id)],reviewed$param_name)
  normalized <- gsub("[^a-z0-9]","",tolower(current$param_name))
  expect_false(anyDuplicated(normalized)>0L)
  r <- DBI::dbGetQuery(con,
    "SELECT r.*,p.param_name FROM discrete.results r JOIN public.parameters p USING(parameter_id)
     WHERE r.result_id IN ($1,$2) ORDER BY endpoint_ph",params=unname(as.list(fixture$results)))
  expect_equal(r$param_name,c("acidity","acidity"))
  expect_equal(r$endpoint_mode,c("fixed","fixed"))
  expect_equal(r$endpoint_ph,c(4.5,8.3))
  expect_equal(r$result,c(12.5,12.5))
  expect_true(all(is.na(r$reporting_basis_id)))
  m <- DBI::dbGetQuery(con,
    "SELECT * FROM discrete.import_parameter_mappings WHERE import_mapping_id IN ($1,$2) ORDER BY import_mapping_id",
    params=unname(as.list(fixture$mappings)))
  retained <- c("import_mapping_id","import_mapping_set_id","source_match","result_offset","active")
  expect_identical(m[retained],fixture$mapping_rows[retained])
  expect_equal(m$conversion,c(1e-6,1e-6))
  expect_equal(length(unique(m$parameter_id)),1L)
  g <- DBI::dbGetQuery(con,
    "SELECT g.guideline_id,p.param_name,s.result_speciation FROM criteria.guidelines g
     JOIN public.parameters p USING(parameter_id) LEFT JOIN discrete.result_speciations s USING(result_speciation_id)")
  original <- before[["public.parameters"]]
  for (form in c("iii","vi")) {
    old_id <- original$parameter_id[tolower(original$param_name)==paste0("chromium(",form,")")]
    guideline <- before[["criteria.guidelines"]]$guideline_id[before[["criteria.guidelines"]]$parameter_id==old_id]
    expect_true(all(g$param_name[g$guideline_id %in% guideline]=="chromium"))
    expect_true(all(g$result_speciation[g$guideline_id %in% guideline]==paste0("Chromium(",toupper(form),")")))
  }
  expect_equal(nrow(g),nrow(before[["criteria.guidelines"]]))
  expect_identical(triggers(),triggers_before)
  DBI::dbExecute(con,"ROLLBACK TO SAVEPOINT patch66_verification")
  restored <- snapshot()
  expect_identical(restored,before)
  codes <- data.table::fread(file.path(patch66_directory,"parameters.tsv"),sep="\t",quote="",na.strings="")$speciation_code
  introduced <- setdiff(codes[!is.na(codes)],before[["discrete.result_speciations"]]$result_speciation_code)
  expect_false(any(restored[["discrete.result_speciations"]]$result_speciation_code %in% introduced))
  expect_false(any(restored[["discrete.results"]]$result_id %in% fixture$results))
  expect_false(any(restored[["discrete.import_parameter_mappings"]]$import_mapping_id %in% fixture$mappings))
  expect_identical(triggers(),triggers_before)
})
