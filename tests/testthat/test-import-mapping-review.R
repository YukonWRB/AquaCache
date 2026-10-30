test_that("mapping review rejects invalid filters before querying", {
  expect_error(getImportMappingReview(NULL, include_history = NA), "include_history")
  expect_error(getImportMappingReview(NULL, include_history = c(TRUE, FALSE)), "include_history")
  expect_error(getImportMappingReview(NULL, source_code = " "), "source_code")
  expect_error(getImportMappingReview(NULL, source_code = c("EQWin", "ALS")), "source_code")
})

test_that("mapping review covers source-wide, workbook, draft and historical mappings", {
  skip_if(Sys.getenv("AQUACACHE_MAPPING_REVIEW_DATABASE") != "aquacache", "dev integration opt-in required")
  con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "aquacache", host = "10.250.12.154",
    port = Sys.getenv("aquacachePort", "5432"), user = Sys.getenv("aquacacheAdminUser"),
    password = Sys.getenv("aquacacheAdminPass"), connect_timeout = 5,
    options = "-c lock_timeout=5000 -c statement_timeout=15000")
  on.exit(DBI::dbDisconnect(con))
  identity <- DBI::dbGetQuery(con,"SELECT current_database() AS db,inet_server_addr()::TEXT AS host,
    (SELECT version FROM information.version_info WHERE item='Last patch number') AS patch")
  stopifnot(identity$db == "aquacache", identity$host == "10.250.12.154/32", identity$patch == "65")
  DBI::dbExecute(con,"BEGIN")
  on.exit(DBI::dbExecute(con,"ROLLBACK"), add = TRUE, after = FALSE)
  code <- paste0("zz_mapping_review_",Sys.getpid())
  stopifnot(DBI::dbGetQuery(con,"SELECT count(*)::INTEGER AS n FROM discrete.import_sources WHERE source_code=$1",params=list(code))$n == 0L)
  parameter <- DBI::dbGetQuery(con,"SELECT parameter_id FROM public.parameters
    WHERE NOT sample_fraction AND NOT reporting_basis AND NOT result_speciation AND NOT result_target AND NOT endpoint
      AND cardinality(analysis_conditions_required_keys)=0 AND analysis_conditions_required_keys_by_matrix='{}'::JSONB
      AND units_liquid IS NOT NULL ORDER BY parameter_id LIMIT 1")$parameter_id
  liquid <- DBI::dbGetQuery(con,"SELECT matrix_state_id FROM public.matrix_states WHERE matrix_state_code='liquid'")$matrix_state_id
  lab <- DBI::dbGetQuery(con,"SELECT result_type_id FROM discrete.result_types WHERE result_type='lab'")$result_type_id
  actual <- DBI::dbGetQuery(con,"SELECT result_value_type_id FROM discrete.result_value_types WHERE result_value_type='Actual'")$result_value_type_id
  location <- DBI::dbGetQuery(con,"SELECT location_id FROM public.locations ORDER BY location_id LIMIT 1")$location_id
  stopifnot(all(lengths(list(parameter,liquid,lab,actual,location))==1L))
  mapping <- data.frame(ParamCode="KNOWN",input_unit="mg/L",parameter_id=parameter,
    result_type=lab,matrix_state_id=liquid,result_value_type=actual)
  upsertImportParameterMappings(con,code,mapping,c("ParamCode","input_unit"),
    source_description="Retain this source description.",publish=TRUE)
  upsertImportProfile(con,code,"workbook","Review workbook",file_type="xlsx",
    datetime_origin="text",column_map=list(parameter_code="Analyte",unit="Unit",result="Value"))
  profiles <- getImportProfiles(con,active=NULL)
  expect_true(any(profiles$source_code==code & profiles$profile_code=="workbook"))
  upsertImportParameterMappings(con,code,transform(mapping,ParamCode="PROFILE"),c("ParamCode","input_unit"),profile_code="workbook",publish=TRUE)
  upsertImportLocationMappings(con,code,data.frame(source_location_code="SITE",location_id=location),publish=FALSE)
  flags <- data.table::data.table(source_flag_column="Flag",source_flag_value="SKIP",result_action="skip_result",
    priority=17L,active=FALSE,note="Intentionally excluded flag.")
  original_flags <- data.table::copy(flags)
  upsertImportResultFlagMappings(con,code,flags,publish=FALSE)
  expect_identical(flags,original_flags)
  saved_flags <- getImportResultFlagMappings(con,code,active=NULL,include_draft=TRUE)
  expect_identical(saved_flags$source_flag_column,"Flag")
  expect_identical(saved_flags$result_action,"skip_result")
  expect_identical(saved_flags$priority,17L)
  expect_false(saved_flags$active)
  expect_identical(saved_flags$note,"Intentionally excluded flag.")
  incomplete <- transform(mapping,ParamCode="INCOMPLETE",parameter_id=NA_integer_)
  upsertImportParameterMappings(con,code,incomplete,c("ParamCode","input_unit"),publish=FALSE)
  rows <- getImportMappingReview(con,source_code=code)
  expect_setequal(rows$mapping_type,c("parameter","location","result_flag"))
  expect_setequal(rows$status,c("published","draft"))
  expect_true(any(is.na(rows$profile_code)))
  expect_true(any(rows$profile_code %in% "workbook"))
  bad <- rows$status=="draft" & !is.na(rows$issue)
  expect_equal(sum(bad),1L)
  expect_match(rows$source_match[bad],"INCOMPLETE",fixed=TRUE)
  expect_error(publishImportMappings(con,code),"incomplete active")
  expect_identical(DBI::dbGetQuery(con,"SELECT source_description FROM discrete.import_sources WHERE source_code=$1",params=list(code))$source_description,
    "Retain this source description.")
  live <- AquaCache:::import_mapping_load_db(con,code)
  expect_equal(nrow(live),1L)
  expect_identical(AquaCache:::import_mapping_resolve_match(live,list(ParamCode="KNOWN",input_unit="mg/L"))$parameter_id,parameter)
  incomplete$active <- FALSE
  incomplete$note <- "Intentionally excluded pending analytical review."
  upsertImportParameterMappings(con,code,incomplete,c("ParamCode","input_unit"),publish=FALSE)
  publishImportMappings(con,code)
  current <- getImportMappingReview(con,source_code=code)
  history <- getImportMappingReview(con,source_code=code,include_history=TRUE)
  expect_false(any(current$status=="retired"))
  expect_true(any(history$status=="retired"))
  expect_false(any(current$active & !is.na(current$issue)))
  inactive <- current[!current$active & current$mapping_type=="parameter",,drop=FALSE]
  expect_equal(nrow(inactive),1L)
  expect_match(inactive$note,"Intentionally excluded",fixed=TRUE)
  definition <- jsonlite::fromJSON(inactive$definition[[1]])
  expect_false("import_mapping_id" %in% names(definition))
  expect_false("modified" %in% names(definition))
  expect_identical(definition$source_match$ParamCode,"INCOMPLETE")
  expect_false(definition$active)

  # A format may be created after mapping work has started. Its empty baseline
  # must not turn the source's existing draft into a published revision.
  draft_source <- paste0(code,"_draft")
  upsertImportParameterMappings(con,draft_source,mapping,c("ParamCode","input_unit"),publish=FALSE)
  upsertImportProfile(con,draft_source,"workbook","Draft source workbook",file_type="xlsx",
    datetime_origin="text",column_map=list(parameter_code="Analyte",unit="Unit",result="Value"))
  draft_review <- getImportMappingReview(con,source_code=draft_source)
  expect_identical(draft_review$status,"draft")
  scopes <- DBI::dbGetQuery(con,"SELECT ms.status,ms.import_profile_id FROM discrete.import_mapping_sets ms
    JOIN discrete.import_sources s USING(import_source_id) WHERE s.source_code=$1",params=list(draft_source))
  expect_false(any(scopes$status=="published" & is.na(scopes$import_profile_id)))
  expect_true(any(scopes$status=="published" & !is.na(scopes$import_profile_id)))

  standalone <- DBI::dbConnect(RPostgres::Postgres(),dbname="aquacache",host="10.250.12.154",
    user=Sys.getenv("aquacacheAdminUser"),password=Sys.getenv("aquacacheAdminPass"),
    options="-c lock_timeout=5000 -c statement_timeout=15000")
  on.exit(DBI::dbDisconnect(standalone),add=TRUE)
  failed_source <- paste0(code,"_failed_profile")
  stopifnot(DBI::dbGetQuery(standalone,"SELECT count(*)::INTEGER AS n FROM discrete.import_sources
    WHERE source_code=$1",params=list(failed_source))$n==0L)
  expect_error(upsertImportProfile(standalone,failed_source,"invalid","Invalid format",file_type="invalid"),"check constraint|file_type")
  expect_equal(DBI::dbGetQuery(standalone,"SELECT count(*)::INTEGER AS n FROM discrete.import_sources
    WHERE source_code=$1",params=list(failed_source))$n,0L)
})
