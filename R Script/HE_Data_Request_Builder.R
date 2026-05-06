library(shiny)
library(bslib)
library(DBI)
library(odbc)
library(pool)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(rpivotTable)
library(jsonlite)
library(openxlsx)
library(DT)
library(htmltools)

# --------------------------------------------------
# Config
# --------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

APP_TITLE <- "Higher Education Data Request Builder"
APP_SCHEMA <- Sys.getenv("APP_SCHEMA", "dbo")
APP_DB_PATTERN <- Sys.getenv("APP_DB_PATTERN", "HESA")
DEFAULT_PREVIEW_N <- 10000L
DEFAULT_MAX_INTERACTIVE_ROWS <- 1000000L
MAX_CATEGORICAL_CHOICES <- 200L
MAX_FILTER_TEXT_COLUMNS <- 15L
DEFAULT_FORMAT_FILE <- "variable_formats.R"

offline_mode <- {
  flag <- tolower(Sys.getenv("APP_OFFLINE", "true"))
  flag %in% c("true", "1", "yes")
}

# --------------------------------------------------
# SQL connection pool
# --------------------------------------------------
app_pool <- NULL
if (!offline_mode) {
  app_pool <- tryCatch(
    {
      dbPool(
        drv = odbc::odbc(),
        Driver = Sys.getenv("DB_DRIVER", "SQL Server"),
        Server = Sys.getenv("DB_SERVER", "SERVERNAME"),
        Database = Sys.getenv("DB_DATABASE", "DATABASENAME"),
        Trusted_Connection = Sys.getenv("DB_TRUSTED_CONNECTION", "True")
      )
    },
    error = function(e) {
      message("SQL pool creation failed. Falling back to offline mode.\n", e$message)
      NULL
    }
  )

  if (is.null(app_pool)) offline_mode <- TRUE
  if (!is.null(app_pool)) onStop(function() poolClose(app_pool))
}

# --------------------------------------------------
# Basic helpers
# --------------------------------------------------
parse_bracket_name <- function(x) {
  x <- as.character(x %||% "")
  if (!nzchar(x)) stop("Dataset name must not be empty.")

  three_part <- regmatches(x, regexec("^\\[([^]]+)\\]\\.\\[([^]]+)\\]\\.\\[([^]]+)\\]$", x))[[1]]
  if (length(three_part) == 4) {
    return(list(database = three_part[2], schema = three_part[3], object = three_part[4]))
  }

  two_part <- regmatches(x, regexec("^\\[([^]]+)\\]\\.\\[([^]]+)\\]$", x))[[1]]
  if (length(two_part) == 3) {
    return(list(database = NA_character_, schema = two_part[2], object = two_part[3]))
  }

  plain_parts <- strsplit(gsub("\\[|\\]", "", x), "\\.")[[1]]
  plain_parts <- plain_parts[nzchar(plain_parts)]
  if (length(plain_parts) >= 3) {
    return(list(database = plain_parts[length(plain_parts) - 2], schema = plain_parts[length(plain_parts) - 1], object = plain_parts[length(plain_parts)]))
  }
  if (length(plain_parts) == 2) {
    return(list(database = NA_character_, schema = plain_parts[1], object = plain_parts[2]))
  }

  stop("Dataset name must be supplied as [database].[schema].[object] or [schema].[object].")
}

sql_quote_ident <- function(x) paste0("[", gsub("]", "]]", x, fixed = TRUE), "]")
sql_quote_string <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")

parse_dataset_name_safe <- function(x) {
  x <- as.character(x %||% "")
  if (!nzchar(x)) return(list(database = NA_character_, schema = NA_character_, object = NA_character_))

  parsed <- tryCatch(parse_bracket_name(x), error = function(e) NULL)
  if (!is.null(parsed)) return(parsed)

  clean_x <- gsub("\\[|\\]", "", x)
  parts <- strsplit(clean_x, "\\.")[[1]]
  parts <- parts[nzchar(parts)]

  if (length(parts) >= 3) {
    return(list(
      database = parts[length(parts) - 2],
      schema = parts[length(parts) - 1],
      object = parts[length(parts)]
    ))
  }
  if (length(parts) == 2) {
    return(list(database = NA_character_, schema = parts[1], object = parts[2]))
  }

  list(database = NA_character_, schema = NA_character_, object = clean_x)
}

build_choice_vector <- function(values, labels = NULL) {
  values <- as.character(values %||% character(0))
  if (!length(values)) return(character(0))
  labels <- as.character(labels %||% values)
  if (length(labels) != length(values)) labels <- values
  stats::setNames(values, labels)
}

dataset_display_label <- function(family, academic_year = "", object_name = "") {
  family <- as.character(family %||% "")
  academic_year <- as.character(academic_year %||% "")
  object_name <- as.character(object_name %||% "")

  year_part <- ifelse(!is.na(academic_year) & nzchar(academic_year), academic_year, object_name)
  year_part[is.na(year_part)] <- ""
  family[is.na(family)] <- ""

  ifelse(nzchar(year_part), paste0(family, " (", year_part, ")"), family)
}

normalize_type <- function(sql_type) {
  t <- tolower(sql_type %||% "")
  if (t %in% c("int", "bigint", "smallint", "tinyint", "decimal", "numeric", "float", "real", "money", "smallmoney")) return("numeric")
  if (t %in% c("date", "datetime", "datetime2", "smalldatetime", "datetimeoffset")) return("date")
  if (t %in% c("bit")) return("logical")
  if (t %in% c("char", "nchar", "varchar", "nvarchar", "text", "ntext")) return("text")
  "text"
}

extract_hesa_year_fragment <- function(x) {
  object_name <- parse_dataset_name_safe(x)$object
  frag <- stringr::str_extract(object_name, "(?<!\\d)\\d{4}(?!\\d)")
  if (is.na(frag) || !nzchar(frag)) return(NA_character_)
  frag
}

extract_year_from_name <- function(x) {
  frag <- extract_hesa_year_fragment(x)
  if (!is.na(frag)) {
    start_yy <- substr(frag, 1, 2)
    return(suppressWarnings(as.integer(paste0("20", start_yy))))
  }

  object_name <- parse_dataset_name_safe(x)$object
  year_hit <- stringr::str_extract(object_name, "(19|20)\\d{2}")
  suppressWarnings(as.integer(year_hit))
}

extract_academic_year_label <- function(x) {
  frag <- extract_hesa_year_fragment(x)
  if (is.na(frag)) return("")
  paste0("20", substr(frag, 1, 2), "/", substr(frag, 3, 4))
}

academic_year_label_from_start_year <- function(year_value) {
  year_value <- suppressWarnings(as.integer(year_value))
  if (is.na(year_value)) return("")
  paste0(year_value, "/", sprintf("%02d", (year_value + 1) %% 100))
}

academic_year_label_for_block <- function(yr_block) {
  label <- academic_year_label_from_start_year(yr_block$year)
  if (nzchar(label)) return(label)

  datasets <- yr_block$datasets %||% character(0)
  if (length(datasets)) {
    label <- extract_academic_year_label(datasets[[1]])
    if (nzchar(label)) return(label)
  }

  "Unknown"
}

add_academic_year_columns <- function(df, yr_block) {
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  label <- academic_year_label_for_block(yr_block)

  if ("year" %in% names(out)) {
    out$source_year <- out$year
  }

  out$year <- label
  out$SelectedYear <- label
  out
}

extract_family_from_name <- function(x) {
  object_name <- parse_dataset_name_safe(x)$object
  out <- object_name |>
    stringr::str_remove_all("(19|20)\\d{2}") |>
    stringr::str_replace_all("[_-]+", " ") |>
    stringr::str_squish()
  if (!nzchar(out)) object_name else out
}

safe_join_keys <- function(df_list) {
  if (length(df_list) < 2) return(character(0))
  common_cols <- Reduce(intersect, lapply(df_list, names))
  preferred <- c("RecordID", "StudentID", "LearnerID", "PersonID", "ApplicationID", "Year", "Region", "Date", "NUMHUS", "UKPRN", "HUSID")
  preferred[preferred %in% common_cols]
}

sanitize_preview_data <- function(df) {
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(out)) {
    if (inherits(out[[nm]], c("Date", "POSIXct", "POSIXt"))) out[[nm]] <- as.character(out[[nm]])
    if (is.factor(out[[nm]])) out[[nm]] <- as.character(out[[nm]])
    if (is.character(out[[nm]])) out[[nm]][is.na(out[[nm]])] <- "(missing)"
  }
  out
}

df_to_basic_html <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(df)) return("<p>No rows to display.</p>")
  header_html <- paste0("<tr>", paste(sprintf("<th>%s</th>", htmlEscape(names(df))), collapse = ""), "</tr>")
  body_html <- apply(df, 1, function(row) {
    paste0("<tr>", paste(sprintf("<td>%s</td>", htmlEscape(as.character(row))), collapse = ""), "</tr>")
  })
  paste0("<table><thead>", header_html, "</thead><tbody>", paste(body_html, collapse = ""), "</tbody></table>")
}

style_download_html <- function(table_html, title = "", notes = "") {
  title_block <- if (nzchar(title)) sprintf("<h1>%s</h1>", htmlEscape(title)) else ""
  notes_block <- if (nzchar(notes)) sprintf("<div class='wg-notes'><strong>Note codes:</strong><br>%s</div>", gsub("\n", "<br>", htmlEscape(notes))) else ""

  paste0(
    "<!doctype html><html><head><meta charset='UTF-8'>",
    "<title>", htmlEscape(title %||% "Exported table"), "</title>",
    "<style>",
    "body{font-family:Arial,sans-serif; margin:32px; color:#1b1b1b;}",
    "h1{font-size:28px; margin-bottom:18px; color:#003078;}",
    ".wg-notes{margin-top:18px; padding-top:12px; border-top:2px solid #b1b4b6;}",
    "table{border-collapse:collapse; width:100%;}",
    "th,td{border:1px solid #b1b4b6; padding:8px; font-size:13px;}",
    "th{background:#f3f2f1; text-align:left;}",
    "</style></head><body>",
    title_block,
    table_html,
    notes_block,
    "</body></html>"
  )
}

# --------------------------------------------------
# Variable formats file support
# --------------------------------------------------
load_variable_formats <- function() {
  candidate_paths <- c(
    Sys.getenv("APP_FORMAT_FILE", DEFAULT_FORMAT_FILE),
    file.path(getwd(), DEFAULT_FORMAT_FILE),
    file.path(dirname(normalizePath(sys.frame(1)$ofile %||% getwd(), mustWork = FALSE)), DEFAULT_FORMAT_FILE),
    file.path("/mnt/data", DEFAULT_FORMAT_FILE)
  )
  candidate_paths <- unique(candidate_paths[file.exists(candidate_paths)])

  empty_result <- list(
    file = NULL,
    labels = list(),
    levels = list(),
    transforms = list()
  )

  if (!length(candidate_paths)) return(empty_result)

  env <- new.env(parent = globalenv())
  source(candidate_paths[[1]], local = env)

  fmt <- env$variable_formats %||% env$formats %||% empty_result
  fmt$file <- candidate_paths[[1]]
  fmt$labels <- fmt$labels %||% list()
  fmt$levels <- fmt$levels %||% list()
  fmt$transforms <- fmt$transforms %||% list()
  fmt
}

apply_variable_formats <- function(df, fmt) {
  if (!nrow(df)) return(df)
  out <- df

  for (nm in names(fmt$levels)) {
    if (!nm %in% names(out)) next
    map <- fmt$levels[[nm]]
    out[[nm]] <- dplyr::recode(as.character(out[[nm]]), !!!map, .default = as.character(out[[nm]]))
  }

  for (nm in names(fmt$transforms)) {
    if (!nm %in% names(out)) next
    fn <- fmt$transforms[[nm]]
    if (is.function(fn)) out[[nm]] <- fn(out[[nm]])
  }

  label_map <- fmt$labels
  if (length(label_map)) {
    renamed <- names(out)
    for (i in seq_along(renamed)) {
      renamed[i] <- label_map[[renamed[i]]] %||% renamed[i]
    }
    names(out) <- make.unique(renamed)
  }

  out
}

# --------------------------------------------------
# Offline data provider
# --------------------------------------------------
make_mock_data <- function(dataset_name, n = 5000L) {
  set.seed(sum(utf8ToInt(dataset_name)))
  ds <- parse_dataset_name_safe(dataset_name)$object
  year_guess <- extract_year_from_name(dataset_name) %||% sample(2021:2026, 1)
  months <- month.abb

  base <- data.frame(
    RecordID = seq_len(n),
    StudentID = sample(100000:999999, n, replace = TRUE),
    Year = year_guess,
    Month = sample(months, n, TRUE),
    Region = sample(c("North Wales", "South East Wales", "South West Wales", "Mid Wales"), n, TRUE),
    Gender = sample(c("Female", "Male", "Other"), n, TRUE),
    AgeBand = sample(c("<18", "18-24", "25-34", "35-44", "45+"), n, TRUE),
    stringsAsFactors = FALSE
  )
  base$Quarter <- paste0("Q", ((match(base$Month, months) - 1) %/% 3) + 1)
  base$Date <- as.Date(sprintf("%d-%02d-%02d", base$Year, match(base$Month, months), sample(1:28, n, TRUE)))

  if (grepl("subject", ds, ignore.case = TRUE)) {
    out <- base |>
      mutate(
        Subject = sample(c("STEM", "Humanities", "Creative Arts", "Health"), n, TRUE),
        SubjectCode = sample(c("S01", "S02", "S03", "S04"), n, TRUE),
        Credits = sample(c(15, 30, 45, 60), n, TRUE),
        Count = sample(1:250, n, TRUE)
      )
  } else if (grepl("apprent", ds, ignore.case = TRUE)) {
    out <- base |>
      mutate(
        Framework = sample(c("Digital", "Health", "Engineering", "Business"), n, TRUE),
        Level = sample(c("Level 2", "Level 3", "Level 4"), n, TRUE),
        Status = sample(c("Started", "Completed", "Withdrawn"), n, TRUE),
        Count = sample(1:200, n, TRUE)
      )
  } else {
    out <- base |>
      mutate(
        Provider = sample(c("Provider A", "Provider B", "Provider C"), n, TRUE),
        Programme = sample(c("HE", "FE", "Apprenticeship"), n, TRUE),
        Status = sample(c("Open", "Closed", "Pending", NA), n, TRUE, prob = c(.35, .35, .25, .05)),
        Amount = round(runif(n, 50, 10000), 2),
        Count = sample(1:500, n, TRUE)
      )
  }

  out$DatasetName <- dataset_name
  out
}

list_datasets_offline <- function() {
  c(
    "[request].[student_2021]",
    "[request].[student_2122]",
    "[request].[student_2223]",
    "[request].[subject_2021]",
    "[request].[subject_2122]",
    "[request].[subject_2223]",
    "[request].[apprenticeship_2122]",
    "[request].[apprenticeship_2223]"
  )
}

get_column_metadata_offline <- function(dataset_name) {
  names(make_mock_data(dataset_name, n = 10L)) |>
    purrr::map_dfr(function(nm, idx = NULL) {
      vals <- make_mock_data(dataset_name, n = 20L)[[nm]]
      dtype <- if (inherits(vals, "Date")) "date" else if (is.numeric(vals)) "float" else "varchar"
      data.frame(column_name = nm, data_type = dtype, stringsAsFactors = FALSE)
    }) |>
    mutate(ordinal_position = row_number())
}

fetch_distinct_values_offline <- function(dataset_names, col, limit = MAX_CATEGORICAL_CHOICES) {
  tmp <- bind_rows(lapply(dataset_names, make_mock_data, n = 1200L))
  vals <- sort(unique(as.character(tmp[[col]])))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  head(vals, limit)
}

# --------------------------------------------------
# SQL provider
# --------------------------------------------------
list_datasets_sql <- function(con, schema = APP_SCHEMA, db_pattern = APP_DB_PATTERN) {
  db_sql <- paste0(
    "SELECT name FROM sys.databases WHERE state_desc = 'ONLINE' ",
    if (nzchar(db_pattern)) "AND name LIKE ? " else "",
    "ORDER BY name;"
  )
  dbs <- if (nzchar(db_pattern)) {
    DBI::dbGetQuery(con, db_sql, params = list(paste0(db_pattern, "%")))$name
  } else {
    DBI::dbGetQuery(con, db_sql)$name
  }
  dbs <- as.character(dbs %||% character(0))
  if (!length(dbs)) return(character(0))

  out <- character(0)
  for (db in dbs) {
    qry <- paste0(
      "SELECT CONCAT(QUOTENAME(TABLE_CATALOG), '.', QUOTENAME(TABLE_SCHEMA), '.', QUOTENAME(TABLE_NAME)) AS full_name ",
      "FROM ", sql_quote_ident(db), ".INFORMATION_SCHEMA.TABLES ",
      "WHERE TABLE_TYPE IN ('BASE TABLE', 'VIEW') ",
      if (nzchar(schema)) "AND TABLE_SCHEMA = ? " else "",
      "ORDER BY TABLE_NAME;"
    )
    res <- tryCatch(
      if (nzchar(schema)) DBI::dbGetQuery(con, qry, params = list(schema)) else DBI::dbGetQuery(con, qry),
      error = function(e) data.frame(full_name = character(0))
    )
    vals <- as.character(res[["full_name"]] %||% character(0))
    out <- c(out, vals)
  }
  unique(out)
}

get_column_metadata_sql <- function(con, dataset_name) {
  p <- parse_bracket_name(dataset_name)
  db_part <- if (!is.na(p$database)) paste0(sql_quote_ident(p$database), ".") else ""
  sql <- paste0(
    "SELECT COLUMN_NAME, DATA_TYPE, ORDINAL_POSITION ",
    "FROM ", db_part, "INFORMATION_SCHEMA.COLUMNS ",
    "WHERE TABLE_SCHEMA = ? AND TABLE_NAME = ? ",
    "ORDER BY ORDINAL_POSITION;"
  )
  out <- DBI::dbGetQuery(con, sql, params = list(p$schema, p$object))
  if (!nrow(out)) return(data.frame(column_name = character(0), data_type = character(0), ordinal_position = integer(0)))
  names(out) <- c("column_name", "data_type", "ordinal_position")
  out
}



combine_column_metadata <- function(meta_list) {
  meta_list <- meta_list[vapply(meta_list, nrow, integer(1)) > 0]
  if (!length(meta_list)) {
    return(data.frame(
      column_name = character(0),
      data_type = character(0),
      ordinal_position = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  combined <- bind_rows(meta_list) |>
    group_by(column_name) |>
    summarise(
      data_type = dplyr::first(data_type),
      ordinal_position = min(ordinal_position, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(ordinal_position, column_name)

  as.data.frame(combined, stringsAsFactors = FALSE)
}

get_combined_column_metadata_offline <- function(dataset_names) {
  meta_list <- lapply(dataset_names, get_column_metadata_offline)
  combine_column_metadata(meta_list)
}

get_combined_column_metadata_sql <- function(con, dataset_names) {
  meta_list <- lapply(dataset_names, function(ds) get_column_metadata_sql(con, ds))
  combine_column_metadata(meta_list)
}

fetch_distinct_values_sql <- function(con, dataset_names, col, limit = MAX_CATEGORICAL_CHOICES) {
  parts <- vapply(dataset_names, function(ds) {
    sprintf(
      "SELECT DISTINCT TOP (%d) %s AS v FROM %s WHERE %s IS NOT NULL",
      limit,
      sql_quote_ident(col),
      ds,
      sql_quote_ident(col)
    )
  }, character(1))

  sql <- paste0(
    "SELECT DISTINCT TOP (", limit, ") v FROM (",
    paste(parts, collapse = " UNION ALL "),
    ") q ORDER BY v"
  )
  as.character(DBI::dbGetQuery(con, sql)$v)
}

build_filter_clause <- function(meta, input, prefix = "flt_") {
  clauses <- c()
  for (i in seq_len(nrow(meta))) {
    col <- meta$column_name[i]
    type <- normalize_type(meta$data_type[i])
    id_base <- paste0(prefix, make.names(col))
    qcol <- sql_quote_ident(col)

    if (type == "text") {
      vals <- input[[paste0(id_base, "_cat")]]
      txt <- trimws(input[[paste0(id_base, "_contains")]] %||% "")
      if (!is.null(vals) && length(vals) > 0) {
        vals_q <- paste(vapply(vals, sql_quote_string, character(1)), collapse = ",")
        clauses <- c(clauses, paste0(qcol, " IN (", vals_q, ")"))
      } else if (nzchar(txt)) {
        clauses <- c(clauses, paste0(qcol, " LIKE ", sql_quote_string(paste0("%", txt, "%"))))
      }
    } else if (type == "numeric") {
      lo <- trimws(input[[paste0(id_base, "_min")]] %||% "")
      hi <- trimws(input[[paste0(id_base, "_max")]] %||% "")
      if (nzchar(lo)) clauses <- c(clauses, paste0(qcol, " >= ", as.numeric(lo)))
      if (nzchar(hi)) clauses <- c(clauses, paste0(qcol, " <= ", as.numeric(hi)))
    } else if (type == "date") {
      lo <- trimws(input[[paste0(id_base, "_from")]] %||% "")
      hi <- trimws(input[[paste0(id_base, "_to")]] %||% "")
      if (nzchar(lo)) clauses <- c(clauses, paste0(qcol, " >= ", sql_quote_string(lo)))
      if (nzchar(hi)) clauses <- c(clauses, paste0(qcol, " <= ", sql_quote_string(hi)))
    }
  }
  if (!length(clauses)) "" else paste("WHERE", paste(clauses, collapse = " AND "))
}

fetch_table_sql <- function(con, dataset_name, where_sql = "", limit = NULL, random_sample = FALSE) {
  top_clause <- if (!is.null(limit)) paste0("TOP (", as.integer(limit), ") ") else ""
  order_clause <- if (random_sample && !is.null(limit)) " ORDER BY NEWID()" else ""
  sql <- paste0("SELECT ", top_clause, "* FROM ", dataset_name, " ", where_sql, order_clause)
  DBI::dbGetQuery(con, sql)
}

count_rows_sql <- function(con, dataset_name, where_sql = "") {
  sql <- paste0("SELECT COUNT_BIG(1) AS n FROM ", dataset_name, " ", where_sql)
  as.numeric(DBI::dbGetQuery(con, sql)$n[[1]] %||% 0)
}

# --------------------------------------------------
# Selection, merge, and filter utilities
# --------------------------------------------------
make_dataset_catalogue <- function(dataset_names) {
  dataset_names <- as.character(dataset_names %||% character(0))
  if (!length(dataset_names)) {
    return(tibble(
      dataset_name = character(0),
      year = integer(0),
      academic_year = character(0),
      family = character(0),
      object_name = character(0)
    ))
  }

  tibble(
    dataset_name = dataset_names,
    year = purrr::map_int(dataset_names, ~ extract_year_from_name(.x) %||% NA_integer_),
    academic_year = purrr::map_chr(dataset_names, extract_academic_year_label),
    family = purrr::map_chr(dataset_names, extract_family_from_name),
    object_name = purrr::map_chr(dataset_names, ~ parse_dataset_name_safe(.x)$object)
  ) |>
    arrange(year, family, object_name)
}

build_selection_plan <- function(input, req_type, selected_years) {
  if (identical(req_type, "single")) {
    main_ds <- input$single_dataset_main
    extra_ds <- input$single_dataset_extra %||% character(0)
    return(list(
      type = "single",
      years = list(list(year = extract_year_from_name(main_ds), datasets = unique(c(main_ds, extra_ds))))
    ))
  }

  years <- vector("list", length(selected_years))
  for (i in seq_along(selected_years)) {
    yr <- selected_years[i]
    main_id <- paste0("ts_main_", yr)
    extra_id <- paste0("ts_extra_", yr)
    main_ds <- input[[main_id]]
    extra_ds <- input[[extra_id]] %||% character(0)
    years[[i]] <- list(year = yr, datasets = unique(c(main_ds, extra_ds)))
  }

  list(type = "timeseries", years = years)
}

selection_datasets <- function(selection_plan) {
  unique(unlist(lapply(selection_plan$years, `[[`, "datasets"), use.names = FALSE))
}

merge_year_group <- function(df_list) {
  df_list <- df_list[lengths(df_list) > 0]
  if (!length(df_list)) return(data.frame())
  if (length(df_list) == 1) return(df_list[[1]])

  join_keys <- safe_join_keys(df_list)
  if (!length(join_keys)) {
    tagged <- Map(function(df, idx) mutate(df, MergeSource = paste0("Dataset ", idx)), df_list, seq_along(df_list))
    return(bind_rows(tagged))
  }

  Reduce(function(x, y) full_join(x, y, by = join_keys), df_list)
}

apply_filters_offline <- function(df, meta, input, prefix = "flt_") {
  out <- df
  for (i in seq_len(nrow(meta))) {
    col <- meta$column_name[i]
    if (!col %in% names(out)) next
    type <- normalize_type(meta$data_type[i])
    id_base <- paste0(prefix, make.names(col))

    if (type == "text") {
      vals <- input[[paste0(id_base, "_cat")]]
      txt <- trimws(input[[paste0(id_base, "_contains")]] %||% "")
      if (!is.null(vals) && length(vals) > 0) {
        out <- out[as.character(out[[col]]) %in% vals, , drop = FALSE]
      } else if (nzchar(txt)) {
        out <- out[grepl(txt, as.character(out[[col]]), ignore.case = TRUE), , drop = FALSE]
      }
    } else if (type == "numeric") {
      lo <- suppressWarnings(as.numeric(trimws(input[[paste0(id_base, "_min")]] %||% "")))
      hi <- suppressWarnings(as.numeric(trimws(input[[paste0(id_base, "_max")]] %||% "")))
      if (!is.na(lo)) out <- out[out[[col]] >= lo, , drop = FALSE]
      if (!is.na(hi)) out <- out[out[[col]] <= hi, , drop = FALSE]
    } else if (type == "date") {
      lo <- suppressWarnings(as.Date(trimws(input[[paste0(id_base, "_from")]] %||% "")))
      hi <- suppressWarnings(as.Date(trimws(input[[paste0(id_base, "_to")]] %||% "")))
      x <- as.Date(out[[col]])
      if (!is.na(lo)) out <- out[x >= lo, , drop = FALSE]
      if (!is.na(hi)) out <- out[x <= hi, , drop = FALSE]
    }
  }
  out
}

fetch_selected_data_offline <- function(selection_plan, meta, input, max_rows = NULL, random_sample = FALSE) {
  year_frames <- lapply(selection_plan$years, function(yr_block) {
    block_frames <- lapply(yr_block$datasets, function(ds) {
      apply_filters_offline(make_mock_data(ds, n = 4000L), meta, input)
    })
    out <- merge_year_group(block_frames)
    out <- add_academic_year_columns(out, yr_block)
    out
  })

  out <- bind_rows(year_frames)
  if (!is.null(max_rows) && nrow(out) > max_rows) {
    if (random_sample) {
      out <- out[sample(seq_len(nrow(out)), max_rows), , drop = FALSE]
    } else {
      out <- out[seq_len(max_rows), , drop = FALSE]
    }
  }
  rownames(out) <- NULL
  out
}

fetch_selected_data_sql <- function(con, selection_plan, meta, input, max_rows = NULL, random_sample = FALSE) {
  where_sql <- build_filter_clause(meta, input)

  year_frames <- lapply(selection_plan$years, function(yr_block) {
    block_frames <- lapply(yr_block$datasets, function(ds) {
      fetch_table_sql(con, ds, where_sql = where_sql, limit = NULL, random_sample = FALSE)
    })
    out <- merge_year_group(block_frames)
    out <- add_academic_year_columns(out, yr_block)
    out
  })

  out <- bind_rows(year_frames)
  if (!is.null(max_rows) && nrow(out) > max_rows) {
    if (random_sample) {
      out <- out[sample(seq_len(nrow(out)), max_rows), , drop = FALSE]
    } else {
      out <- out[seq_len(max_rows), , drop = FALSE]
    }
  }
  rownames(out) <- NULL
  out
}

count_selected_rows_offline <- function(selection_plan, meta, input) {
  nrow(fetch_selected_data_offline(selection_plan, meta, input, max_rows = NULL, random_sample = FALSE))
}

count_selected_rows_sql <- function(con, selection_plan, meta, input) {
  nrow(fetch_selected_data_sql(con, selection_plan, meta, input, max_rows = NULL, random_sample = FALSE))
}

# --------------------------------------------------
# Pivot capture helper
# --------------------------------------------------
pivot_table_to_df <- function(pivot_obj) {
  if (is.null(pivot_obj)) return(data.frame())
  if (is.data.frame(pivot_obj)) return(as.data.frame(pivot_obj, stringsAsFactors = FALSE, check.names = FALSE))

  if (is.character(pivot_obj) && length(pivot_obj) == 1) {
    parsed <- tryCatch(jsonlite::fromJSON(pivot_obj, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(parsed)) return(pivot_table_to_df(parsed))
  }

  if (is.list(pivot_obj) && !is.null(pivot_obj$data)) pivot_obj <- pivot_obj$data
  if (is.list(pivot_obj) && !is.null(pivot_obj$result)) pivot_obj <- pivot_obj$result

  if (is.list(pivot_obj) && !is.null(pivot_obj$matrix)) {
    mat <- pivot_obj$matrix
    if (is.null(mat) || !length(mat)) return(data.frame())

    row_list <- lapply(mat, function(r) {
      vals <- as.character(unlist(r, use.names = FALSE))
      vals[is.na(vals)] <- ""
      vals
    })

    if (!length(row_list)) return(data.frame())

    max_cols <- max(vapply(row_list, length, integer(1)))
    row_list <- lapply(row_list, function(r) {
      length(r) <- max_cols
      r[is.na(r)] <- ""
      r
    })

    mat2 <- do.call(rbind, row_list)
    out <- as.data.frame(mat2, stringsAsFactors = FALSE, check.names = FALSE)

    if (nrow(out) >= 2) {
      header_rows <- out[1:2, , drop = FALSE]
      cn <- character(ncol(header_rows))
      for (j in seq_len(ncol(header_rows))) {
        parts <- trimws(as.character(header_rows[, j]))
        parts <- parts[nzchar(parts)]
        parts <- unique(parts)
        cn[j] <- if (length(parts)) paste(parts, collapse = " | ") else paste0("Col", j)
      }
      names(out) <- make.unique(cn)
      out <- out[-c(1, 2), , drop = FALSE]
    } else {
      names(out) <- paste0("Col", seq_len(ncol(out)))
    }

    keep <- apply(out, 1, function(x) any(nzchar(trimws(as.character(x)))))
    out <- out[keep, , drop = FALSE]
    rownames(out) <- NULL
    return(out)
  }

  if (is.list(pivot_obj) && !is.null(pivot_obj$rows)) {
    rows <- pivot_obj$rows %||% list()
    headers <- pivot_obj$headers %||% list()
    if (!length(rows)) return(data.frame())

    row_list <- lapply(rows, function(r) as.character(unlist(r, use.names = FALSE)))
    max_cols <- max(vapply(row_list, length, integer(1)))
    row_list <- lapply(row_list, function(r) { length(r) <- max_cols; r })
    mat <- do.call(rbind, row_list)
    out <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)

    if (length(headers)) {
      hdr_list <- lapply(headers, function(h) as.character(unlist(h, use.names = FALSE)))
      hdr_list <- lapply(hdr_list, function(h) { length(h) <- max_cols; h })
      hdr_mat <- do.call(rbind, hdr_list)
      cn <- character(max_cols)
      for (j in seq_len(max_cols)) {
        parts <- trimws(hdr_mat[, j])
        parts <- parts[!is.na(parts) & nzchar(parts)]
        parts <- unique(parts)
        cn[j] <- if (length(parts)) paste(parts, collapse = " | ") else paste0("Col", j)
      }
      names(out) <- make.unique(cn)
    } else {
      names(out) <- paste0("Col", seq_len(ncol(out)))
    }

    keep <- apply(out, 1, function(x) any(!is.na(x) & nzchar(trimws(as.character(x)))))
    out <- out[keep, , drop = FALSE]
    rownames(out) <- NULL
    return(out)
  }

  stop("Unsupported pivot payload type.")
}

round_numeric_to_base <- function(x, base = 5) {
  if (!is.numeric(x)) return(x)
  round(x / base) * base
}

round_df_for_export <- function(df, mode = "none") {
  out <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  if (!identical(mode, "5")) return(out)
  out[] <- lapply(out, function(col) {
    if (is.numeric(col)) round_numeric_to_base(col, 5) else col
  })
  out
}

selected_variable_names <- function(meta, input, default_all = TRUE) {
  if (is.null(meta) || !nrow(meta)) return(character(0))
  out <- character(0)
  for (i in seq_len(nrow(meta))) {
    col <- meta$column_name[i]
    id <- paste0("var_keep_", make.names(col))
    keep <- input[[id]]
    if (is.null(keep)) keep <- default_all
    if (isTRUE(keep)) out <- c(out, col)
  }
  unique(out)
}

subset_for_pivot <- function(df, selected_cols) {
  if (is.null(df) || !nrow(df)) return(df)
  selected_cols <- intersect(unique(selected_cols %||% character(0)), names(df))
  if (!length(selected_cols)) selected_cols <- names(df)
  out <- df[, selected_cols, drop = FALSE]
  numeric_cols <- names(out)[vapply(out, is.numeric, logical(1))]
  if (!length(numeric_cols)) out$.RecordCount <- 1L
  out
}

# --------------------------------------------------
# Theme and styling
# --------------------------------------------------
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#003078",
  secondary = "#4c6272",
  success = "#00703c",
  bg = "#f8f8f8",
  fg = "#1b1b1b",
  base_font = c("Inter")
)

app_css <- HTML(
  ".app-shell{max-width:1400px; margin:0 auto;}\n",
  ".hero-panel{background:#ffffff; border-left:8px solid #003078; padding:1.5rem; border-radius:1rem; box-shadow:0 1px 8px rgba(0,0,0,0.08);}\n",
  ".step-chip{display:inline-block; padding:0.35rem 0.75rem; border-radius:999px; background:#d8e7f7; color:#003078; font-weight:600; margin-right:0.5rem;}\n",
  ".status-pill{display:inline-block; padding:0.3rem 0.65rem; border-radius:999px; background:#f3f2f1; border:1px solid #b1b4b6; margin-right:0.5rem;}\n",
  ".wg-card{background:#fff; border:1px solid #d8d8d8; border-radius:1rem; padding:1rem; margin-bottom:1rem; box-shadow:0 1px 6px rgba(0,0,0,0.05);}\n",
  ".wg-muted{color:#505a5f;}\n",
  ".wg-actions{display:flex; gap:0.75rem; flex-wrap:wrap; margin-top:1rem;}\n",
  ".form-label,.control-label{font-weight:600;}\n",
  ".sticky-sidebar{position:sticky; top:1rem; max-height:calc(100vh - 2rem); overflow-y:auto;}\n",
  ".filter-shell{display:flex; flex-direction:column; gap:1rem;}\n",
  ".filter-search-box{background:#ffffff; border:1px solid #d8d8d8; border-radius:1rem; padding:1rem; position:sticky; top:0; z-index:2;}\n",
  ".filter-results-box{background:#ffffff; border:1px solid #d8d8d8; border-radius:1rem; padding:1rem; max-height:70vh; overflow-y:auto;}\n",
  ".var-meta-row{display:flex; justify-content:space-between; align-items:center; gap:1rem; flex-wrap:wrap;}\n",
  ".small-muted{font-size:0.9rem; color:#505a5f;}\n"
)

# --------------------------------------------------
# UI components
# --------------------------------------------------
nav_buttons <- function(back_id = NULL, next_id = NULL, next_label = "Next") {
  div(
    class = "wg-actions",
    if (!is.null(back_id)) actionButton(back_id, "Back", class = "btn btn-outline-secondary"),
    if (!is.null(next_id)) actionButton(next_id, next_label, class = "btn btn-primary"),
    actionButton("exit_app", "Exit", class = "btn btn-outline-dark")
  )
}

page_header <- function(step_no, title, subtitle = NULL, status_line = NULL) {
  div(
    class = "hero-panel mb-3",
    div(class = "step-chip", paste("Step", step_no)),
    tags$h2(title),
    if (!is.null(subtitle)) tags$p(class = "wg-muted", subtitle),
    if (!is.null(status_line)) div(status_line)
  )
}

# --------------------------------------------------
# Main app
# --------------------------------------------------
ui <- page_fillable(
  theme = app_theme,
  tags$head(tags$style(app_css)),
  div(class = "app-shell p-3", uiOutput("page_ui"))
)

server <- function(input, output, session) {
  formats <- reactiveVal(load_variable_formats())

  rv <- reactiveValues(
    page = "start",
    connection_status = if (offline_mode) "Offline mode" else "Connected to SQL Server",
    datasets_all = character(0),
    catalogue = NULL,
    selected_years = integer(0),
    selection_plan = NULL,
    filter_meta = NULL,
    filter_choices = list(),
    selected_variables = character(0),
    filter_state = list(),
    filters_open = NULL,
    filtered_row_count = NULL,
    preview_data = NULL,
    formatted_data = NULL,
    pivot_payload = NULL,
    pivot_html_cache = NULL,
    final_table = NULL
  )

  observe({
    rv$datasets_all <- tryCatch(
      {
        if (offline_mode) {
          list_datasets_offline()
        } else {
          con <- poolCheckout(app_pool); on.exit(poolReturn(con), add = TRUE)
          list_datasets_sql(con)
        }
      },
      error = function(e) {
        showNotification(paste("Dataset discovery failed:", e$message), type = "error", duration = 8)
        character(0)
      }
    )
    rv$catalogue <- make_dataset_catalogue(rv$datasets_all)
  })

  current_status_line <- reactive({
    tagList(
      span(class = "status-pill", rv$connection_status),
      span(class = "status-pill", paste("DB pattern:", APP_DB_PATTERN)),
      span(class = "status-pill", paste("Schema:", APP_SCHEMA)),
      span(class = "status-pill", paste("Formats file:", basename(formats()$file %||% "not found")))
    )
  })

  output$page_ui <- renderUI({
    req(rv$catalogue)

    switch(
      rv$page,
      start = tagList(
        page_header(
          1,
          APP_TITLE,
          "Build a single-year or time-series request, filter the result, optionally apply variable formats, and then produce a pivot-ready export.",
          current_status_line()
        ),
        div(
          class = "wg-card",
          radioButtons(
            "req_type",
            "Choose request type",
            choices = c("Single year / one main dataset" = "single", "Timeseries" = "ts"),
            selected = "single"
          ),
          checkboxInput("auto_fallback_offline", "If SQL fails during development, continue using offline testing data", value = TRUE),
          nav_buttons(next_id = "to_dataset")
        )
      ),
      dataset = tagList(
        page_header(
          2,
          "Dataset decisions",
          "Choose the datasets to use. For time series, choose the years first, then a main dataset for each year plus any extra datasets you want merged into that year.",
          current_status_line()
        ),
        div(
          class = "wg-card",
          if (!nrow(rv$catalogue)) {
            tags$div(
              class = "alert alert-warning",
              tags$strong("No datasets were found."),
              tags$div("Check the database pattern, schema, and SQL permissions. The app is currently looking for databases that start with '", APP_DB_PATTERN, "' and schema '", APP_SCHEMA, "'.")
            )
          } else if (identical(input$req_type %||% "single", "single")) {
            tagList(
              selectizeInput(
                "single_dataset_main",
                "Main dataset",
                choices = NULL,
                multiple = FALSE,
                options = list(
                  placeholder = "Search for a dataset",
                  maxOptions = 2000
                )
              ),
              selectizeInput(
                "single_dataset_extra",
                "Extra datasets to merge with the main dataset",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Optional",
                  maxOptions = 2000
                )
              )
            )
          } else {
            tagList(
              selectizeInput(
                "ts_years",
                "Years to include",
                choices = sort(unique(stats::na.omit(rv$catalogue$year))),
                selected = head(sort(unique(stats::na.omit(rv$catalogue$year))), 2),
                multiple = TRUE,
                options = list(placeholder = "Choose one or more years")
              ),
              uiOutput("ts_year_ui")
            )
          },
          nav_buttons(back_id = "back_to_start", next_id = "to_filters")
        )
      ),
      filters = tagList(
        page_header(
          3,
          "Filtering and formatting",
          "Filter the request down before building the pivot. You can also switch on the variable formats file so labels, recodes, and transformations are applied before the pivot table is created.",
          current_status_line()
        ),
        layout_sidebar(
          sidebar = sidebar(
            width = 360,
            class = "sticky-sidebar",
            numericInput("max_interactive_rows", "Maximum rows for a full interactive pivot", value = DEFAULT_MAX_INTERACTIVE_ROWS, min = 1000, step = 1000),
            numericInput("preview_n", "Preview sample size when the result is too large", value = DEFAULT_PREVIEW_N, min = 100, step = 100),
            checkboxInput("preview_if_large", "Use a sample preview when the filtered result is above the threshold", value = TRUE),
            checkboxInput("apply_standard_formats", "Apply variable formats from external R file", value = FALSE),
            actionButton("reload_formats", "Reload formats file", class = "btn btn-outline-secondary"),
            tags$hr(),
            textInput("filter_var_search", "Search variables", value = "", placeholder = "Search variable names or types"),
            tags$div(class = "small-muted", textOutput("selected_var_text", inline = TRUE)),
            div(
              class = "wg-actions",
              actionButton("select_all_vars", "Select all variables", class = "btn btn-outline-secondary btn-sm"),
              actionButton("select_no_vars", "Clear variable selection", class = "btn btn-outline-secondary btn-sm")
            ),
            tags$hr(),
            actionButton("count_rows", "Check row count", class = "btn btn-primary"),
            actionButton("clear_filters", "Clear filters", class = "btn btn-outline-secondary"),
            tags$div(class = "mt-2", strong(textOutput("row_count_text", inline = TRUE))),
            nav_buttons(back_id = "back_to_dataset", next_id = "to_pivot", next_label = "Load pivot")
          ),
          div(
            class = "filter-shell",
            div(
              class = "filter-search-box",
              tags$strong("Variable selection and filters"),
              tags$p(class = "wg-muted mb-0", "Tick variables to include them in the pivot/export. Filters remain available for all variables so you can filter without displaying a field.")
            ),
            div(
              class = "filter-results-box",
              uiOutput("filters_ui")
            )
          )
        )
      ),
      pivot = tagList(
        page_header(
          4,
          "Pivot table",
          "Drag and drop fields into the pivot table. The pivot uses the filtered result only.",
          current_status_line()
        ),
        div(
          class = "wg-card",
          p(class = "wg-muted", "If the filtered data is larger than the threshold, the pivot is built from a preview sample so the app stays responsive."),
          rpivotTableOutput("pivot_widget", width = "100%", height = "680px"),
          checkboxInput("show_preview", "Show preview data used in the pivot", FALSE),
          conditionalPanel("input.show_preview == true", DTOutput("preview_table")),
          nav_buttons(back_id = "back_to_filters", next_id = "to_export")
        )
      ),
      export = tagList(
        page_header(
          5,
          "Outputs and table formatting",
          "Add a title and note codes if needed, then export the pivot result to HTML, CSV, or Excel.",
          current_status_line()
        ),
        div(
          class = "wg-card",
          radioButtons("export_mode", "Export mode", choices = c("Export as is" = "raw", "Format with title and note codes" = "format"), selected = "raw"),
          selectInput("round_option", "Rounding for exports", choices = c("No rounding" = "none", "Round numeric values to nearest 5" = "5"), selected = "none"),
          conditionalPanel(
            "input.export_mode == 'format'",
            textInput("export_title", "Table title", value = ""),
            textAreaInput("export_notes", "Note code(s)", value = "", rows = 4)
          ),
          actionButton("refresh_final", "Build export table from pivot", class = "btn btn-primary"),
          tags$hr(),
          DTOutput("final_preview"),
          tags$hr(),
          div(
            class = "wg-actions",
            downloadButton("download_html", "Download HTML"),
            downloadButton("download_csv", "Download CSV"),
            downloadButton("download_xlsx", "Download Excel")
          ),
          nav_buttons(back_id = "back_to_pivot")
        )
      )
    )
  })

  output$ts_year_ui <- renderUI({
    req(rv$catalogue)
    years <- sort(as.integer(input$ts_years %||% integer(0)))
    rv$selected_years <- years
    if (!length(years)) return(tags$p(class = "wg-muted", "Choose at least one year."))

    tagList(lapply(years, function(yr) {
      yr_choices <- rv$catalogue |>
        filter(year == yr) |>
        mutate(label = dataset_display_label(family, academic_year, object_name))

      div(
        class = "wg-card",
        tags$h4(paste("Academic year starting", yr)),
        selectizeInput(
          paste0("ts_main_", yr),
          paste("Main dataset for", yr),
          choices = build_choice_vector(yr_choices$dataset_name, yr_choices$label),
          multiple = FALSE,
          options = list(placeholder = "Search for a dataset")
        ),
        selectizeInput(
          paste0("ts_extra_", yr),
          paste("Extra datasets to merge into", yr),
          choices = build_choice_vector(yr_choices$dataset_name, yr_choices$label),
          multiple = TRUE,
          options = list(placeholder = "Optional")
        )
      )
    }))
  })

  observeEvent(rv$catalogue, {
    req(nrow(rv$catalogue) > 0)

    single_choices <- rv$catalogue |>
      mutate(label = dataset_display_label(family, academic_year, object_name))

    choice_vec <- build_choice_vector(single_choices$dataset_name, single_choices$label)

    updateSelectizeInput(
      session,
      "single_dataset_main",
      choices = choice_vec,
      selected = isolate(input$single_dataset_main %||% character(0)),
      server = TRUE
    )

    updateSelectizeInput(
      session,
      "single_dataset_extra",
      choices = choice_vec,
      selected = isolate(input$single_dataset_extra %||% character(0)),
      server = TRUE
    )
  }, ignoreInit = FALSE)

  observeEvent(input$to_dataset, rv$page <- "dataset")
  observeEvent(input$back_to_start, rv$page <- "start")
  observeEvent(input$back_to_dataset, { rv$final_table <- NULL; rv$page <- "dataset" })
  observeEvent(input$back_to_filters, { rv$final_table <- NULL; rv$page <- "filters" })
  observeEvent(input$back_to_pivot, { rv$final_table <- NULL; rv$page <- "pivot" })
  observeEvent(input$exit_app, stopApp())

  observeEvent(input$reload_formats, {
    formats(load_variable_formats())
    showNotification(
      paste("Formats reloaded from", formats()$file %||% "no file found"),
      type = "message"
    )
  })

  observeEvent(input$to_filters, {
    req(rv$catalogue)
    if (!nrow(rv$catalogue)) {
      showNotification("No datasets are available. Check the configured schema or SQL permissions.", type = "error", duration = 8)
      return()
    }

    req_type <- input$req_type %||% "single"

    plan <- tryCatch(
      build_selection_plan(input, req_type, rv$selected_years),
      error = function(e) {
        showNotification(e$message, type = "error")
        NULL
      }
    )
    if (is.null(plan)) return()

    datasets <- selection_datasets(plan)
    if (!length(datasets) || any(!nzchar(datasets))) {
      showNotification("Choose the required datasets before moving on.", type = "error")
      return()
    }

    rv$selection_plan <- plan

    meta <- NULL
    choices_map <- list()

    if (offline_mode) {
      meta <- get_combined_column_metadata_offline(datasets)
      text_cols <- meta$column_name[vapply(meta$data_type, function(x) normalize_type(x) == "text", logical(1))]
      text_cols <- head(text_cols, MAX_FILTER_TEXT_COLUMNS)

      if (length(text_cols)) {
        for (cl in text_cols) {
          choices_map[[cl]] <- fetch_distinct_values_offline(datasets, cl)
        }
      }
    } else {
      con <- poolCheckout(app_pool)
      on.exit(poolReturn(con), add = TRUE)

      meta <- get_combined_column_metadata_sql(con, datasets)
      text_cols <- meta$column_name[vapply(meta$data_type, function(x) normalize_type(x) == "text", logical(1))]
      text_cols <- head(text_cols, MAX_FILTER_TEXT_COLUMNS)

      if (length(text_cols)) {
        for (cl in text_cols) {
          choices_map[[cl]] <- tryCatch(
            fetch_distinct_values_sql(con, datasets, cl),
            error = function(e) character(0)
          )
        }
      }
    }

    if (is.null(meta) || !nrow(meta)) {
      showNotification("No column metadata could be found for the selected dataset(s). Check the object names and permissions.", type = "error", duration = 8)
      return()
    }

    rv$filter_meta <- meta
    rv$filter_choices <- choices_map
    rv$selected_variables <- meta$column_name
    rv$filter_state <- list()
    rv$filters_open <- NULL
    rv$filtered_row_count <- NULL
    rv$preview_data <- NULL
    rv$formatted_data <- NULL
    rv$pivot_payload <- NULL
    rv$pivot_html_cache <- NULL
    rv$final_table <- NULL
    rv$page <- "filters"
  })

  output$filters_ui <- renderUI({
    req(rv$filter_meta)
    meta <- rv$filter_meta
    search_term <- trimws(tolower(input$filter_var_search %||% ""))

    if (nzchar(search_term)) {
      keep_idx <- vapply(seq_len(nrow(meta)), function(i) {
        hay <- paste(meta$column_name[i], meta$data_type[i], collapse = " ")
        grepl(search_term, tolower(hay), fixed = TRUE)
      }, logical(1))
      meta <- meta[keep_idx, , drop = FALSE]
    }

    if (!nrow(meta)) {
      return(tags$div(class = "alert alert-info", "No variables match the current search."))
    }

    accordion(
      id = "filters_accordion",
      open = isolate(rv$filters_open),
      !!!lapply(seq_len(nrow(meta)), function(i) {
        col <- meta$column_name[i]
        type <- normalize_type(meta$data_type[i])
        idb <- paste0("flt_", make.names(col))
        keep_id <- paste0("var_keep_", make.names(col))

        saved_selected <- isolate(rv$filter_state[[paste0(idb, "_cat")]] %||% character(0))
        saved_contains <- isolate(rv$filter_state[[paste0(idb, "_contains")]] %||% "")
        saved_min <- isolate(rv$filter_state[[paste0(idb, "_min")]] %||% "")
        saved_max <- isolate(rv$filter_state[[paste0(idb, "_max")]] %||% "")
        saved_from <- isolate(rv$filter_state[[paste0(idb, "_from")]] %||% "")
        saved_to <- isolate(rv$filter_state[[paste0(idb, "_to")]] %||% "")
        keep_value <- col %in% (rv$selected_variables %||% rv$filter_meta$column_name)

        filter_controls <- if (type == "text") {
          if (length(rv$filter_choices[[col]] %||% character(0)) > 0) {
            tagList(
              checkboxInput(keep_id, "Include this variable in pivot/export", value = keep_value),
              selectizeInput(
                paste0(idb, "_cat"),
                paste0(col, " values"),
                choices = rv$filter_choices[[col]],
                selected = saved_selected,
                multiple = TRUE,
                options = list(placeholder = "Leave empty for no filter")
              ),
              textInput(paste0(idb, "_contains"), paste0(col, " contains (fallback)"), value = saved_contains)
            )
          } else {
            tagList(
              checkboxInput(keep_id, "Include this variable in pivot/export", value = keep_value),
              textInput(paste0(idb, "_contains"), paste0(col, " contains"), value = saved_contains)
            )
          }
        } else if (type == "numeric") {
          tagList(
            checkboxInput(keep_id, "Include this variable in pivot/export", value = keep_value),
            fluidRow(
              column(6, textInput(paste0(idb, "_min"), paste0(col, " minimum"), value = saved_min)),
              column(6, textInput(paste0(idb, "_max"), paste0(col, " maximum"), value = saved_max))
            )
          )
        } else if (type == "date") {
          tagList(
            checkboxInput(keep_id, "Include this variable in pivot/export", value = keep_value),
            fluidRow(
              column(6, textInput(paste0(idb, "_from"), paste0(col, " from (YYYY-MM-DD)"), value = saved_from)),
              column(6, textInput(paste0(idb, "_to"), paste0(col, " to (YYYY-MM-DD)"), value = saved_to))
            )
          )
        } else {
          tagList(
            checkboxInput(keep_id, "Include this variable in pivot/export", value = keep_value),
            tags$p("No filter control available for this field type.")
          )
        }

        accordion_panel(
          value = paste0("panel_", make.names(col)),
          title = tagList(
            div(
              class = "var-meta-row",
              span(paste0(col, " (", meta$data_type[i], ")")),
              span(class = "small-muted", if (keep_value) "Selected" else "Not selected")
            )
          ),
          filter_controls
        )
      })
    )
  })

  observe({
    req(rv$filter_meta)
    for (i in seq_len(nrow(rv$filter_meta))) {
      col <- rv$filter_meta$column_name[i]
      idb <- paste0("flt_", make.names(col))

      ids <- c(
        paste0(idb, "_cat"),
        paste0(idb, "_contains"),
        paste0(idb, "_min"),
        paste0(idb, "_max"),
        paste0(idb, "_from"),
        paste0(idb, "_to")
      )

      for (id in ids) {
        val <- input[[id]]
        if (!is.null(val)) rv$filter_state[[id]] <- val
      }
    }
  })

  observe({
    req(rv$filter_meta)
    selected_now <- rv$selected_variables
    if (!length(selected_now)) selected_now <- character(0)

    for (col in rv$filter_meta$column_name) {
      id <- paste0("var_keep_", make.names(col))
      keep <- input[[id]]
      if (is.null(keep)) next
      if (isTRUE(keep)) {
        selected_now <- union(selected_now, col)
      } else {
        selected_now <- setdiff(selected_now, col)
      }
    }

    rv$selected_variables <- unique(selected_now)
  })

  observe({
    rv$filters_open <- input$filters_accordion %||% NULL
  })

  output$selected_var_text <- renderText({
    req(rv$filter_meta)
    paste("Selected for pivot/export:", length(rv$selected_variables %||% character(0)), "of", nrow(rv$filter_meta))
  })

  observeEvent(input$select_all_vars, {
    req(rv$filter_meta)
    for (col in rv$filter_meta$column_name) {
      updateCheckboxInput(session, paste0("var_keep_", make.names(col)), value = TRUE)
    }
    rv$selected_variables <- rv$filter_meta$column_name
  })

  observeEvent(input$select_no_vars, {
    req(rv$filter_meta)
    for (col in rv$filter_meta$column_name) {
      updateCheckboxInput(session, paste0("var_keep_", make.names(col)), value = FALSE)
    }
    rv$selected_variables <- character(0)
  })

  observeEvent(input$clear_filters, {
    req(rv$filter_meta)
    for (i in seq_len(nrow(rv$filter_meta))) {
      col <- rv$filter_meta$column_name[i]
      idb <- paste0("flt_", make.names(col))
      rv$filter_state[[paste0(idb, "_cat")]] <- character(0)
      rv$filter_state[[paste0(idb, "_contains")]] <- ""
      rv$filter_state[[paste0(idb, "_min")]] <- ""
      rv$filter_state[[paste0(idb, "_max")]] <- ""
      rv$filter_state[[paste0(idb, "_from")]] <- ""
      rv$filter_state[[paste0(idb, "_to")]] <- ""
      updateSelectizeInput(session, paste0(idb, "_cat"), selected = character(0))
      updateTextInput(session, paste0(idb, "_contains"), value = "")
      updateTextInput(session, paste0(idb, "_min"), value = "")
      updateTextInput(session, paste0(idb, "_max"), value = "")
      updateTextInput(session, paste0(idb, "_from"), value = "")
      updateTextInput(session, paste0(idb, "_to"), value = "")
    }
    rv$filtered_row_count <- NULL
  })

  observeEvent(input$count_rows, {
    req(rv$selection_plan, rv$filter_meta)
    n <- tryCatch(
      {
        if (offline_mode) {
          count_selected_rows_offline(rv$selection_plan, rv$filter_meta, input)
        } else {
          con <- poolCheckout(app_pool); on.exit(poolReturn(con), add = TRUE)
          count_selected_rows_sql(con, rv$selection_plan, rv$filter_meta, input)
        }
      },
      error = function(e) {
        showNotification(paste("Row count failed:", e$message), type = "error", duration = 8)
        NULL
      }
    )

    if (!is.null(n)) rv$filtered_row_count <- as.numeric(n)
  })

  output$row_count_text <- renderText({
    if (is.null(rv$filtered_row_count)) return("Rows matched: click 'Check row count'")
    paste("Rows matched:", format(rv$filtered_row_count, big.mark = ","))
  })

  observeEvent(input$to_pivot, {
    req(rv$selection_plan, rv$filter_meta)
    if (is.null(rv$filtered_row_count)) {
      showNotification("Check the row count first.", type = "warning")
      return()
    }

    max_rows <- as.numeric(input$max_interactive_rows %||% DEFAULT_MAX_INTERACTIVE_ROWS)
    preview_n <- as.integer(input$preview_n %||% DEFAULT_PREVIEW_N)
    use_sample <- rv$filtered_row_count > max_rows

    if (use_sample && !isTRUE(input$preview_if_large)) {
      showNotification("The result is above the threshold. Add more filters or enable sample preview.", type = "error", duration = 8)
      return()
    }

    df <- tryCatch(
      {
        if (offline_mode) {
          fetch_selected_data_offline(
            rv$selection_plan,
            rv$filter_meta,
            input,
            max_rows = if (use_sample) preview_n else max_rows,
            random_sample = use_sample
          )
        } else {
          con <- poolCheckout(app_pool); on.exit(poolReturn(con), add = TRUE)
          fetch_selected_data_sql(
            con,
            rv$selection_plan,
            rv$filter_meta,
            input,
            max_rows = if (use_sample) preview_n else max_rows,
            random_sample = use_sample
          )
        }
      },
      error = function(e) {
        showNotification(paste("Loading filtered data failed:", e$message), type = "error", duration = 8)
        NULL
      }
    )

    if (is.null(df) || !nrow(df)) {
      showNotification("No rows matched the selected filters.", type = "warning")
      return()
    }

    if (isTRUE(input$apply_standard_formats)) {
      df <- apply_variable_formats(df, formats())
    }

    selected_now <- rv$selected_variables %||% character(0)
    if (!length(selected_now)) {
      showNotification("Select at least one variable to carry into the pivot/export.", type = "warning", duration = 8)
      return()
    }

    # Always carry the generated academic-year field into the pivot/export.
    # It is created after SQL retrieval, so it is not shown as a SQL filter field.
    selected_now <- unique(c("year", selected_now))

    df <- subset_for_pivot(df, selected_now)
    rv$selected_variables <- selected_now

    rv$preview_data <- df
    rv$formatted_data <- df
    rv$pivot_payload <- NULL
    rv$pivot_html_cache <- NULL
    rv$final_table <- NULL

    if (use_sample) {
      showNotification(
        paste("Loaded", nrow(df), "rows for the pivot preview. The full filtered result was", format(rv$filtered_row_count, big.mark = ","), "rows."),
        type = "message",
        duration = 8
      )
    }

    rv$page <- "pivot"
  })

  output$pivot_widget <- renderRpivotTable({
    req(rv$formatted_data)
    dat <- sanitize_preview_data(rv$formatted_data)

    row_default <- intersect(c("Region", "Programme", names(dat)[1]), names(dat))[1]
    col_default <- intersect(c("year", "SelectedYear", "Year", names(dat)[2]), names(dat))[1]
    val_default <- intersect(c("Count", "Amount", "Credits", ".RecordCount"), names(dat))
    if (!length(val_default)) {
      numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
      val_default <- numeric_cols[1]
    } else {
      val_default <- val_default[1]
    }
    agg_default <- if (is.null(val_default) || !length(val_default) || is.na(val_default) || !nzchar(val_default)) "Count" else "Sum"
    vals_default <- if (identical(agg_default, "Count")) NULL else val_default

    widget <- rpivotTable(
      data = dat,
      rows = row_default,
      cols = col_default,
      vals = vals_default,
      aggregatorName = agg_default,
      rendererName = "Table"
    )

    htmlwidgets::onRender(widget, paste(
      "function(el, x){",
      "  if (!window.Shiny) return;",
      "  function expandTable(table){",
      "    var rows = Array.from(table.querySelectorAll('tr'));",
      "    var grid = [];",
      "    rows.forEach(function(tr, rIdx){",
      "      if (!grid[rIdx]) grid[rIdx] = [];",
      "      var cIdx = 0;",
      "      Array.from(tr.children).forEach(function(cell){",
      "        while (grid[rIdx][cIdx] !== undefined) cIdx++;",
      "        var rowspan = parseInt(cell.getAttribute('rowspan') || '1', 10);",
      "        var colspan = parseInt(cell.getAttribute('colspan') || '1', 10);",
      "        var text = ((cell.innerText)||'').replace(/\\s+/g,' ').trim();",
      "        for (var r = 0; r < rowspan; r++) {",
      "          for (var c = 0; c < colspan; c++) {",
      "            if (!grid[rIdx + r]) grid[rIdx + r] = [];",
      "            grid[rIdx + r][cIdx + c] = text;",
      "          }",
      "        }",
      "        cIdx += colspan;",
      "      });",
      "    });",
      "    var maxCols = 0;",
      "    grid.forEach(function(row){ if (row && row.length > maxCols) maxCols = row.length; });",
      "    grid = grid.map(function(row){",
      "      row = row || [];",
      "      for (var i = 0; i < maxCols; i++) if (row[i] === undefined) row[i] = '';",
      "      return row;",
      "    });",
      "    return grid;",
      "  }",
      "  function capturePivot(){",
      "    var table = el.querySelector('.pvtTable');",
      "    if (!table) return false;",
      "    var payload = {headers: [], rows: [], matrix: expandTable(table)};",
      "    table.querySelectorAll('thead tr').forEach(function(tr){",
      "      var row = [];",
      "      tr.querySelectorAll('th').forEach(function(th){ row.push(((th.innerText)||'').replace(/\\s+/g,' ').trim()); });",
      "      payload.headers.push(row);",
      "    });",
      "    table.querySelectorAll('tbody tr').forEach(function(tr){",
      "      var row = [];",
      "      tr.querySelectorAll('th,td').forEach(function(td){ row.push(((td.innerText)||'').replace(/\\s+/g,' ').trim()); });",
      "      payload.rows.push(row);",
      "    });",
      "    if (!payload.rows.length && !(payload.matrix && payload.matrix.length)) return false;",
      "    Shiny.setInputValue('pivot_table', payload, {priority:'event'});",
      "    Shiny.setInputValue('pivot_html', table.outerHTML, {priority:'event'});",
      "    return true;",
      "  }",
      "  function schedule(){ setTimeout(function(){ capturePivot(); }, 250); }",
      "  schedule();",
      "  var obs = new MutationObserver(function(){ schedule(); });",
      "  obs.observe(el, {childList:true, subtree:true});",
      "  if (window.jQuery) { window.jQuery(el).on('change', '.pvtAxisContainer, .pvtRenderer, .pvtAggregator, .pvtVals select, .pvtDropdown', function(){ schedule(); }); }",
      "}",      sep = "\n"
    ))
  })
  outputOptions(output, "pivot_widget", suspendWhenHidden = FALSE)

  output$preview_table <- renderDT({
    req(rv$formatted_data)
    datatable(head(sanitize_preview_data(rv$formatted_data), 100), options = list(scrollX = TRUE, pageLength = 10))
  })

  observeEvent(input$pivot_table, {
    payload <- input$pivot_table
    if (length(payload$rows %||% list()) == 0) {
      showNotification("Pivot capture returned an empty table. Keeping the last valid pivot.", type = "warning")
      return()
    }

    rv$pivot_payload <- payload
    rv$final_table <- tryCatch(pivot_table_to_df(payload), error = function(e) NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$pivot_html, {
    rv$pivot_html_cache <- input$pivot_html
  }, ignoreInit = TRUE)

  observeEvent(input$to_export, {
    tbl <- tryCatch({
      req(rv$pivot_payload)
      out <- pivot_table_to_df(rv$pivot_payload)
      if (!nrow(out)) stop("Pivot table is empty.")
      out
    }, error = function(e) {
      showNotification(paste("Could not move to export step:", e$message), type = "error", duration = 8)
      NULL
    })

    if (is.null(tbl)) return()
    rv$final_table <- tbl
    rv$page <- "export"
  })

  observeEvent(input$refresh_final, {
    tbl <- tryCatch({
      req(rv$pivot_payload)
      out <- pivot_table_to_df(rv$pivot_payload)
      if (!nrow(out)) stop("Pivot table is empty.")
      out
    }, error = function(e) {
      showNotification(paste("Pivot export failed:", e$message), type = "error", duration = 8)
      NULL
    })

    if (!is.null(tbl)) rv$final_table <- tbl
  })

  output$final_preview <- renderDT({
    req(rv$final_table)
    df_preview <- round_df_for_export(rv$final_table, input$round_option %||% "none")
    datatable(df_preview, options = list(scrollX = TRUE, pageLength = 15))
  })

  output$download_html <- downloadHandler(
    filename = function() paste0("request_table_", Sys.Date(), ".html"),
    content = function(file) {
      req(rv$final_table)
      title <- if ((input$export_mode %||% "raw") == "format") input$export_title %||% "" else ""
      notes <- if ((input$export_mode %||% "raw") == "format") input$export_notes %||% "" else ""
      df_export <- round_df_for_export(rv$final_table, input$round_option %||% "none")
      table_html <- if (identical(input$round_option %||% "none", "none") && !is.null(rv$pivot_html_cache) && nzchar(rv$pivot_html_cache %||% "")) rv$pivot_html_cache else df_to_basic_html(df_export)
      writeLines(style_download_html(table_html, title = title, notes = notes), file, useBytes = TRUE)
    }
  )

  output$download_csv <- downloadHandler(
    filename = function() paste0("request_table_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$final_table)
      df_export <- round_df_for_export(rv$final_table, input$round_option %||% "none")
      if ((input$export_mode %||% "raw") == "format" && (nzchar(input$export_title %||% "") || nzchar(input$export_notes %||% ""))) {
        header_df <- data.frame(V1 = c(
          if (nzchar(input$export_title %||% "")) paste("Title:", input$export_title),
          if (nzchar(input$export_notes %||% "")) paste("Note codes:", gsub("\n", " | ", input$export_notes)),
          if ((input$round_option %||% "none") == "5") "Rounding: Numeric values rounded to nearest 5",
          ""
        ))
        utils::write.table(header_df, file, sep = ",", row.names = FALSE, col.names = FALSE, quote = TRUE)
        suppressWarnings(write.table(df_export, file, sep = ",", row.names = FALSE, col.names = TRUE, quote = TRUE, append = TRUE))
      } else {
        write.csv(df_export, file, row.names = FALSE)
      }
    }
  )

  output$download_xlsx <- downloadHandler(
    filename = function() paste0("request_table_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(rv$final_table)
      df_export <- round_df_for_export(rv$final_table, input$round_option %||% "none")
      wb <- createWorkbook()
      addWorksheet(wb, "Table")
      r <- 1

      if ((input$export_mode %||% "raw") == "format") {
        if (nzchar(input$export_title %||% "")) {
          writeData(wb, 1, paste("Title:", input$export_title), startRow = r, startCol = 1)
          addStyle(wb, 1, createStyle(textDecoration = "bold", fontSize = 14), rows = r, cols = 1)
          r <- r + 2
        }
      }

      writeData(wb, 1, df_export, startRow = r, startCol = 1)
      addStyle(wb, 1, createStyle(textDecoration = "bold", fgFill = "#D8E7F7"), rows = r, cols = seq_len(ncol(df_export)), gridExpand = TRUE)
      r <- r + nrow(df_export) + 2

      if ((input$export_mode %||% "raw") == "format" && nzchar(input$export_notes %||% "")) {
        writeData(wb, 1, paste("Note codes:", input$export_notes), startRow = r, startCol = 1)
        r <- r + 2
      }
      if ((input$round_option %||% "none") == "5") {
        writeData(wb, 1, "Rounding: Numeric values rounded to nearest 5", startRow = r, startCol = 1)
      }

      setColWidths(wb, 1, cols = seq_len(max(1, ncol(df_export))), widths = "auto")
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)