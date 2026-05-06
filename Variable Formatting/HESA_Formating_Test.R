# ============================================================
# HESA Legacy + Data Futures variable formats from local files
# Reads mapping workbooks from your Downloads folder
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(janitor)
library(writexl)

# ------------------------------------------------------------
# 1) SET YOUR DOWNLOADS FOLDER
# ------------------------------------------------------------
# Change this if your Downloads folder is in a different place
downloads_dir <- file.path("R Script\\Variable Formatting\\downloads\\")

# ------------------------------------------------------------
# 2) SET YOUR FILE NAMES
# ------------------------------------------------------------
# Replace these with the EXACT filenames you downloaded
files <- c(
  field_level         = file.path(downloads_dir, "legacy_to_df_field_level_mapping_f_22.xlsx"),
  field_level_derived = file.path(downloads_dir, "legacy_to_df_field_level_mapping_derived_f_22.xlsx"),
  entry_value         = file.path(downloads_dir, "legacy_to_df_field_entry_value_mapping_f_22.xlsx"),
  entry_value_derived = file.path(downloads_dir, "legacy_to_df_field_entry_value_mapping_derived_f_22.xlsx")
)

# ------------------------------------------------------------
# 3) OUTPUT FOLDER
# ------------------------------------------------------------
out_dir <- file.path(downloads_dir, "hesa_legacy_to_datafutures_outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

raw_csv_dir <- file.path(out_dir, "raw_csv_exports")
if (!dir.exists(raw_csv_dir)) dir.create(raw_csv_dir, recursive = TRUE)

# ------------------------------------------------------------
# 4) HELPER FUNCTIONS
# ------------------------------------------------------------
read_all_sheets <- function(path, workbook_name) {
  sheets <- excel_sheets(path)

  map(sheets, function(s) {
    df <- read_excel(path, sheet = s) %>%
      clean_names()

    df$source_workbook <- workbook_name
    df$source_sheet <- s
    df
  }) %>%
    set_names(sheets)
}

coalesce_cols <- function(df, patterns) {
  hits <- names(df)[str_detect(names(df), str_c(patterns, collapse = "|"))]

  if (length(hits) == 0) {
    return(rep(NA_character_, nrow(df)))
  }

  out <- as.character(df[[hits[1]]])

  if (length(hits) > 1) {
    for (i in 2:length(hits)) {
      out <- dplyr::coalesce(out, as.character(df[[hits[i]]]))
    }
  }

  out
}

standardise_field_mapping <- function(df) {
  tibble(
    legacy_field       = coalesce_cols(df, c("^legacy.*field$", "^legacy_field$", "legacy.*item", "legacy.*name")),
    legacy_description = coalesce_cols(df, c("legacy.*description", "legacy.*desc")),
    legacy_entity      = coalesce_cols(df, c("legacy.*entity")),
    legacy_format      = coalesce_cols(df, c("legacy.*format", "legacy.*type", "legacy.*field_type", "legacy.*data_type", "legacy.*datatype", "legacy.*length")),
    df_field           = coalesce_cols(df, c("^df.*field$", "^data_futures.*field$", "data.*futures.*field", "student.*field", "new.*field")),
    df_description     = coalesce_cols(df, c("^df.*description$", "data.*futures.*description", "new.*description", "student.*description")),
    df_entity          = coalesce_cols(df, c("^df.*entity$", "data.*futures.*entity", "student.*entity")),
    df_format          = coalesce_cols(df, c("^df.*format$", "data.*futures.*format", "df.*type", "df.*field_type", "df.*data_type", "df.*datatype", "df.*length")),
    notes              = coalesce_cols(df, c("note", "notes", "comment", "comments", "guidance")),
    source_workbook    = df$source_workbook,
    source_sheet       = df$source_sheet
  ) %>%
    filter(if_any(everything(), ~ !is.na(.) & . != ""))
}

standardise_value_mapping <- function(df) {
  tibble(
    legacy_field       = coalesce_cols(df, c("^legacy.*field$", "legacy.*item", "legacy.*name")),
    legacy_value       = coalesce_cols(df, c("^legacy.*value$", "legacy.*code", "legacy.*entry")),
    legacy_value_label = coalesce_cols(df, c("legacy.*label", "legacy.*description", "legacy.*desc")),
    df_field           = coalesce_cols(df, c("^df.*field$", "^data_futures.*field$", "data.*futures.*field", "student.*field", "new.*field")),
    df_value           = coalesce_cols(df, c("^df.*value$", "df.*code", "data.*futures.*value", "new.*value", "entry.*value")),
    df_value_label     = coalesce_cols(df, c("^df.*label$", "^df.*description$", "data.*futures.*label", "data.*futures.*description", "new.*description")),
    notes              = coalesce_cols(df, c("note", "notes", "comment", "comments", "guidance")),
    source_workbook    = df$source_workbook,
    source_sheet       = df$source_sheet
  ) %>%
    filter(if_any(everything(), ~ !is.na(.) & . != ""))
}

# ------------------------------------------------------------
# 5) CHECK FILES EXIST
# ------------------------------------------------------------
file_check <- tibble(
  workbook = names(files),
  path = unname(files),
  exists = file.exists(unname(files))
)

print(file_check)

if (!all(file_check$exists)) {
  stop(
    "One or more files were not found. Check the filenames in the 'files' section."
  )
}

# ------------------------------------------------------------
# 6) READ ALL WORKBOOKS
# ------------------------------------------------------------
all_workbooks <- imap(files, function(path, nm) {
  read_all_sheets(path, workbook_name = nm)
})

# ------------------------------------------------------------
# 7) EXPORT RAW SHEETS TO CSV
# ------------------------------------------------------------
for (wb_name in names(all_workbooks)) {
  wb <- all_workbooks[[wb_name]]

  for (sheet_name in names(wb)) {
    clean_sheet_name <- str_replace_all(sheet_name, "[^A-Za-z0-9_]+", "_")
    out_file <- file.path(raw_csv_dir, paste0(wb_name, "__", clean_sheet_name, ".csv"))
    write.csv(wb[[sheet_name]], out_file, row.names = FALSE, na = "")
  }
}

# ------------------------------------------------------------
# 8) BUILD COMBINED TABLES
# ------------------------------------------------------------
field_level_tables <- c("field_level", "field_level_derived")
value_level_tables <- c("entry_value", "entry_value_derived")

combined_field_level <- map(field_level_tables, function(nm) {
  if (!nm %in% names(all_workbooks)) return(NULL)
  bind_rows(all_workbooks[[nm]])
}) %>%
  bind_rows() %>%
  standardise_field_mapping() %>%
  distinct()

combined_value_level <- map(value_level_tables, function(nm) {
  if (!nm %in% names(all_workbooks)) return(NULL)
  bind_rows(all_workbooks[[nm]])
}) %>%
  bind_rows() %>%
  standardise_value_mapping() %>%
  distinct()

# ------------------------------------------------------------
# 9) SPLIT INTO LEGACY AND DATA FUTURES VARIABLE FORMATS
# ------------------------------------------------------------
legacy_variable_formats <- combined_field_level %>%
  transmute(
    variable_system = "Legacy",
    variable_name   = legacy_field,
    description     = legacy_description,
    entity          = legacy_entity,
    format_or_type  = legacy_format,
    mapped_to       = df_field,
    notes           = notes,
    source_workbook,
    source_sheet
  ) %>%
  filter(!is.na(variable_name) & variable_name != "") %>%
  distinct()

datafutures_variable_formats <- combined_field_level %>%
  transmute(
    variable_system = "Data Futures",
    variable_name   = df_field,
    description     = df_description,
    entity          = df_entity,
    format_or_type  = df_format,
    mapped_from     = legacy_field,
    notes           = notes,
    source_workbook,
    source_sheet
  ) %>%
  filter(!is.na(variable_name) & variable_name != "") %>%
  distinct()

# ------------------------------------------------------------
# 10) SAVE OUTPUTS
# ------------------------------------------------------------
write.csv(
  combined_field_level,
  file.path(out_dir, "combined_field_level_mapping.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  combined_value_level,
  file.path(out_dir, "combined_entry_value_mapping.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  legacy_variable_formats,
  file.path(out_dir, "legacy_variable_formats.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  datafutures_variable_formats,
  file.path(out_dir, "datafutures_variable_formats.csv"),
  row.names = FALSE,
  na = ""
)

write_xlsx(
  list(
    combined_field_level         = combined_field_level,
    combined_entry_value_mapping = combined_value_level,
    legacy_variable_formats      = legacy_variable_formats,
    datafutures_variable_formats = datafutures_variable_formats
  ),
  path = file.path(out_dir, "hesa_legacy_to_datafutures_mapping_outputs.xlsx")
)

# ------------------------------------------------------------
# 11) SUMMARY
# ------------------------------------------------------------
cat("\nDone.\n")
cat("Outputs saved to:\n", out_dir, "\n\n")
cat("Files created:\n")
cat("- combined_field_level_mapping.csv\n")
cat("- combined_entry_value_mapping.csv\n")
cat("- legacy_variable_formats.csv\n")
cat("- datafutures_variable_formats.csv\n")
cat("- hesa_legacy_to_datafutures_mapping_outputs.xlsx\n")
cat("- raw_csv_exports folder\n")
