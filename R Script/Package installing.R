dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE, showWarnings = FALSE)
install.packages("languageserver", repos = "https://cran.r-project.org/", lib = Sys.getenv("R_LIBS_USER"))

install.packages(c("shiny", "bslib", "DBI", "odbc", "pool", "dplyr", "purrr", "stringr", "tidyr", "jsonlite", "openxlsx", "DT", "htmltools"))

install_archived_cran <- function(pkg, version, repos = "https://cloud.r-project.org") {
  # Ensure dependencies come from a normal CRAN mirror
  options(repos = c(CRAN = repos))
  
  # Install dependencies first (best effort)
  # Note: available.packages() won't include archived pkg, but will help for dependencies
  # You may still need to manually install archived dependencies if any are also archived.
  
  archive_url <- sprintf(
    "https://cran.r-project.org/src/contrib/Archive/%s/%s_%s.tar.gz",
    pkg, pkg, version
  )
  
  message("Installing archived package from: ", archive_url)
  install.packages(archive_url, repos = NULL, type = "source")
}

# Example for rpivotTable (CRAN shows version 0.3.0)
install_archived_cran("rpivotTable", "0.3.0")