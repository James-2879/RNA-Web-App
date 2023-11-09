
### ---- Functions

make_reqs <- function(path) {
  #` Save currently loaded package names and versions.
  #'
  #' Refer to repository documentation for usage.
  packages <- sessioninfo::package_info() %>% 
    filter(attached) %>% 
    select(package, loadedversion)
  rownames(packages) <- NULL
  write_tsv(packages, file = path)
}

install_reqs <- function(path) {
  #' Install packages and associated versions from file.
  #' 
  #' Refer to repository documentation for usage.
  df <- read.delim(path)
  failed_packages <- c()
  for (x in 1:dim(df)[1]) {
    package = df[x, 1]
    version = df[x, 2]
    tryCatch(
      {
        devtools::install_version(package = package, version = version)
      },
      error = function(msg) {
        message(msg)
        failed_packages <- c(failed_packages, package)
      }
    )
  }
  if (length(failed_packages) > 0) {
    message("These packages failed to install")
    message("Possibly due to being unavailable in CRAN")
    message(failed_packages)
  }
}

### ---- Usage

make_reqs(path = "/srv/shiny-server/rna-web-app/reqs.tsv")

# if you are about to install specific package versions,
# you probably want to do this in an R virtual environment
# so you don't wreck your main package library

# if you do this you will need to call `renv::init()`
# at the top of the app global.R file

# library(renv)
# renv::init()

install_reqs(path = "/srv/shiny-server/rna-web-app/reqs.tsv")

# renv::snapshot()

