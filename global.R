#---------------------------------- Libraries ----------------------------------

message(paste0("---------------------- App started [", Sys.time(),"] ----------------------"))

suppressPackageStartupMessages({
  library(tidyverse) # data wrangling
  library(shiny) # app
  library(shinydashboard) # add sidebar to UI
  library(shinyWidgets) # better widgets than Shiny standard
  library(shinyBS) # UI
  library(shinybusy) # loading modals
  library(bsplus) # UI
  library(ggpubr) # plots
  library(Cairo) # graphics
  library(DT) # tables
  library(ComplexHeatmap) # heatmap
  library(markdown) # display markdown documents
  library(shinyjs) # JS support
  library(V8) # JS add-on
  library(sendmailR) # bug reports and feature requests
  library(docstring) # Roxygen style docstrings
})

options(shiny.usecairo = T) # better graphics!

#----------------------------------- Setup -------------------------------------

maintainer <- "james.swift"
app_email <- "rna-web-app@organisation.com" # bug reports & feature requests will get sent here
application_path <- "/srv/shiny-server/rna-web-app/"

#------------------------------------ RDS --------------------------------------

Sys.setenv(RDS_USER = "") # TODO set in operating system for deployment
Sys.setenv(RDS_PASS = "") # TODO  set in operating system for deployment
RDS_user <- Sys.getenv("RDS_USER")
RDS_pass <- Sys.getenv("RDS_PASS")

path <- paste0(application_path, "scripts/sql_rnaseq_fetcher/")
source(paste0(path, "initialize.R"))
initialize(path)
connect(host = "db.rds.amazonaws.com",
                      dbname = "db",
                      user = RDS_user,
                      password = RDS_pass)

#---------------------------------- Metadata -----------------------------------

# load list of available genes from RDS
gene_names <- gene_symbols()
gene_ids <- ensembl_ids()

