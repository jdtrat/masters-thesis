library(targets)
library(tarchetypes)
# library(stantargets)

source("R/utils-sborg.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "fs", "qs", "here"))


# SBORG Task Data ------------------------------------------------


sborg_task_metadata <- list(
  tar_target(
    sborg_prospects,
    sbg_gen_possible_prospects(),
    format = "qs"
  )
)

sborg_task_data <- list(
  tar_target(
    sborg_files,
    fs::dir_ls("data/nonpd/",
      recurse = TRUE,
      regexp = "*.sborg.\\d+.nosessionid.*\\d.txt"
    ),
    format = "file"
  ),
  tar_target(
    sborg_raw,
    sbg_read_data(sborg_files),
    format = "qs"
  ),
  tar_target(
    sborg_processed,
    sbg_process_raw_data(sborg_raw),
    format = "qs"
  )
)


# Thesis Documents --------------------------------------------------------


reports <- list(
  tar_map(
    tar_render(
      main_thesis,
      path = "thesis-documents/main.Rmd",
      output_format = format,
    ),
    values = tibble::tibble(format = c("bookdown::pdf_document2",
                                       "bookdown::html_document2",
                                       "bookdown::word_document2"),
                            suffix = c("pdf", "html", "word")),
    names = "suffix"
  )
)


list(
  sborg_task_metadata,
  sborg_task_data,
  reports
)

