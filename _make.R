#! /usr/bin/env Rscript
usethis::ui_done("Starting targets pipeline")

targets::tar_make()

usethis::ui_done("Finished targets pipeline")
