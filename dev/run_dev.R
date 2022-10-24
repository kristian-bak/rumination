# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app()

## Document
devtools::document()

## Load all functions
devtools::load_all()

## Bump version
usethis::use_version()

usethis::use_news_md()

usethis::use_package("purrr")
usethis::use_package("tidyr")
