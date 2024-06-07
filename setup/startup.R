# Setup

# install.packages(
#   c(
#     "devtools",
#     "roxygen2",
#     "testthat",
#     "knitr",
#     "pkgload")
# )

library(devtools)
library(usethis)

use_devtools()

## 1: Github stuff

# use_git_config(
#   user.name = "Anders Bjørn Møller"
#   , user.email = "perserkongen@gmail.com"
# )

# use_git()

R.version.string

# update.packages(ask = FALSE, checkBuilt = TRUE)

# usethis::git_default_branch_configure()

# usethis::create_github_token()

# gitcreds::gitcreds_set() # For personal access token

## 2: Other stuff

# usethis::use_readme_rmd()  # Creates and rmd for the readme, adds it to buildignore

# usethis::use_news_md()

# usethis::use_description()

devtools::build_readme()  # Renders the readme

usethis::use_build_ignore(
  c("setup", "README_files")
)

use_pipe(export = TRUE)  # Make magrittr pipes available to users

devtools::document()

# devtools::run_examples()

Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)

devtools::check(document = FALSE)

devtools::install()

# END
