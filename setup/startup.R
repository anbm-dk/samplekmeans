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

# use_gpl3_license()

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


# Check results:

# '::' or ':::' imports not declared from:
#   'ClusterR' 'dplyr' 'fields' 'tidyr'
# 'library' or 'require' calls not declared from:
#   'ClusterR' 'fields' 'future.apply' 'magrittr' 'tools'
# 'library' or 'require' calls in package code:
#   'ClusterR' 'fields' 'future.apply' 'magrittr' 'tools'
# Please use :: or requireNamespace() instead.
# See section 'Suggested packages' in the 'Writing R Extensions' manual.
#
# ❯ checking R code for possible problems ... NOTE
# sample_kmeans: no visible global function definition for '%<>%'
# sample_kmeans : map_clusters_fun: no visible global function definition
# for '%<>%'
# sample_kmeans: no visible binding for global variable 'ID'
# Undefined global functions or variables:
#   %<>% ID

# END
