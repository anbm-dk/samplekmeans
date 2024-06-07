# Make description

library(desc)
library(usethis)

use_gpl3_license()

x <- desc(file = "DESCRIPTION")

# Title and description

x$set(
  Package = "samplekmeans",
  Title = "Sampling with k-means",
  Description = "Functions to sample datasets based on the k-means algorithm.",
  URL = "https://github.com/anbm-dk/samplekmeans",
  BugReports = "https://github.com/anbm-dk/samplekmeans/issues"
)

# Set version

x$set_version("0.1.0")

# Set dependencies

x$set_dep("terra")
x$set_dep("magrittr")
x$set_dep("foreach")
x$set_dep("methods")
x$set_dep("ClusterR")
x$set_dep("stats")
x$set_dep("dplyr")
x$set_dep("fields")
x$set_dep("tidyr")
x$set_dep("future.apply")
x$set_dep("tools")

# Set authors

x$del("Authors@R")

x$add_author(
  given = "Anders Bjørn",
  family = "Møller",
  email = "anbm@agro.au.dk",
  orcid = "0000-0002-2737-5780",
  role = c("aut", "cre"),
)

x$get_authors()

# Last check

x

# Write description

x$write(file = "DESCRIPTION")

# END
