# This script sets up a renv for the project and installs required packages

# We are using the daily CRAN snapshots from RStudio Package Manager: 
# https://packagemanager.rstudio.com/client/#/repos/1/overview
# Currently, we are using the snapshot from May 1, 2023:
# https://packagemanager.posit.co/cran/2023-08-09

# Select the repo snapshot:
options(repos = c(
  REPO_NAME = "https://packagemanager.posit.co/cran/2023-08-09"
  ))

# Install renv
install.packages("renv")

# Initialize renv for the project
# bare = TRUE: instead of installing dependencies automatically, we install packages manually
renv::init(bare = TRUE)

# Install the packages
install.packages(c(
  "tidyverse", "dplyr", "stringr", "readr", "ggplot2",
  "rvest", "sf", "png", "gifski"
  ))

install.packages("https://cran.r-project.org/src/contrib/Archive/gganimate/gganimate_1.0.7.tar.gz", repos = NULL, type = "source")
install.packages("https://cran.r-project.org/src/contrib/Archive/transformr/transformr_0.1.3.tar.gz", repos = NULL, type = "source")

# Take a snapshot of the renv
renv::snapshot()
