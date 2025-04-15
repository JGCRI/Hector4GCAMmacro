# Description: Set up a consistent # Read in R packages and define project constants

# 0. Environment Set Up --------------------------------------------------------
# Start from a clean environment
remove(list = ls())

#remotes::install_github("jgcri/hector", force = TRUE)

# Load the packages
library(assertthat)
library(dplyr)
library(here)
library(tidyr)
library(hector)


# TODO probably use a package manager but for now this is probably good enough
stopifnot(packageVersion("assertthat") == "0.2.1")
stopifnot(packageVersion("dplyr") == "1.1.4")
stopifnot(packageVersion("here") == "1.0.1")
stopifnot(packageVersion("tidyr") == "1.3.1")
stopifnot(packageVersion("hector") == "3.2.0")


# TODO packages that are probably not going to be required but could be helpful during
# the developmental stage.
library(ggplot2)
theme_set(theme_bw())


