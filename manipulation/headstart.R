# knitr::stitch_rmd(script="./manipulation/groom_cars.R", output="./manipulation/stitched_output/groom_cars.md")

#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

# @knitr load_sources ==============================

# @knitr load_packages ==============================
library(magrittr)
library(ggplot2)
requireNamespace("readr", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("lubridate", quietly=TRUE)
# requireNamespace("plyr", quietly=TRUE)

# @knitr declare_globals ==============================
path_input <- "./data_phi_free/raw/ece-headstart.csv"
path_item <- "./data_phi_free/meta/item.csv"
path_variable <- "./data_phi_free/meta/variable.csv"
path_output <- "./data_phi_free/derived/headstart.rds"

default_day_of_month <- 15L

# @knitr load_data ==============================
ds_item <- readr::read_csv(path_item)
ds_variable <- readr::read_csv(path_variable)
ds <- readr::read_csv(path_input)

# Dataset description can be found at: http://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html
ds <- dplyr::rename_(ds,
  "subject_id"      = "C0000100"
  , "mother_id"     = "C0000200"
  , "race"          = "C0005300"
  , "gender"        = "C0005400"
  , "dob_month"     = "C0005500"
  , "dob_year"      = "C0005700"
)
colnames(ds)

# @knitr tweak_data ==============================

# Recode Gen2 missing values
ds[ds == -1L] <- NA_integer_  # Refused
ds[ds == -2L] <- NA_integer_  # Don't know
ds[ds == -3L] <- NA_integer_  # Invalid missing
ds[ds == -7L] <- NA_integer_  # Missing

ds$mob <- as.Date(ISOdate(ds$dob_year, ds$dob_month, default_day_of_month))
ds$dob_year <- NULL
ds$dob_month <- NULL

sapply(ds, class)
# @knitr erase_artifacts ==============================

# @knitr save_to_disk ==============================
# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(ds, file=path_output, compress="xz")
