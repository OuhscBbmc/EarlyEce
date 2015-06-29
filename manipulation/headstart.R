# knitr::stitch_rmd(script="./manipulation/headstart.R", output="./manipulation/stitched_output/headstart.md")

#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

# @knitr load_sources ------------------------------

# @knitr load_packages ------------------------------
library(magrittr)
library(tidyr)
library(ggplot2)
library(NlsyLinks) # devtools::install_github("LiveOak/NlsyLinks")
requireNamespace("readr", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("lubridate", quietly=TRUE)
# requireNamespace("plyr", quietly=TRUE)

# @knitr declare_globals ------------------------------
path_input <- "./data_phi_free/raw/ece-headstart.csv"
# path_item <- "./data_phi_free/meta/item.csv"
path_variable <- "./data_phi_free/derived/variable.csv"
path_output <- "./data_phi_free/derived/headstart.rds"

default_day_of_month <- 15L

# @knitr load_data ------------------------------
# ds_item <- readr::read_csv(path_item)
ds_variable <- readr::read_csv(path_variable)
ds_wide <- readr::read_csv(path_input)

# ds_wide <- dplyr::rename_(ds_wide,
#   "subject_id"      = "C0000100"
#   , "mother_id"     = "C0000200"
#   , "race"          = "C0005300"
#   , "gender"        = "C0005400"
# )
colnames(ds_wide)

# @knitr tweak_data ------------------------------
colnames(ds_wide) <- plyr::mapvalues(colnames(ds_wide), from=ds_variable$variable_code, to=ds_variable$column_name_wide)

# Recode Gen2 missing values
ds_wide[ds_wide == -1L] <- NA_integer_  # Refused
ds_wide[ds_wide == -2L] <- NA_integer_  # Don't know
ds_wide[ds_wide == -3L] <- NA_integer_  # Invalid missing
ds_wide[ds_wide == -7L] <- NA_integer_  # Missing

sapply(ds_wide, class)
# @knitr elongate ------------------------------
headstart_ever_columns <- grep( "^headstart_ever_y_\\d{4}$", colnames(ds_wide), perl=T, value=T)
desired_columns <- c("subject_id", headstart_ever_columns)
ds_wide <- ds_wide[, desired_columns]

ds_long <- ds_wide %>%
  tidyr::gather_(key="year", value="headstart_ever", headstart_ever_columns) %>%
  dplyr::arrange_(c("subject_id", "year"))#%>%
#   dplyr::mutate(
#     year <-
#
#   )

ds_long$year <- as.integer(gsub("^headstart_ever_y_(\\d{4})$", "\\1", ds_long$year))
ds_long$headstart_ever <- as.logical(ds_long$headstart_ever)

# @knitr collapse ------------------------------
ds_subject <- ds_long %>%
  dplyr::group_by_("subject_id") %>%
  dplyr::summarize(
    ever_count = sum(headstart_ever, na.rm=T),
    any_yes = any(headstart_ever),
    any_response = any(!is.na(headstart_ever))
  )
table(ds_subject$ever_count)
sum(ds_subject$any_yes, na.rm=T)
sum(ds_subject$any_response, na.rm=T)

# @knitr erase_artifacts ------------------------------

# @knitr save_to_disk ------------------------------
# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
# saveRDS(ds_wide, file=path_output, compress="xz")
