# knitr::stitch_rmd(script="./manipulation/groom_cars.R", output="./manipulation/stitched_output/groom_cars.md")

#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

# @knitr load_sources ==============================

# @knitr load_packages ==============================
library(magrittr)
library(NlsyLinks) #devtools::install_github("LiveOak/NlsyLinks")
requireNamespace("readr")
requireNamespace("dplyr")

# @knitr declare_globals ==============================
path_item <- "./data_phi_free/meta/item.csv"
path_variable <- "./data_phi_free/meta/variable.csv"
path_extract_source <- "./data_phi_free/meta/extract_source.csv"
path_output <- "./data_phi_free/derived/variable.csv"


# @knitr load_data ==============================
ds_item <- readr::read_csv(path_item)
ds_variable <- readr::read_csv(path_variable)
ds_extract_source <- readr::read_csv(path_extract_source)

# @knitr tweak_data ==============================

ds_item <- ds_item %>%
  dplyr::select_(
    "item_id"
    , "item"           = "label"
  )

ds_extract_source <- ds_extract_source %>%
  dplyr::select_(
    "extract_source_id"
    , "extract_source"           = "label"
  )

ds <- ds_variable %>%
  dplyr::left_join(ds_item, by="item_id") %>%
  dplyr::left_join(ds_extract_source, by="extract_source_id") %>%
  dplyr::filter(
    generation == 2
  ) %>%
  dplyr::mutate(
    column_name_wide = ifelse(survey_year > 0,
                              paste0(item, "_y_", survey_year),
                              item)
  )

ds$translate <- NULL
sapply(ds, class)
# @knitr erase_artifacts ==============================

# @knitr save_to_disk ==============================
readr::write_csv(ds, path=path_output)
