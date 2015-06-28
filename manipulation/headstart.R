# knitr::stitch_rmd(script="./manipulation/groom_cars.R", output="./manipulation/stitched_output/groom_cars.md")

#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

# @knitr load_sources ==============================

# @knitr load_packages ==============================
library(ggplot2)

# @knitr declare_globals ==============================
path_input <- "./data_phi_free/raw/ece-headstart.csv"
path_item <- "./data_phi_free/meta/item.csv"
path_variable <- "./data_phi_free/meta/variable.csv"
path_output <- "./data_phi_free/derived/headstart.rds"

# @knitr load_data ==============================
ds_item <- readr::read_csv(path_item)
ds_variable <- readr::read_csv(path_variable)
ds <- readr::read_csv(path_input)

# Dataset description can be found at: http://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html

colnames(ds)

# @knitr tweak_data ==============================

# @knitr erase_artifacts ==============================

# @knitr save_to_disk ==============================
# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(ds, file=path_output, compress="xz")
