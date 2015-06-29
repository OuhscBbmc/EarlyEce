



This report was automatically generated with the R package **knitr**
(version 1.10.5).


```r
# knitr::stitch_rmd(script="./manipulation/headstart.R", output="./manipulation/stitched_output/headstart.md")

#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
```


```r
library(magrittr)
library(tidyr)
library(ggplot2)
library(NlsyLinks) # devtools::install_github("LiveOak/NlsyLinks")
requireNamespace("readr", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("lubridate", quietly=TRUE)
# requireNamespace("plyr", quietly=TRUE)
```

```r
path_input <- "./data_phi_free/raw/ece-headstart.csv"
# path_item <- "./data_phi_free/meta/item.csv"
path_variable <- "./data_phi_free/derived/variable.csv"
path_output <- "./data_phi_free/derived/headstart.rds"

default_day_of_month <- 15L
```

```r
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
```

```
##  [1] "C0000100" "C0000200" "C0005300" "C0005400" "C0005500" "C0005700"
##  [7] "C0592000" "C0592200" "C0592500" "C0811400" "C0811600" "C0811700"
## [13] "C0811800" "C1001400" "C1001500" "C1001600" "C1001700" "C1001800"
## [19] "C1205000" "C1205100" "C1205200" "C1205300" "C1205400" "C1525000"
## [25] "C1525100" "C1525200" "C1525300" "C1525400" "C1771400" "C1771500"
## [31] "C1771600" "C1771700" "C1771800" "C2244400" "C2244600" "C2244700"
## [37] "C2244800" "C2244900" "C2245000" "C2245100" "C2687900" "C2688000"
## [43] "C2688100" "C2688200" "C2688300" "C2688400" "C2688500" "C2688600"
## [49] "C2688700" "C2969400" "C2969500" "C2969600" "C2969700" "C2969800"
## [55] "C2969900" "C2970000" "C3549400" "C3549500" "C3549600" "C3549700"
## [61] "C3549800" "C3549900" "C3550000" "C3893800" "C3893900" "C3894000"
## [67] "C3894100" "C3894200" "C3894300" "C3894400" "C5142800" "C5142900"
## [73] "C5143000" "C5143100" "C5143200" "C5143300" "C5143400" "C5720000"
## [79] "C5720200" "C5720300" "C5720400" "C5720500" "C5720600" "C5720700"
```

```r
colnames(ds_wide) <- plyr::mapvalues(colnames(ds_wide), from=ds_variable$variable_code, to=ds_variable$column_name_wide)

# Recode Gen2 missing values
ds_wide[ds_wide == -1L] <- NA_integer_  # Refused
ds_wide[ds_wide == -2L] <- NA_integer_  # Don't know
ds_wide[ds_wide == -3L] <- NA_integer_  # Invalid missing
ds_wide[ds_wide == -7L] <- NA_integer_  # Missing

sapply(ds_wide, class)
```

```
##            subject_id             mother_id                  race 
##             "integer"             "integer"             "integer" 
##                gender              C0005500              C0005700 
##             "integer"             "integer"             "integer" 
## headstart_ever_y_1988              C0592200              C0592500 
##             "integer"             "integer"             "integer" 
## headstart_ever_y_1990              C0811600              C0811700 
##             "integer"             "integer"             "integer" 
##              C0811800 headstart_ever_y_1992              C1001500 
##             "integer"             "integer"             "integer" 
##              C1001600              C1001700              C1001800 
##             "integer"             "integer"             "integer" 
## headstart_ever_y_1994              C1205100              C1205200 
##             "integer"             "integer"             "integer" 
##              C1205300              C1205400 headstart_ever_y_1996 
##             "integer"             "integer"             "integer" 
##              C1525100              C1525200              C1525300 
##             "integer"             "integer"             "integer" 
##              C1525400 headstart_ever_y_1998              C1771500 
##             "integer"             "integer"             "integer" 
##              C1771600              C1771700              C1771800 
##             "integer"             "integer"             "integer" 
##              C2244400              C2244600 headstart_ever_y_2000 
##             "integer"             "integer"             "integer" 
##              C2244800              C2244900              C2245000 
##             "integer"             "integer"             "integer" 
##              C2245100              C2687900              C2688000 
##             "integer"             "integer"             "integer" 
##              C2688100              C2688200 headstart_ever_y_2002 
##             "integer"             "integer"             "integer" 
##              C2688400              C2688500              C2688600 
##             "integer"             "integer"             "integer" 
##              C2688700              C2969400              C2969500 
##             "integer"             "integer"             "integer" 
## headstart_ever_y_2004              C2969700              C2969800 
##             "integer"             "integer"             "integer" 
##              C2969900              C2970000              C3549400 
##             "integer"             "integer"             "integer" 
##              C3549500 headstart_ever_y_2006              C3549700 
##             "integer"             "integer"             "integer" 
##              C3549800              C3549900              C3550000 
##             "integer"             "integer"             "integer" 
##              C3893800              C3893900 headstart_ever_y_2008 
##             "integer"             "integer"             "integer" 
##              C3894100              C3894200              C3894300 
##             "integer"             "integer"             "integer" 
##              C3894400              C5142800              C5142900 
##             "integer"             "integer"             "integer" 
## headstart_ever_y_2010              C5143100              C5143200 
##             "integer"             "integer"             "integer" 
##              C5143300              C5143400              C5720000 
##             "integer"             "integer"             "integer" 
##              C5720200 headstart_ever_y_2012              C5720400 
##             "integer"             "integer"             "integer" 
##              C5720500              C5720600              C5720700 
##             "integer"             "integer"             "integer"
```

```r
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
```

```r
ds_subject <- ds_long %>%
  dplyr::group_by_("subject_id") %>%
  dplyr::summarize(
    ever_count = sum(headstart_ever, na.rm=T),
    any_yes = any(headstart_ever),
    any_response = any(!is.na(headstart_ever))
  )
table(ds_subject$ever_count, useNA="always")
```

```
## 
##    0    1    2    3 <NA> 
## 9541 1289  560  122    0
```

```r
sum(ds_subject$any_yes, na.rm=T)
```

```
## [1] 1971
```

```r
sum(ds_subject$any_response, na.rm=T)
```

```
## [1] 9410
```


```r
# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
# saveRDS(ds_wide, file=path_output, compress="xz")
```

The R session information (including the OS info, R version and all
packages used):


```r
sessionInfo()
```

```
## R version 3.2.1 Patched (2015-06-18 r68542)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 8 x64 (build 9200)
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] tidyr_0.2.0     NlsyLinks_1.407 ggplot2_1.0.1   magrittr_1.5   
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.11.6      knitr_1.10.5     xml2_0.1.1       devtools_1.8.0  
##  [5] MASS_7.3-41      munsell_0.4.2    colorspace_1.2-6 R6_2.0.1        
##  [9] stringr_1.0.0    plyr_1.8.3       dplyr_0.4.2      tools_3.2.1     
## [13] parallel_3.2.1   grid_3.2.1       gtable_0.1.2     DBI_0.3.1       
## [17] git2r_0.10.1     rversions_1.0.1  lazyeval_0.1.10  digest_0.6.8    
## [21] assertthat_0.1   formatR_1.2      readr_0.1.1      reshape2_1.4.1  
## [25] curl_0.9         evaluate_0.7     memoise_0.2.1    stringi_0.5-2   
## [29] scales_0.2.5     lubridate_1.3.3  markdown_0.7.7   proto_0.3-10
```

```r
Sys.time()
```

```
## [1] "2015-06-28 22:17:08 CDT"
```

