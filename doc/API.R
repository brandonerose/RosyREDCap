## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
wl <- function(x){length(which(x))}
REDCap_API <- RosyREDCap:::REDCap_API

## -----------------------------------------------------------------------------
#knitr::kable(REDCap_API$methods)

