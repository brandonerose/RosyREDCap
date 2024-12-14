
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RosyREDCap <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Rosyverse

Rosyverse is an evolving pre-CRAN set of packages maintained by Brandon
Rose, MD, MPH. It is important to continue to run
`Rosyverse::update_all()` while you are using this set of related
packages. Please out if there are issues with installation,
documentation, or packages.

``` r
# install remotes package if you don't have it
# install.packages("remotes") 
# install Rosyverse metapackage which has a function called `update_all()`
remotes::install_github("brandonerose/Rosyverse")
Rosyverse::update_all() # run update on all packages
Rosyverse::load_all() #load all Rosyverse packages!
```

If you have any issues above download the most recent version of R at
RStudtio and update all packages in RStudio. See
[thecodingdocs.com/r/getting-started](https://www.thecodingdocs.com/r/getting-started "R Getting Started").

## RosyREDCap Installation

Use your REDCap API token to create an updatable R list object (DB) to
be used downstream for analysis, data exports, shiny apps, and even data
imports! We are still in development. At this time this package is **not
suited for Multi-Arm projects or massive REDCap projects yet**. If the
functions are taking more than a minute or two you can use the internal
functions of the package to build a subset. More to come in future
versions!

## Installation

You can install the development version of RosyREDCap like so:

``` r
# install remotes package if you don't have it
# install.packages("remotes") 
remotes::install_github("brandonerose/RosyREDCap")
```

If you have any issues above download the most recent version of R at
RStudtio and update all packages in RStudio. See
[thecodingdocs.com/r/getting-started](https://www.thecodingdocs.com/r/getting-started "R Getting Started").

## Setup

This is how you get REDCap turned into an R database…

``` r
library("RosyREDCap")

projects <- get_projects() # get list of cached projects

DB <- setup_RosyREDCap(
  short_name = "PROJECT1",
  token_name = "PROJECT1_token",
  redcap_base_link<-"https://redcap.miami.edu/",
  dir_path = getwd(), # or change to your intended file path
  # force = T,
  merge_form_name = "patient",
  use_csv = FALSE # if you don't have Microsoft change to TRUE
)
```

You can set your REDCap token in two ways!

``` r
#1. set each time in your session (not recommended in saved/shared scripts!)
Sys.setenv(PROJECT1_token = "YoUrNevErShaReToken")

#2. set to your private R sessions!
usethis::edit_r_environ() #finds your file
# Then you add --> PROJECT1_token = 'YoUrNevErShaReToken'
# then save file and restart R

# If it worked you will see your token when you run...
Sys.getenv("PROJECT1_token")
#And if your DB object is setup properly...
view_redcap_token(DB)
```

## Run Core Functions

The following functions represent the core functions of the package.

``` r

DB <- update_RosyREDCap(DB) # update from redcap by checking log and using saved object 

DB <- add_forms_transformation_to_DB(DB,forms_tranformation = default_forms_transformation(DB))

DB <- transform_DB(DB) # transform to most basic forms, can be modified

DB <- drop_redcap_dir(DB)

#run shiny app!
run_RosyREDCap()

# dev functions not ready for public yet
DB <- clean_DB(DB)
DB <- summarize_DB(DB) #can use for subsets!
DB <- summarize_DB(DB)
DB %>% save_summary() # will save summary data, look at the tabs!
```

## Explore Outputs!

If it worked you will see your token when you run…

``` r

DB$metadata %>% add_list_to_global()

DB$data %>% add_list_to_global()

#DB$summary %>% add_list_to_global() #not ready for public

listviewer::jsonedit(DB) # view object
```

## Future plans

- Future versions will demonstrate more advanced features already
  included!
- Documentation needs to be updated
- Need to add vignettes
- Need to clean up external vs internal namespace
- Plan to document how to use excel for bulk uncoded edits
- Plan to document how to use quality control functions
- Plan to have shiny app that is more all-in-one
- Plan to show R Markdown and automatic HTML, PDF reports
- Need to submit to CRAN
- Open to collaboration/feedback

## Links

The RosyREDCap package is at
[github.com/brandonerose/RosyREDCap](https://github.com/brandonerose/RosyREDCap "RosyREDCap R package")
See instructions above. Install remotes and install RosyREDCap

Donate if I helped you out and want more development (anything helps)!
[account.venmo.com/u/brandonerose](https://account.venmo.com/u/brandonerose "Venmo Donation")

For more R coding visit
[thecodingdocs.com/](https://www.thecodingdocs.com/ "TheCodingDocs.com")

For correspondence/feedback/issues, please email
<TheCodingDocs@gmail.com>!

Follow us on Twitter
[twitter.com/TheCodingDocs](https://twitter.com/TheCodingDocs "TheCodingDocs Twitter")

Follow me on Twitter
[twitter.com/BRoseMDMPH](https://twitter.com/BRoseMDMPH "BRoseMDMPH Twitter")

[![TheCodingDocs.com](man/figures/TCD.png)](http://www.thecodingdocs.com)
