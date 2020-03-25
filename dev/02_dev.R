# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 

# 2. All along your project

## 2.1 Add modules
## 
golem::add_module( name = "plots_pca" ) # Name of the module
golem::add_module( name = "my_other_module" ) # Name of the module

## 2.2 Add dependencies

usethis::use_package( "DAPARdata" ) # To call each time you need a new package

## 2.3 Add tests

usethis::use_test( "app" )

## 2.4 Add a browser button

golem::browser_button()

## 2.5 Add external files

golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom_sass" )

## 2.6 Add function files
golem::add_fct( "helpers" ) 
golem::add_utils( "Prostar" )
#These two function create R/fct_helpers.R and R/utils_helpers.R, two file you can use to add business logic functions.

## 2.7 Add other files
usethis::use_r("config")

# 3. Documentation

## 3.1 Vignette
usethis::use_vignette("Prostar2")
devtools::build_vignettes()

## 3.2 Code coverage
## You'll need GitHub there
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! 
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
