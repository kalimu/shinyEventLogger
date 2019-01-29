
devtools::install_github("kalimu/shinyEventLogger", force = TRUE)
library(shinyEventLogger)

# devtools::use_travis()

# run_demo(in_background = TRUE)

devtools::document(roclets = c('rd', 'collate', 'namespace', 'vignette'))
spelling::spell_check_package(".")

# spelling::get_wordlist(pkg = ".")
# spelling::update_wordlist(".")
# spelling::spell_check_setup(".")

goodpractice::gp()

setwd("~/# Rscripts/shinyEventLogger")
pkgdown:::build_site_external()

devtools::install_github("kalimu/shinyEventLogger", force = TRUE)
options(rsconnect.check.certificate = FALSE)
setwd("~/# Rscripts/shinyEventLogger/inst/shiny/demoapp")
dir()
rsconnect::deployApp(appFiles = c("app.R", "DESCRIPTION", "Readme.md", ".db_url"), forceUpdate = TRUE)


setwd("~/# Rscripts/shinyEventLogger/inst/shiny/dashboardapp")
dir()
rsconnect::deployApp(appFiles = c("app.R", ".db_url"), forceUpdate = TRUE)

devtools::check()

devtools::check_win_release()
devtools::check_win_devel()


