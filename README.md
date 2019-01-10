
<!-- README.md is generated from README.Rmd. Please edit that file -->
shinyEventLogger
================

[![Licence](https://img.shields.io/badge/licence-MIT-blue.svg)](https://www.r-project.org/Licenses/MIT) [![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-organge.svg)](https://www.tidyverse.org/lifecycle/) [![CRAN Status](https://www.r-pkg.org/badges/version-ago/shinyEventLogger)](https://cran.r-project.org/package=shinyEventLogger) [![CRAN Checks](https://cranchecks.info/badges/summary/shinyEventLogger)](https://cran.r-project.org/web/checks/check_results_shinyEventLogger.html) [![Monthly downloads badge](http://cranlogs.r-pkg.org/badges/last-month/shinyEventLogger)](https://cran.r-project.org/package=shinyEventLogger) [![Daily downloads badge](https://cranlogs.r-pkg.org/badges/last-day/shinyEventLogger?color=blue)](https://CRAN.R-project.org/package=shinyEventLogger) [![Weekly downloads badge](https://cranlogs.r-pkg.org/badges/last-week/shinyEventLogger?color=blue)](https://CRAN.R-project.org/package=shinyEventLogger) [![HitCount](http://hits.dwyl.io/kalimu/shinyEventLogger.svg)](http://hits.dwyl.io/kalimu/shinyEventLogger)

by [Kamil Wais](https://kalimu.github.io/)

> **Logging Events in Complex Shiny Apps**. Logging framework dedicated for complex shiny apps. Different types of events can be logged (character string, value of a variable, multi-line output of a function, result of a unit test, custom error, warning, or diagnostic message). Each event can be logged with a list of parameters that are event-specific, common for a group of events, or common for all app events. Logging can be done simultaneously to R console, browser JavaScript console, a file log, and a database (currently MongoDB). Log data can be further analyzed with the help of process-mining techniques from 'bupaR' package.

Installation
------------

### Installing stable version from CRAN

``` r
install.packages("shinyEventLogger")
```

### Installing version in development from GitHub

``` r
# install.packages('devtools')
devtools::install_github("kalimu/shinyEventLogger")
```

Demo Apps
---------

Simple app logging different events to R console, browser JavaScript console and to a file.

``` r
shinyEventLogger::run_demo()
```

Dashboard that allows for interactive analysis of events from demo app.

``` r
shinyEventLogger::run_demo_dashboard()
```

Hello World
-----------

``` r
library(shiny)
library(shinyEventLogger)
shinyApp(
  ui = fluidPage(log_init()),
  server = function(input, output) {
    set_logging()
    log_event("Hello World")
 }
)
```

Executing the code above you should see in the console something like this:

    Logging to R console:          ENABLED
    Logging to JavaScript console: ENABLED
    Logging to a file:             disabled
    Logging to a database:         disabled

    |#1|EVENT|Hello World|FIRED|

Documentation
-------------

Project description: <https://kalimu.github.io/shinyEventLogger/>

Package pkgdown homepage:

Vignettes: \* TODO
