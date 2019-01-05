

devtools::document(roclets = c('rd', 'collate', 'namespace', 'vignette'))
spelling::spell_check_package(".")

spelling::get_wordlist(pkg = ".")
# spelling::update_wordlist(".")
# spelling::spell_check_setup(".")

# read_eventlog(db = readLines(system.file("shiny", "demoapp/.db_url",
# package = "shinyEventLogger"))[1], last_n = 20, verbose  = F)









