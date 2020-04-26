## code to prepare `lipsum_data` dataset goes here
lipsum_data = jsonlite::fromJSON("https://baconipsum.com/api/?type=all-meat&paras=15&start-with-lorem=1")
usethis::use_data(lipsum_data, internal=TRUE)
