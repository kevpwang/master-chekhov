library(googledrive)
library(googlesheets4)
library(janitor)
library(tidyverse)

search_composer <- function() {
  input <- readline("Please enter a surname: ")
  input <- paste(toupper(substr(input, 1, 1)), substr(input, 2, nchar(input)), sep="")
  results <- master_chekhov %>% 
    filter(str_detect(composer, input))
  
  num_results <- results %>% 
    distinct(composer)
  len_results <- nrow(num_results)
  
  if(len_results > 1) {
    cat("Found multiple results:\n")
    print(num_results)
    input <- as.numeric(readline(
      paste("Which result were you looking for? ", "(number 1-", len_results, ") \n", sep = "")
    ))
    
    while(input %in% c(1:len_results) == FALSE) {
      input <- as.numeric(readline(
        paste("Please enter a number from ", "1-", len_results, ": \n", sep = "")
      ))
    }
    
    result <- pull(slice(num_results, input))
    results <- results %>% 
      filter(composer == result)
  }
  
  View(results)
}

search_period <- function() {
  input <- as.numeric(readline("Please enter a period: (number 0-6) "))
  
  while(input %in% c(0:6) == FALSE) {
    input <- as.numeric(readline("Please enter a number from 0-6: "))
  }
  
  results <- master_chekhov %>% 
    filter(period == input)
  
  View(results)
}

drive_download(file = "https://docs.google.com/spreadsheets/u/1/d/1IwFAsVlEMyFg_4zVZhx-r2gsNnFeZr5wjTlk0BA8LR8/edit?usp=drive_web&ouid=104147344569562764703",
               path = "master_chekhov.csv",
               type = "csv",
               overwrite = TRUE)

master_chekhov <- read_csv("master_chekhov.csv",
                           col_types = cols(
                             Label = col_character(),
                             Format = col_character(),
                             Number = col_character(),
                             Period = col_double(),
                             Composer = col_character(),
                             Work = col_character(),
                             Performers = col_character(),
                             Time = col_double()
                             )
                           ) %>% 
  select(-`X9`) %>% 
  clean_names() %>% 
  drop_na() %>% 
  select(period, composer, work, performers, label, format, number, time)

master_chekhov <- data.frame(lapply(master_chekhov, function(x) {
  stringi::stri_trans_general(x, "Latin-ASCII")
  }))


