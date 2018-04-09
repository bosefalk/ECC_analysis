library(tidyverse)
library(magrittr)
library(validate)

load_data <- function(path_name, print_validation = FALSE) {
  
  dat <- read_csv(file = paste0('data/', path_name, '/ESS1-7e01.csv'))
  # Convert relevant columns to factors
  dat %<>% mutate_at(c("cntry", "cname", "cedition", "cproddat", "name", "essround", 'edition'), funs(factor(.)))
  
  # First 12 columns are always the same, in the remaining look for non-response codes and replace with NA
  non_response_codes <- c(66, 77, 88, 99)
  dat[, 13:length(dat)] <- apply(dat[, 13:length(dat)], FUN = function(x) {replace(x, x %in% non_response_codes, NA)}, MARGIN = 2)
  
  # open up the supporting doc with variable descriptions etc in browser / Rstudio viewer
  viewer <- getOption("viewer")
  viewer(paste0('data/', path_name, '/codebook.html'))

  
  # Data validation block
  valid_metadata <- dat %>% check_that(nlevels(cname) == 1,
                                       nlevels(cedition) == 1,
                                       nlevels(name) == nlevels(essround))
  
  valid_responses <- dat[, 13:length(dat)] %>% check_that(. >=0 & . <= 10)
  
  if(any(summary(valid_metadata)$fails > 0)) {
    warning("Metadata failed validation")
  }
  
  if(any(summary(valid_responses)$fails > 0)) {
    warning("Responses failed validation")
  }
  
  if(print_validation == TRUE) {
    print(summary(valid_metadata))
    print(summary(valid_responses))
  }
  
  return(dat)
}









