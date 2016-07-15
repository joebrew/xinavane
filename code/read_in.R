##### DIRECTORY INFORMATION
# Start in 'xinavane'
root_dir <- getwd()
data_dir <- paste0(root_dir, '/data')
code_dir <- paste0(root_dir, '/code')

##### PACKAGES
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(Hmisc)

##### FUNCTIONS
source(paste0(root_dir, '/lib/helpers.R'))
source(paste0(root_dir, '/lib/read_final_data.R'))


##### READ IN DATA
if('read_in_finished.RData' %in% dir(data_dir)){
  load(paste0(data_dir, '/read_in_finished.RData'))
} else {
  # Run list for all three worker types
  worker_types <- c('agriculture', 
                    'factory',
                    'general services')
  master_ab <- list()
  master_workers <- list()
  for (wt in 1:length(worker_types)){
    this_type <- worker_types[wt]
    message('Working on : ', this_type )
    x <- read_final_data(worker_type = this_type)
    master_ab[[wt]] <- x[[1]]
    master_workers[[wt]] <- x[[2]]
  }
  ab <- do.call('rbind', master_ab)
  workers <- do.call('rbind', master_workers)
  
  ##### GET RID OF EMPTY COLUMNS
  flags <- rep(FALSE, ncol(workers))
  for (j in 1:ncol(workers)){
    if(length(which(is.na(workers[,j]))) == nrow(workers)){
      flags[j] <- TRUE
    }
  }
  workers <- workers[,!flags]
  rm(flags, j)
  
  # For now, just removing duplicates
  workers <- workers[!duplicated(workers$id_number),]
  setwd(root_dir)
  rm(x)
  
  # NEED TO REMOVE WORKERS OUTSIDE OF OUR TIME PERIOD

  # Create a dataframe of eligible absences
  x <- expand.grid(
    date = seq(min(ab$date, na.rm = TRUE),
               max(ab$date, na.rm = TRUE),
               by = 1),
    number = unique(workers$number))
  
  # Get start and end dates
  x <- left_join(x = x,
                 y = workers %>%
                   dplyr::select(number,
                                 company_entry_date,
                                 last_paid))
  
  # Remove those with no number or no company entry or last paid dates
  x <- x %>%
    filter(!is.na(date),
           !is.na(number),
           !is.na(company_entry_date),
           !is.na(last_paid))
  
  # Remove those workers who hadn't yet started
  x <- x %>%
    dplyr::filter(date >= company_entry_date)
  
  # Remove those who are no longer working
  x <- x %>%
    dplyr::filter(date <= last_paid)
  
  # Get day of week
  x$dow <- weekdays(x$date)
  
  # Remove those dates for which the worker has off
  # Do this differentially for those that have 6 days off vs. 5
  # and this with "None selected" vs an actual day selected.
  x <- left_join(x = x,
                 y = workers %>%
                   dplyr::select(number, days_off),
                 by = c("number")) %>%
    # Mark "non selected" as Sunday
    mutate(days_off = ifelse(days_off == 'None selected',
                             'Sunday',
                             days_off)) %>%
    filter(!dow %in% days_off)
  
  # x is now a dataframe containg all
  # unique worker-days for which a worker
  # SHOULD have worked
  
  # Now we need to bring absences into x
  x <- x %>%
    left_join(ab %>%
                mutate(absence = TRUE) %>%
                dplyr::select(number, type, date, absence)) %>%
    mutate(absence = ifelse(is.na(absence), FALSE, absence))
  
  # Join worker data to our absence/eligibility dataframe
  df <- x %>%
    left_join(workers %>%
                dplyr::select(#-days_off, 
                  -last_paid,
                              -company_entry_date, 
                              -type)
              )
  
  # # See absences
  # x = df %>% filter(days_off == 'None selected' & days_wk == 6) %>% group_by(absence, dow) %>% tally
  
  ##### SAVE IMAGE
  save.image(paste0(data_dir, '/read_in_finished.RData'))
}
msg('Done reading in and cleaning data.')

