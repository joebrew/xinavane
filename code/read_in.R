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
                                 last_paid),
                 by = 'number')
  
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
  x <- x %>%
    left_join(workers %>%
                dplyr::select(number, days_wk, days_off),
              by = 'number') %>%
    filter(!is.na(days_wk))
  x6 <- x %>% filter(days_wk == 6 &
                       days_off != 'Sunday;Saturday' &
                       days_off != 'None selected')
  x5 <- x %>% filter(!number %in% unique(x6$number))
  
  # For those with 6 days, and clear days off, construct
  # true panel data (denominator only)
  x6 <- x6 %>%
    filter(dow != days_off)
  # Turn into monthly
  x6 <- x6 %>%
    mutate(month = format(date, '%m'),
           year = format(date, '%Y')) %>%
    group_by(year, month, number) %>%
    summarise(eligibles = n())
  
  # For those with 5 days, or those who don't have clear days off,
  # we have to just estimate the number of days they were eligible
  x5 <-
    x5 %>%
    mutate(day = format(date, '%d'), 
           month = format(date, '%m'),
           year = format(date, '%Y')) %>%
    group_by(year, month) %>%
    mutate(days_in_month = length(unique(day))) %>%
    ungroup %>%
    group_by(year, month, number) %>%
    summarise(eligibles = (mean(days_wk) / 7) * mean(days_in_month))
  x5 <- x5[,names(x6)]
  
  # Recombine our exact and estimated denominators
  x <- rbind(x5, x6)
  
  # Get the company entry date and start date
  x <- 
    x %>%
    left_join(workers %>%
                dplyr::select(number,
                              company_entry_date,
                              last_paid) %>%
                mutate(start_month = format(company_entry_date, '%m'),
                       start_year = format(company_entry_date, '%Y'),
                       end_month = format(last_paid, '%m'),
                       end_year = format(last_paid, '%Y')),
              by = 'number') %>%
    mutate(flag = ifelse(start_month == month & start_year == year,
                         TRUE,
                         ifelse(end_month == month & end_year == year,
                                TRUE,
                                FALSE))) %>%
    filter(!flag) %>%
    dplyr::select(-flag,
                  -start_month,
                  -start_year,
                  -end_month,
                  -end_year,
                  -company_entry_date,
                  -last_paid)

  # x is now a dataframe containg all
  # unique worker-months with the number of eligible days for work
  
  # Now we need to bring (montyly) absences into x
  x <- x %>%
    left_join(ab %>%
                mutate(day = format(date, '%d'), 
                       month = format(date, '%m'),
                       year = format(date, '%Y')) %>%
                mutate(absence = TRUE) %>%
                group_by(year, month, number) %>%
                summarise(absences = n(),
                          sick_absences = length(type[type == 'sick'])) %>%
                dplyr::select(number, year, month, absences,
                              sick_absences),
              by = c('year', 'month', 'number')) %>%
    mutate(absences = ifelse(is.na(absences), 0, absences),
           sick_absences = ifelse(is.na(sick_absences), 0, sick_absences))
  
  # Add a rate
  x$absenteeism_rate <- x$absences / x$eligibles * 100
  x$sick_absenteeism_rate <- x$sick_absences / x$eligibles * 100
  
  # Add a month start column
  x$month_start <- 
    as.Date(paste0(x$year, '-', x$month, '-01'))
  
  # Reorder a bit
  x <- x %>%
    dplyr::select(month_start,
                  year,
                  month,
                  number, 
                  absences, 
                  sick_absences,
                  eligibles,
                  absenteeism_rate,
                  sick_absenteeism_rate)
  
  # Join worker data to our absence/eligibility dataframe
  df <- x %>%
    left_join(workers,
              by = 'number')
  rm(x, x5, x6)
  
  # Write dta/csv for elisa
  library(foreign)
  write.dta(df, '~/Desktop/monthly_panel.dta')
  write_csv(df, '~/Desktop/monthly_panel.csv')

  ##### SAVE IMAGE
  save.image(paste0(data_dir, '/read_in_finished.RData'))
}
msg('Done reading in and cleaning data.')

######################################################
# Bring in census data

# Magude
load('census/maltem/2016-07-15_HOUSEHOLD.RData')
census_magude <- HOUSEHOLD; rm(HOUSEHOLD)
census_magude$lng <- 
  census_magude$longitude <-
  census_magude$x <-
  census_magude$lon <-
  census_magude$HOUSEHOLD_HEAD_GPS_LNG
census_magude$lat <- 
  census_magude$latitude <-
  census_magude$y <-
  census_magude$HOUSEHOLD_HEAD_GPS_LAT
census_magude$name <- census_magude$HOUSEHOLD_HEAD_NAME_HEAD
census_magude$geo <- 'Magude'

census_magude <-
  census_magude %>% dplyr::select(lng, 
                           longitude,
                           x,
                           lon,
                           lat, 
                           latitude,
                           y,
                           name, 
                           geo)

# Manhica
load('census/openhds/2016-07-15_location.RData')
# load('census/openhds/2016-07-15_individual.RData')
# individual <- left_join(individual, location,
                        # by = 'uuid')

# census_manhica <- individual; rm(location, individual)
census_manhica <- location; rm(location)
census_manhica$lng <-
  census_manhica$x <-
  census_manhica$lon <- 
  census_manhica$longitude
census_manhica$lat <-
  census_manhica$y <-
  census_manhica$latitude
census_manhica$name <- NA
census_manhica$geo <- 'Manhiça'
census_manhica <- 
  census_manhica %>%
  dplyr::select(lng, 
              longitude,
              x,
              lon,
              lat, 
              latitude,
              y,
              name, 
              geo)

# Join the censuses
census <- rbind(census_manhica, census_magude)


# DONE

plot(census$x, census$y, col = ifelse(census$geo == 'Magude', 'red', 'blue'), pch = '.')

# 

x <- 
  df %>%
  group_by(month_start) %>%
  summarise(absences = sum(absences),
            eligibles = sum(eligibles),
            sick_absences = sum(sick_absences)) %>%
  mutate(absenteeism_rate = absences / eligibles * 100,
         sick_absenteeism_rate = sick_absences / eligibles * 100)

ggplot(data = x) +
  geom_bar(aes(x = month_start,
                y = eligibles),
           stat = 'identity') +
  geom_bar(aes(x = month_start,
                y = absences),
           stat = 'identity',
            fill = 'red')

ggplot(data = x,
       aes(x = month_start, y = absenteeism_rate)) +
  geom_bar(stat = 'identity')

ggplot(data = x,
       aes(x = month_start, y = sick_absenteeism_rate)) +
  geom_bar(stat = 'identity')
