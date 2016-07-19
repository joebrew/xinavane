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
library(sp)
library(raster)
library(stringdist)

##### FUNCTIONS
source(paste0(root_dir, '/lib/helpers.R'))
source(paste0(root_dir, '/lib/read_final_data.R'))
source(paste0(root_dir, '/lib/read_final_data_2.R'))


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
    x <- read_final_data_2(worker_type = this_type)
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
  
  # For those with greater than 100% absenteeism,
  # this means we underestimated their true days at risk. Re-do.  
  # !!!!!!!!!!
  
  # # Write dta/csv for elisa
  # library(foreign)
  # write.dta(df, '~/Desktop/monthly_panel.dta')
  # write_csv(df, '~/Desktop/monthly_panel.csv')
  # 
  # # Write locations for laia
  # locations <- workers %>%
  #   dplyr::select(address_1,
  #                 address_2,
  #                 address_3,
  #                 address_4)
  # locations <- locations[!duplicated(locations),]
  # locations <- locations %>%
  #   arrange(address_1,
  #           address_2,
  #           address_3,
  #           address_4)
  # write_csv(locations, '~/Desktop/locations_for_laia.csv')
  
  ######################################################
  # Bring in census data
  
  # Magude  ----------------------------------------------------------
  load('census/maltem/2016-07-15_HOUSEHOLD.RData')
  locations_magude <- HOUSEHOLD; rm(HOUSEHOLD)
  locations_magude$lng <-
    locations_magude$longitude <-
    locations_magude$x <-
    locations_magude$lon <-
    locations_magude$HOUSEHOLD_HEAD_GPS_LNG
  locations_magude$lat <-
    locations_magude$latitude <-
    locations_magude$y <-
    locations_magude$HOUSEHOLD_HEAD_GPS_LAT
  locations_magude$geo <- 'Magude'
  locations_magude$permid <- locations_magude$HOUSEHOLD_HEAD_PERM_ID
  locations_magude$house_number <- locations_magude$HOUSEHOLD_HEAD_AGREG_NUM
  locations_magude <-
    locations_magude %>% dplyr::select(house_number,
                                       lng,
                                       longitude,
                                       x,
                                       lon,
                                       lat,
                                       latitude,
                                       y,
                                       geo)
  # Also get birthday
  load('census/maltem/2016-07-15_MEMBER.RData')
  magude_member <- MEMBER; rm(MEMBER)
  magude_member <-
    magude_member %>%
    dplyr::select(PERM_ID_MEMBER,
                  BIRTH_MEMBER,
                  MEMBER_NAME,
                  HOUSEHOLD_NUMBER) %>%
    rename(permid = PERM_ID_MEMBER,
           dob = BIRTH_MEMBER,
           name = MEMBER_NAME,
           house_number = HOUSEHOLD_NUMBER) %>%
    mutate(dob = as.Date(substr(dob, 1, 10)))
  # Join
  census_magude <-
    left_join(x = magude_member,
              y = locations_magude,
              by = 'house_number') %>%
    mutate(geo = 'Magude')
  rm(magude_member)
  
  # Manhica ----------------------------------------------------------
  load('census/openhds/2016-07-15_individual.RData')
  individual$dob <- as.Date(individual$dob)
  individual$house_number <- substr(individual$lastName, 1, 8)
  individual$name <- individual$firstName
  individual$permid <- individual$lastName
  individual <- individual %>%
    dplyr::select(permid, name, house_number, dob)
  # Read in coordinates (emailed from Charfudin)
  coords <- read_csv('census/openhds/Coordenadas.csv')
  names(coords) <- c('house_number', 'region', 'lat', 'lng')
  coords$region <- NULL
  # Combine
  census_manhica <- left_join(individual,
                              coords,
                              by = 'house_number')
  rm(individual, coords)
  
  # Convert census_manhica to lat/lng
  census_manhica_location <- census_manhica %>% filter(!is.na(lat) & !is.na(lng))
  census_manhica_no_location <- census_manhica %>% filter(is.na(lat) | is.na(lng))
  sp::coordinates(census_manhica_location) <- ~lng+lat
  proj4string(census_manhica_location) <- CRS("+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  census_manhica_location <- spTransform(census_manhica_location, CRS('+proj=longlat'))
  # Extract the coordinates
  ll <- coordinates(census_manhica_location)
  # Add back into the original dataframe
  census_manhica_location$x <- ll[,1]
  census_manhica_location$y <- ll[,2]
  census_manhica_location <- data.frame(census_manhica_location@data)
  # change names
  census_manhica_location <-
    census_manhica_location %>%
    rename(lng = x,
           lat = y)
  # Combine all of manhica back together
  census_manhica <-
    rbind(census_manhica_location,
          census_manhica_no_location)
  rm(census_manhica_location, census_manhica_no_location)
  
  # Expand coordinates
  census_manhica$longitude <-
    census_manhica$x <-
    census_manhica$lon <-
    census_manhica$lng
  census_manhica$latitude <-
    census_manhica$y <-
    census_manhica$lat
  census_manhica$geo <- 'Manhiça'
  census_manhica <-
    census_manhica %>%
    dplyr::select(permid,
                  dob,
                  name,
                  house_number,
                  lng,
                  longitude,
                  x,
                  lon,
                  lat,
                  latitude,
                  y,
                  geo)
  
  
  # Join the censuses ----------------------------------------
  census_magude <- data.frame(census_magude)
  census_manhica <- data.frame(census_manhica)
  for (j in 1:ncol(census_manhica)){
    if(class(census_manhica[,j]) == 'factor'){
      census_manhica[,j] <- as.character(census_manhica[,j])
    }
    if(class(census_magude[,j]) == 'factor'){
      census_magude[,j] <- as.character(census_magude[,j])
    }
  }
  census <- rbind(census_manhica, census_magude)
  rm(census_manhica, census_magude)
  # DONE
  
  # Join census to worker data -------------------------------------
  workers$dob <- workers$date_of_birth
  workers$name <- toupper(workers$full_name)
  census$name <- toupper(iconv(enc2utf8(census$name),sub="byte"))
  # Remove from census anything before the last hyphen (if there are any)
  keep_last <- function(x){
    x <- strsplit(x, split = '-')
    x <- unlist(x[[1]][length(x[[1]])])
    return(x)
  }
  census$name <- ifelse(grepl('[0-9]+', census$name),
                        keep_last(census$name),
                        census$name)
  # Get birth years in both
  census$birth_year <- NA
  workers$birth_year <- NA
  census$birth_year[!is.na(census$dob)] <- 
    as.numeric(format(census$dob[!is.na(census$dob)], '%Y'))
  workers$birth_year[!is.na(workers$dob)] <- 
    as.numeric(format(workers$dob[!is.na(workers$dob)], '%Y'))
  
  # census stuff
  census$name_in_census <- census$name
  
  # Attempt to join directly
  workers <-
    left_join(x = workers,
              y = census %>%
                mutate(perfect_match = TRUE) %>%
                mutate(same_birthday = TRUE) %>%
                dplyr::select(permid, dob, name, name_in_census, 
                              perfect_match, 
                              same_birthday),
              by = c('dob', 'name'))
  workers$perfect_match <-
    ifelse(is.na(workers$perfect_match), FALSE, workers$perfect_match)
  
  # prop.table(table(is.na(x$permid)))
  
  # Use string matching with jaro winkler distance
  workers$score <- NA
  workers$score[!is.na(workers$permid)] <- 0
  
  for (i in which(is.na(workers$permid))){
    done <- FALSE
    message(i)
    # Get the row in question
    this_row <- workers[i,]
    # Get all possible matches
    possibles <- census
    # Keep only those with the same birth year
    yob <- this_row$birth_year
    if(!is.na(yob)){
      possibles <- possibles %>%
        filter(birth_year == yob)
    } else {
      done <- TRUE
    }
    
    if(!done){
      # Compute matching of name
      scores <- stringdist(a = this_row$name, 
                           b = possibles$name,
                           method = 'jw')
      # Narrow down the possibles to keep only those below threshold
      possibles <- possibles[scores <= 0.2,]
      # narrow down scores too (in case we need to use them later)
      scores <- scores[scores <= 0.2]
      # If there's anything left, keep going
      if(nrow(possibles) == 0){
        done <- TRUE
      }
      if(!done){
        # If there is anyone left, see if we can get birthday
        shared_birthday <- which(possibles$dob == this_row$dob)
        if(length(shared_birthday) > 0){
          # narrow down possibles and scores
          possibles <- possibles[shared_birthday,]
          scores <- scores[shared_birthday]
        }
        # Keep only the best score
        if(nrow(possibles) > 1){
          possibles <- possibles[which.min(scores),][1,]
        } else {
          done <- TRUE
        }
        
        if(!done){
          # Register the score and permid
          workers$score[i] <- min(scores)
          workers$permid[i] <- possibles$permid
          workers$name_in_census[i] <- possibles$name_in_census
          workers$same_birthday[i] <- possibles$dob == workers$dob[i]
        }
      }
    }
  }
  
  # Join census information to workers
  workers <- 
    left_join(x = workers,
              y = census %>%
                dplyr::select(-name, -birth_year, -name_in_census) %>%
                rename(dob_in_census = dob),
              by = 'permid')
  
  ##### SAVE IMAGE
  save.image(paste0(data_dir, '/read_in_finished.RData'))
}
msg('Done reading in and cleaning data.')



# Peak at results
x <- workers %>%
  filter(score > 0.1 & score < 0.15) %>%
  dplyr::select(name, name_in_census, score,
                same_birthday)
# View(head(x, 100))


# Plot
cols <- adjustcolor(ifelse(census$geo == 'Magude', 'darkorange', 'darkred'), alpha.f = 0.1)

plot(census$x,
     census$y,
     xlab = 'Longitude',
     ylab = 'Latitude')
# Now set the plot region to grey
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
       "black")

points(census$x,
     census$y,
     col = cols,
     pch = 16,
     cex = 0.2)
title(main = 'Magude and Manhiça residents')

worker_cols <- adjustcolor('blue', alpha.f = 0.5)
points(workers$x,
       workers$y,
       col = worker_cols,
       pch = 16,
       cex = 0.2)

# Add xinavane
xin <- 
  data.frame(x = 32.6174913,
             y = -25.0517658)
points(xin, col = 'white', pch = 16, cex = 2)

# #
# 
# x <-
#   df %>%
#   group_by(month_start) %>%
#   summarise(absences = sum(absences),
#             eligibles = sum(eligibles),
#             sick_absences = sum(sick_absences)) %>%
#   mutate(absenteeism_rate = absences / eligibles * 100,
#          sick_absenteeism_rate = sick_absences / eligibles * 100)
# 
# ggplot(data = x) +
#   geom_bar(aes(x = month_start,
#                 y = eligibles),
#            stat = 'identity') +
#   geom_bar(aes(x = month_start,
#                 y = absences),
#            stat = 'identity',
#             fill = 'red')
# 
# ggplot(data = x,
#        aes(x = month_start, y = absenteeism_rate)) +
#   geom_bar(stat = 'identity')
# 
# ggplot(data = x,
#        aes(x = month_start, y = sick_absenteeism_rate)) +
#   geom_bar(stat = 'identity')
