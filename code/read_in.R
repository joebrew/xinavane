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
library(rgdal)

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
  rm(flags, j, master_workers)
  
  # For now, just removing duplicates
  workers <- workers[!duplicated(workers$number),]
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
    mutate(month = format(date, '%m'),
           year = format(date, '%Y')) %>%
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
                  -end_year)
  x <- x[,!grepl('company_entry|last_paid', names(x))]
  
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
  
  # # Write dta/csv for elisa
  # library(foreign)
  # write.dta(df, 'xinavane_monthly_panel_2014-2016.dta')
  # write_csv(df, 'monthly_panel.csv')
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
  # write_csv(locations, 'locations_for_laia.csv')
  
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
                  MEMBER_GENDER,
                  MEMBER_NAME,
                  HOUSEHOLD_NUMBER) %>%
    rename(permid = PERM_ID_MEMBER,
           dob = BIRTH_MEMBER,
           gender = MEMBER_GENDER,
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
  # Recode gender
  census_magude$gender <-
    ifelse(census_magude$gender == '1',
           'male',
           ifelse(census_magude$gender == '2', 
                  'female',
                  NA))
  
  # Manhica ----------------------------------------------------------
  load('census/openhds/2016-07-15_individual.RData')
  individual$dob <- as.Date(individual$dob)
  individual$house_number <- substr(individual$lastName, 1, 8)
  individual$name <- individual$firstName
  individual$permid <- individual$lastName
  individual <- individual %>%
    dplyr::select(permid, name, house_number, dob, gender)
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
                  gender,
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
  # Recode gender
  census_manhica$gender <-
    ifelse(census_manhica$gender == 'F',
           'female',
           ifelse(census_manhica$gender == 'M',
                  'male', 
                  NA))
  
  
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
  # Get a last name
  workers$last_name <- toupper(workers$last_name)
  census$last_name <-
    unlist(lapply(strsplit(census$name, ' '), 
                  function(x){x[length(x)]}))
  # Get a first name
  workers$first_name <- 
    unlist(lapply(strsplit(workers$name, ' '), 
                  function(x){x[1]}))
  census$first_name <-
    unlist(lapply(strsplit(census$name, ' '), 
                  function(x){x[1]}))
  # Get a middle name
  workers$middle_name <- 
    unlist(lapply(strsplit(workers$name, ' '), 
                  function(x){x[2]}))
  census$middle_name <- 
    unlist(lapply(strsplit(census$name, ' '), 
                  function(x){x[2]}))
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
  census$birth_year <- census$birth_month <- NA
  workers$birth_year <- workers$birth_month <-  NA
  census$birth_year[!is.na(census$dob)] <- 
    as.numeric(format(census$dob[!is.na(census$dob)], '%Y'))
  census$birth_month[!is.na(census$dob)] <- 
    as.numeric(format(census$dob[!is.na(census$dob)], '%m'))
  workers$birth_year[!is.na(workers$dob)] <- 
    as.numeric(format(workers$dob[!is.na(workers$dob)], '%Y'))
  workers$birth_month[!is.na(workers$dob)] <- 
    as.numeric(format(workers$dob[!is.na(workers$dob)], '%m'))
  # census stuff
  census$name_in_census <- census$name
  
  # Keep only individual permids
  census <- census %>% dplyr::filter(!duplicated(permid))
  # Keep only individual workers
  workers <- workers %>% dplyr::filter(!duplicated(number));
  
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
  
  # MATCHING ---------------------------------------------
  for (i in which(is.na(workers$permid))){
    done <- FALSE
    message(i)
    # Get the row in question
    this_row <- workers[i,]
    # Get all possible matches
    possibles <- census
    # # Keep only those with the same sex
    # the_sex <- this_row$gender
    # if(!is.na(the_sex)){
    #   possibles <- possibles %>%
    #     filter(gender == the_sex)
    # } else {
    #   # If no gender, then we won't match
    #   done <- TRUE
    # }
    # # Only continue if matchables exist
    # if(nrow(possibles) == 0){
    #   done <- TRUE
    # }
    
    if(!done){
      # # Compute matching of name
      # scores <- stringdist(a = this_row$name, 
      #                      b = possibles$name,
      #                      method = 'jw')
      # Extract last name
      last_name_scores <- stringdist(a = this_row$last_name,
                                     b = possibles$last_name,
                                     method = 'jw')
      last_name_scores <- ifelse(is.na(last_name_scores), 1, last_name_scores)
      # Compute matching of first name
      first_name_scores <-stringdist(a = this_row$first_name,
                                     b = possibles$first_name,
                                     method = 'jw')
      first_name_scores <- ifelse(is.na(first_name_scores), 0.5, first_name_scores)
      # Compute matching of middle name
      middle_name_scores <-stringdist(a = this_row$middle_name,
                                     b = possibles$middle_name,
                                     method = 'jw')
      middle_name_scores <- ifelse(is.na(middle_name_scores), 0, middle_name_scores)
      # Compute birthday scores
      dob_scores <- stringdist(a = this_row$dob,
                               b = possibles$dob,
                               method = 'jw')
      dob_scores <- ifelse(is.na(dob_scores), 0, dob_scores)
      # Compute sex score
      sex_scores <- ifelse(possibles$gender == this_row$gender,
                           0, 
                           1)
      sex_scores <- ifelse(is.na(sex_scores), 0, sex_scores)
      # Get overall score
      scores <- 
        (last_name_scores * 0.3) +
        (first_name_scores * 0.2) +
        (middle_name_scores * 0.1) + 
        (dob_scores * 0.25) +
        (sex_scores * 0.15)
      
      # Narrow down the possibles to keep only those below threshold
      possibles <- possibles[scores <= 0.1,]
      # narrow down scores too (in case we need to use them later)
      scores <- scores[scores <= 0.1]
      # If there's anything left, keep going
      if(nrow(possibles) == 0){
        done <- TRUE
      }
      if(!done){
        # # If there is anyone left, see if we can get birthday
        # shared_birthday <- which(possibles$dob == this_row$dob)
        # if(length(shared_birthday) > 0){
        #   # narrow down possibles and scores
        #   possibles <- possibles[shared_birthday,]
        #   scores <- scores[shared_birthday]
        # }
        # Keep only the best score
        possibles <- possibles[which.min(scores),][1,]
        score <- min(scores)

        # Register the score and permid
        workers$score[i] <- score
        workers$permid[i] <- possibles$permid
        workers$name_in_census[i] <- possibles$name_in_census
        workers$same_birthday[i] <- possibles$dob == workers$dob[i]
      }
    }
  }
 
  save.image('temp.RData')
  x <- workers %>%
    dplyr::select(id_number,
                  number,
                  score,
                  permid,
                  name_in_census,
                  same_birthday)
  save(x, file = 'workers_post_matching.RData')
   
  # Examine results ad throw out those with bad ones
  # Having looked at results, setting threshold at 0.1
  workers$name_in_census[workers$score > 0.1] <- NA
  workers$permid[workers$score > 0.1] <- NA
  workers$same_birthday[workers$score > 0.1] <- NA
  workers$score[workers$score > 0.1] <- NA
  
  # Join census information to workers
  census <- census %>%
    filter(!is.na(permid))
  workers <- 
    left_join(x = workers,
              y = census %>%
                dplyr::select(-name, -birth_year, -name_in_census,
                              -last_name, -first_name, -middle_name, -birth_month,
                              -gender) %>%
                rename(dob_in_census = dob),
              by = 'permid')
  
  # Remove garbage
  workers <- workers %>% filter(!is.na(workers$name))
  
  # Read in the locations which laia coded
  locations <- readxl::read_excel('data/Bairros.xls', sheet = 4)
  # "Location (1=Manhiça (except Xinavane); 2= Xinavane; 3=Magude)"
  names(locations)[4] <- 'location_laia'
  locations$location_laia <- ifelse(locations$location_laia == 1,
                               'Manhiça',
                               ifelse(locations$location_laia == 2,
                                      'Xinavane',
                                      ifelse(locations$location_laia == 3, 
                                             'Magude',
                                             NA)))
  # "Originally from outside Magude or Manhiça district? (1=yes)" 
  names(locations)[5] <- 'originally_outside_laia'
  locations$originally_outside_laia <-
    ifelse(locations$originally_outside_laia == 1, 
           TRUE, 
           FALSE)
  locations$originally_outside_laia[is.na(locations$originally_outside_laia)] <- FALSE
  
  # Join
  locations <- data.frame(locations)
  address_columns <- c('address_1',
                       'address_2',
                       'address_3')
  for(i in address_columns){
    locations[,i][locations[,i] == 'NA'] <- NA
    workers[,i][workers[,i] == 'NA'] <- NA
    # locations[,i][is.na(locations[,i])] <- 'x'
    # workers[,i][is.na(workers[,i])] <- 'x'
  }
  workers$ad <- 
    paste0(workers$address_1, workers$address_2, workers$address_3)
  locations$ad <- 
    paste0(locations$address_1, locations$address_2, locations$address_3)
  # locations <- locations %>%
  #   dplyr::select(ad, location_laia, originally_outside_laia)
    locations <- locations[locations$ad %in% unique(workers$ad),]
  locations$location_laia[is.na(locations$location_laia)] <- ''
  workers$ad[!workers$ad %in% locations$ad] <- NA
  locations <- locations[!duplicated(locations$ad),]
  workers <- workers[!duplicated(workers),]
  
  
  
  workers <- left_join(workers,
                       locations %>%
                         dplyr::select(ad, location_laia, originally_outside_laia),
                       by = 'ad')

  save.image('tempx.RData')
  
  # # Bring in worker types and recategorize
  # worker_categories <-
  #   c('Agriculture',
  #     'Factory',
  #     'General Services')
  # worker_list <- list()
  # for (i in 1:length(worker_categories)){
  #   temp <- read_excel('data/Xinavane job title.xlsx',
  #                      sheet = worker_categories[i],
  #                      skip = 1)
  #   temp$worker_type <- tolower(worker_categories[i])
  #   worker_list[[i]] <- temp
  # }
  # worker_types <- do.call('rbind', worker_list)
  # worker_types <- worker_types %>%
  #   dplyr::select(job_title, `new job_title`, worker_type)
  # names(worker_types) <- 
  #   c('job_title',
  #     'new_job_title',
  #     'worker_type')
  # worker_types <- read_excel('data/worker_types_match_10_aug.xlsx')
  worker_types <- read.csv('data/worker_types_match_10_aug.csv')
  worker_types <- worker_types[,1:4]
  worker_types <-
    worker_types %>%
    dplyr::select(worker_type, job_title, new_job_title)

  # Deal with the character encoding issue by finding the most similar one
  x <- data.frame(workers = sort(unique(workers$job_title)),
                  worker_types = NA)
  worker_scores <- adist(x = sort(unique(workers$job_title)),
                         y = sort(unique(worker_types$job_title)))
  x <- data.frame(workers = sort(unique(workers$job_title)),
                  worker_types = sort(unique(worker_types$job_title)))
  indices <- apply(worker_scores, 1, function(z){which.min(z)})
  x$worker_types <- sort(unique(worker_types$job_title))[indices]
  workers <- left_join(x = workers,
                       y = x,
                       by = c('job_title' = 'workers'))
  rm(x)
  workers <- workers %>%
    dplyr::select(-job_title) 
  workers <- workers %>%
    dplyr::rename(job_title = worker_types) 
  
  # Merge
  workers <- workers %>%
    left_join(worker_types,
              by = c('job_title',
                     'worker_type'))
  
  # Use the xinavane shapefile to get precise worker location
  xinavane <- readOGR('geo', 'Xinavane')
  workers_geo <- workers
  workers_geo <- workers_geo %>% filter(!is.na(x), !is.na(y))
  coordinates(workers_geo) <- ~x+y
  proj4string(workers_geo) <- proj4string(xinavane)
  x <- over(workers_geo, polygons(xinavane))
  in_xinavane <- data.frame(in_xinavane = x == 1 & !is.na(x),
                            number = workers_geo$number)
  in_xinavane <- in_xinavane[!duplicated(in_xinavane$number),]
  workers <- workers %>%
    left_join(in_xinavane, 
              by = 'number')
  workers$geo <- ifelse(workers$in_xinavane,
                        'Xinavane',
                        workers$geo)
  
  # Reformulate df with the new information
  df <- 
    df %>%
    dplyr::select(month_start,
                  year, 
                  month,
                  absences,
                  sick_absences,
                  eligibles,
                  absenteeism_rate,
                  sick_absenteeism_rate,
                  last_name,
                  number)
  df <- df %>%
    filter(!duplicated(number, month_start))
  workers <- workers %>%
    dplyr::select(-second_name,
                  -third_name,
                  -insurance_1_no_,
                  -insurance_2_no_,
                  -insurance_3_no_,
                  -insurance_4_no_,
                  -gratuity_entitlement,
                  -gratuity_payout_date,
                  -extend_probation,
                  -on_probation,
                  -email,
                  -`actual_bal_`,
                  -`days_bal_`
                  -`basic_bal_`,
                  -expiry_date.1,
                  -x, -y, -lng, -lat, -lon,
                  -in_xinavane)
  workers <-
    workers %>%
    dplyr::select(-address_1.y,
                  -address_2.y,
                  -address_3.y,
                  -address_1.x,
                  -address_2.x,
                  -address_3.x)
  save.image('temp2.RData')

  # Adjust for estimated denominator (ie, >100% or <0% absences)
  df$eligibles <- ifelse(df$absenteeism_rate > 100,
                         df$absences,
                         df$eligibles)
  df$absenteeism_rate <- df$absences / df$eligibles * 100
  
  # Fix worker new_job_title spacing
  workers$new_job_title <- as.character(workers$new_job_title)
  workers$new_job_title <-
    ifelse(grepl('supervisor', workers$new_job_title),
           'supervisor',
           workers$new_job_title)
  
  # Create a variable for permanent vs. temporary workers
  workers$temporary_or_permanent <- as.character(NA)
  workers$temporary_or_permanent <- 
    ifelse(workers$type %in% c('Apprentice',
                               'Eventual',
                               'Estagiário'),
           'temporary',
           ifelse(workers$type %in% c('Contract',
                                      'Efectivo',
                                      'Empregados'),
                  'permanent',
                  'other'))
  # Remove sensitive variables
  workers <- 
    workers %>%
    dplyr::select(number,
                  date_of_birth,
                  category,
                  gender,
                  marital_status,
                  isactive,
                  type,
                  job_description,
                  home_location,
                  hrs_day,
                  days_wk,
                  team,
                  grade,
                  shift_worker,
                  shift_type,
                  activity_group,
                  pay_group,
                  contract_count,
                  rate,
                  salaried,
                  pay_method,
                  income,
                  paid_in_kind,
                  paygrp_days,
                  worker_type,
                  permid,
                  perfect_match,
                  score,
                  dob_in_census,
                  longitude,
                  latitude,
                  geo,
                  ad,
                  location_laia,
                  originally_outside_laia,
                  job_title,
                  new_job_title,
                  temporary_or_permanent)

  # Remove duplicates
  workers <- workers %>%
    filter(!duplicated(number))

    df <- 
      df %>% 
      left_join(workers,
              by = 'number')
  
    # Convert NAs back to NA
    df <- data.frame(df)
    classes <- lapply(df, class)
    for (j in 1:ncol(df)){
      print(j)
      if(classes[j] %in% c('character', 'factor')){
        df[,j][df[,j] == ''] <- NA
      }
    }
  
  ##### SAVE IMAGE
  save.image(paste0(data_dir, '/read_in_finished.RData'))
}
msg('Done reading in and cleaning data.')

# # Write dta and csv
recent <- df %>%
  filter(month_start >= '2014-01-01')
library(foreign)
write.dta(recent, 'xinavane_monthly_panel_2014-2016_only_2016-09-25.dta')
write_csv(recent, 'xinavane_monthly_panel_2014-2016_only_2016-09-25.csv')
# 
# # 
# # # ANIMATION PLOT
# # source('code/theme.R')
# # the_dates <- sort(unique(df$month_start))
# # xl <- range(the_dates)
# # x <- df %>% 
# #   filter(!is.na(location_laia),
# #          location_laia != 'Xinavane') %>%
# #   mutate(district = location_laia)
# # results_list <- list()
# # dir.create('gifs')
# # for (i in 1:length(the_dates)){
# #   message(i)
# #   file_name <- i
# #   while(nchar(file_name) <3){
# #     file_name <- paste0('0', file_name)
# #   }
# #   this_date <- the_dates[i]
# #   these_data <- x %>%
# #     filter(month_start <= this_date) %>%
# #     group_by(date = month_start, district) %>%
# #     summarise(absences = sum(absences),
# #               eligibles = sum(eligibles)) %>%
# #     ungroup %>%
# #     mutate(absenteeism_rate = absences / eligibles * 100) %>%
# #     mutate(today = date == this_date) %>%
# #     mutate(observation_date = this_date)
# #   results_list[[i]] <- these_data
# #   
# #   png(filename = paste0('gifs/', file_name, '.png'),
# #       width = 680, height = 480)
# #   g <- ggplot(data = these_data,
# #               aes(x = date,
# #                   y = absenteeism_rate,
# #                   color = district)) +
# #     geom_point(alpha = 0.5) +
# #     geom_line(alpha = 0.2) +
# #     geom_smooth() +
# #     xlab('Date') +
# #     ylab('Absenteeism rate') +
# #     theme_xinavane() +
# #     xlab('Date') +
# #     ylab('Absenteeism rate') +
# #     scale_color_manual(name = 'Location',
# #                        values = c('darkorange', 'darkgreen')) +
# #     geom_vline(xintercept = as.numeric(as.Date(c('2015-12-01',
# #                                                  '2016-01-01'))),
# #                alpha = 0.6,
# #                lty = 2) +
# #     xlim(xl) +
# #     ylim(0, 3) +
# #     ggtitle(format(the_dates[i], format = '%B %d, %Y'))
# #   print(g)
# #   dev.off()
# # }
# # results <- do.call('rbind', results_list)
# # g <- ggplot(data = these_data,
# #             aes(x = date,
# #                 y = absenteeism_rate,
# #                 color = district)) +
# #   geom_point(alpha = 0.5) +
# #   geom_line(alpha = 0.2) +
# #   geom_smooth() +
# #   xlab('Date') +
# #   ylab('Absenteeism rate') +
# #   theme_xinavane() +
# #   xlab('Date') +
# #   ylab('Absenteeism rate') +
# #   scale_color_manual(name = 'Location',
# #                      values = c('darkorange', 'darkgreen')) +
# #   geom_vline(xintercept = as.numeric(as.Date(c('2015-12-01',
# #                                                '2016-01-01'))),
# #              alpha = 0.6,
# #              lty = 2) +
# #   xlim(xl) +
# #   ylim(0, 3) +
# #   ggtitle(format(the_dates[i], format = '%B %d, %Y'))
# # print(g)
# # 
# # # Peak at results
# # x <- workers %>%
# #   filter(score > 0.1 & score < 0.15) %>%
# #   dplyr::select(name, name_in_census, score,
# #                 same_birthday)
# # # View(head(x, 100))
# # 
# # 
# # # Plot
# # cols <- adjustcolor(ifelse(census$geo == 'Magude', 'darkorange', 'darkred'), alpha.f = 0.1)
# # 
# # plot(census$x,
# #      census$y,
# #      xlab = 'Longitude',
# #      ylab = 'Latitude',
# #      col = NA)
# # # # Now set the plot region to grey
# # # rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
# # #        "black")
# # 
# # points(census$x,
# #      census$y,
# #      col = cols,
# #      pch = 16,
# #      cex = 0.2)
# # title(main = 'Magude and Manhiça residents')
# # 
# # worker_cols <- adjustcolor('blue', alpha.f = 0.5)
# # points(workers$x,
# #        workers$y,
# #        col = worker_cols,
# #        pch = 16,
# #        cex = 0.2)
# # 
# # # Add xinavane factory
# # xin <-
# #   data.frame(x = 32.8022838,
# #              y = -25.043155)
# # 
# # points(xin, 
# #        col = adjustcolor('blue',
# #                               alpha.f = 0.3), 
# #        pch = 16, 
# #        cex = 2)
# # 
# # # Add xinavane district
# # plot(xinavane, add = TRUE,
# #      col = adjustcolor('black', alpha.f = 0.3))
# # 
# # 
# # library(leaflet)
# # man <- workers %>% filter(!is.na(longitude),
# #                           geo == 'Manhiça') 
# # man <- man[1:500,]
# # mag <- workers %>% filter(!is.na(longitude),
# #                           geo == 'Magude')
# # mag <- mag[1:500,]
# # 
# # leaflet::leaflet(xinavane) %>%
# #   addProviderTiles("Esri.WorldImagery") %>%
# #   addPolygons(color = 'red') %>%
# #   addCircles(data = xin,
# #              lng = xin$x,
# #              lat = xin$y,
# #              fill = TRUE) %>%
# #   addCircleMarkers(lng = man$longitude,
# #                    lat = man$latitude,
# #                    fill = TRUE,
# #                    fillColor = 'blue',
# #                    color = NA,
# #                    fillOpacity = 0.3) %>%
# #   addCircleMarkers(lng = mag$longitude,
# #                    lat = mag$latitude,
# #                    fill = TRUE,
# #                    fillColor = 'red',
# #                    color = NA,
# #                    fillOpacity = 0.3) %>%
# #   addPopups(lng = mag$longitude,
# #             lat = mag$latitude,
# #             popup = mag$name) %>%
# #   addPopups(lng = man$longitude,
# #             lat = man$latitude,
# #             popup = man$name)
# # 
# # # #
# # #
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
# 
# 
# # GIF for person-level worker data
# dir.create('worker_gifs')
# dir.create('worker_gifts_number')
# x <- df %>%
#   mutate(location = ifelse(location_laia == 'Magude',
#                            'Magude',
#                            ifelse(location_laia == 'Manhiça',
#                                   'Manhiça',
#                                   NA))) %>%
#   filter(!is.na(location))
# dates <- sort(unique(x$month_start))
# numbers <- sort(unique(x$number))
# number_numbers <- as.numeric(factor(numbers))
# # dates <- dates[dates >= '2015-01-01']
# for(i in seq(1, max(number_numbers), by = 20)){
# # for (i in dates){
#   
#   message(i)
#   file_name <- i
#   while(nchar(file_name) < 6){
#     file_name <- paste0('0', file_name)
#   }
#     png(filename = paste0('worker_gifts_number/', file_name, '.png'),
#         width = 680, height = 480)
#     
#     sub_data <- x %>% 
#       # filter(month_start <= i) %>%
#       filter(number %in% numbers[1:i])
# 
#     g <- ggplot(data = sub_data,
#            aes(x = month_start,
#                y = absenteeism_rate,
#                color = location)) +
#       # geom_jitter(alpha = 0.2) +
#       # geom_smooth(alpha = 0.5) +
#       geom_line(aes(group = number),
#                 alpha = 0.7) +
#       xlab('Month') +
#       ylab('Worker-specific absenteeism rate') +
#       scale_color_manual(name = 'Location',
#                          values = c('darkorange', 'darkgreen')) +
#       geom_vline(xintercept = as.numeric(as.Date(c('2015-12-01',
#                                                    '2016-01-01'))),
#                  lty = 2, 
#                  alpha = 0.6) +
#       # scale_y_log10() +
#       theme_xinavane() +
#       ggtitle('Person-level absenteeism',
#               'One-contract workers only') +
#       xlim(dates[1], dates[length(dates)]) +
#       ylim(0, 100)
#       # xlim(dates[1], i)
#     print(g)
#     dev.off()
# }