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

##### READ IN DATA

setwd(data_dir)

# General absteneeism data
all_ab <- read_excel('Xinavane_general absenteeism_agriculture_joe.xls',
                     skip = 1)

# Sickness-specific absenteeism data
sick_ab <- read_excel('Xinavane_sickness_agriculture_joe.xls',
                      skip = 1)

# Worker data
workers <- read_excel('Xinavane_plantilla trabajadores_agriculture_joe.xls',
                      skip = 1)

setwd(root_dir)

##### COMBINE ABSENCE DATA (for simplicity's sake)
# Note: these are mutually exclusive datasets - just need to stack on top of one another
all_ab$type <- 'healthy'
sick_ab$type <- 'sick'
ab <- rbind(all_ab, sick_ab); rm (all_ab, sick_ab)


##### MANUALLY CLEAN UP COLUMN NAMES

# ab ---------------
# Lower case all
names(ab) <- tolower(names(ab))
# Remove trailing/leading whitespace
names(ab) <- gsub("^\\s+|\\s+$", "", names(ab))
# Spaces to underscores
names(ab) <- gsub(' ', '_', names(ab))
# Specify which are overtime
names(ab)[8:10] <- paste0('overtime_', names(ab)[8:10])

# workers  ---------------
# Lower case all
names(workers) <- tolower(names(workers))
# Remove periods
names(workers) <- gsub('.', '', names(workers), fixed = TRUE)
# Remove trailing/leading whitespace
names(workers) <- gsub("^\\s+|\\s+$", "", names(workers))
# Spaces to underscores
names(workers) <- gsub(' ', '_', names(workers))
# Add the meta-column names (had to look at spreadsheet for this)
names(workers)[97] <- paste0('driver_license_', names(workers)[97])
names(workers)[99] <- paste0('passport_', names(workers)[99])

##### CLEAN UP DATE OBJECTS

# ab ---------------
ab$date <- as.Date(ab$date, format = '%m/%d/%Y')

# workers ---------------
workers$date_of_birth <- as.Date(workers$date_of_birth, format = '%m/%d/%Y')
workers$company_entry_date <- as.Date(workers$company_entry_date, format = '%d/%m/%Y')
