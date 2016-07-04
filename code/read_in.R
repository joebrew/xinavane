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

##### READ IN DATA
if('read_in_finished.RData' %in% dir(data_dir)){
  load(paste0(data_dir, '/read_in_finished.RData'))
} else {
  
  setwd(data_dir)

  # Define function to read in data
  reader <- function(worker_type = 'agriculture'){
    if(worker_type == 'agriculture'){
      # General absteneeism data
      all_ab <- #rbind(
        read_excel(paste0(capitalize(worker_type), 
                          '/Xinavane_general absenteeism_',
                          worker_type,
                          '_1.xls'),
                   skip = 1)#,
      #     read_excel(paste0(capitalize(worker_type), 
      #   '/Xinavane_general absenteeism_',
      #   worker_type,
      #   '_2.xls'),
      # skip = 1))
    } else {
      # General absteneeism data
      all_ab <- #rbind(
        read_excel(paste0(capitalize(worker_type), 
                          '/Xinavane_general absenteeism_',
                          worker_type,
                          '_1.xls'),
                   skip = 1)#,
      #     read_excel(paste0(capitalize(worker_type), 
      #   '/Xinavane_general absenteeism_',
      #   worker_type,
      #   '_2.xls'),
      # skip = 1))
    }
    
    all_ab <- all_ab[!duplicated(all_ab),]
    
    # Sickness-specific absenteeism data
    sick_ab <- rbind(read_excel(paste0(capitalize(worker_type), 
                                       '/Xinavane_sickness_',
                                       worker_type,
                                       '_1.xls'),
                                skip = 1),
                     read_excel(paste0(capitalize(worker_type), 
                                       '/Xinavane_sickness_',
                                       worker_type,
                                       '_2.xls'),
                                skip = 1))
    sick_ab <- sick_ab[!duplicated(sick_ab),]
    
    # Worker data
    workers <- rbind(read_excel(paste0(capitalize(worker_type),
                                       '/Xinavane_plantilla trabajadores_',
                                       worker_type,
                                       '_1.xls'),
                                skip = 1),
                     read_excel(paste0(capitalize(worker_type),
                                       '/Xinavane_plantilla trabajadores_',
                                       worker_type,
                                       '_2.xls'),
                                skip = 1))
    workers <- workers[!duplicated(workers),]
    workers$worker_type <- worker_type
    
    ##### COMBINE ABSENCE DATA (for simplicity's sake)
    # Note: these are mutually exclusive datasets - just need to stack on top of one another
    all_ab$type <- 'healthy'
    sick_ab$type <- 'sick'
    ab <- rbind(all_ab, sick_ab); rm (all_ab, sick_ab)
    ab$worker_type <- worker_type
    
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
    names(workers) <- gsub('.', '_', names(workers), fixed = TRUE)
    # Replace slashes
    names(workers) <- gsub('/', '_', names(workers), fixed = TRUE)
    # Remove trailing/leading whitespace
    names(workers) <- gsub("^\\s+|\\s+$", "", names(workers))
    # Spaces to underscores
    names(workers) <- gsub(' ', '_', names(workers))
    # Remove double underscores 
    names(workers) <- gsub('__', '_', names(workers))
    # Add the meta-column names (had to look at spreadsheet for this)
    names(workers)[97] <- paste0('driver_license_', names(workers)[97])
    names(workers)[99] <- paste0('passport_', names(workers)[99])
    
    ##### CLEAN UP DATE OBJECTS
    
    # ab ---------------
    ab$date <- as.Date(ab$date, format = '%m/%d/%Y')
    
    # workers ---------------
    workers$date_of_birth <- as.Date(workers$date_of_birth, format = '%m/%d/%Y')
    workers$company_entry_date <- as.Date(workers$company_entry_date, format = '%d/%m/%Y')
    
    ##### CONVERT EVERYTHING TO REGULAR DATAFRAMES
    ab <- data.frame(ab)
    workers <- data.frame(workers)
    
    ##### ASSIGN TO GLOBAL ENVIRONMENT
    return(list(ab, workers))
  }
  
  # Run list for all three worker types
  worker_types <- c('agriculture', 
                    'factory',
                    'general services')
  master_ab <- list()
  master_workers <- list()
  for (wt in 1:length(worker_types)){
    this_type <- worker_types[wt]
    message('Working on : ', this_type )
    x <- reader(worker_type = this_type)
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
  
  # ISSUE WITH NUMBER OF WORKERS / REPEAT ROWS 
  # !!! NEED TO ADDRESS
  # For now, just removing duplicates
  workers <- workers[!duplicated(workers$id_number),]
  setwd(root_dir)
  rm(x)
  ##### SAVE IMAGE
  save.image(paste0(data_dir, '/read_in_finished.RData'))
}
msg('Done reading in and cleaning data.')

