read_final_data_2 <- function(worker_type = 'agriculture'){
  require(readxl)
  require(readr)
  
  # Define the names of the different worker types in the files
  wt <- ifelse(worker_type == 'agriculture',
               'Agric',
               ifelse(worker_type == 'factory',
                      'Factory',
                      'Gen Serv'))
  
  # Alter the worker type for reading in general data
  wtg <- 
    ifelse(worker_type == 'agriculture', 'Agriculture',
           ifelse(worker_type == 'factory', 'Factory',
                  'Gen Services'))
  
  # Read in worker data -------------------------------
  workers <- 
    read_csv(paste0('data/dump_2016-07-14/',
                    'Employees under Location Enterprise - ',
                    wtg,
                    '.csv'),
             skip = 1)
  # workers <- workers[!duplicated(workers),]
  workers$worker_type <- worker_type
  
  # Read in absenteeism data -------------------------------
  # General absteneeism data
  all_ab <- 
    rbind(
      read_excel(paste0(
        'data/dump_2016-07-18/',
        'Costs for Activity (FN  (Falta)) at Location Enterprise - ',
        wtg,
        ' for period 1 Jan 2010 to 31 Dec 2013.xls'),
        skip = 1),
      read_excel(paste0(
        'data/dump_2016-07-18/',
        'Costs for Activity (FN  (Falta)) at Location Enterprise - ',
        wtg,
        ' for period 1 Jan 2014 to 18 Jul 2016.xls'),
        skip = 1)
    )
    
  # Remove duplicates
  all_ab <- all_ab[!duplicated(all_ab),]
  
  # Sickness-specific absenteeism data
  sick_ab <- 
    rbind(
      read_excel(paste0(
        'data/dump_2016-07-18/',
        'Costs for Activity (EADor  (Doente)) at Location Enterprise - ',
        wtg, 
        ' for period 1 Jan 2010 to 31 Dec 2013.xls'),
        skip = 1),
      read_excel(paste0(
        'data/dump_2016-07-18/',
        'Costs for Activity (EADor  (Doente)) at Location Enterprise - ',
        wtg, 
        ' for period 1 Jan 2014 to 18 Jul 2016.xls'),
        skip = 1)
    )
    
  
  sick_ab <- sick_ab[!duplicated(sick_ab),]
  
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
  # # Specify which are overtime
  # names(ab)[8:10] <- paste0('overtime_', names(ab)[8:10])
  
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
  # # Add the meta-column names (had to look at spreadsheet for this)
  # names(workers)[97] <- paste0('driver_license_', names(workers)[97])
  # names(workers)[99] <- paste0('passport_', names(workers)[99])
  # 
  ##### CLEAN UP DATE OBJECTS
  
  # ab ---------------
  ab$date <- as.Date(ab$date, format = '%m/%d/%Y')
  
  # workers ---------------
  workers$date_of_birth <- as.Date(workers$date_of_birth, format = '%m/%d/%Y')
  workers$company_entry_date <- as.Date(workers$company_entry_date, format = '%d/%m/%Y')
  workers$last_paid <- as.Date(workers$last_paid, format = '%m/%d/%Y')
  
  ##### CONVERT EVERYTHING TO REGULAR DATAFRAMES
  ab <- data.frame(ab)
  workers <- data.frame(workers)
  
  ##### ASSIGN TO GLOBAL ENVIRONMENT
  return(list(ab, workers))
}