msg <- function(text){
  lng <- nchar(as.character(text))
  framer <- paste0(rep('-',lng), collapse = '')
  message(paste0(framer,
             '\n',
             text,
             '\n',
             framer,
             '\n'))
  
}
