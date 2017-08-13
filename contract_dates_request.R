library(tidyverse)
x = workers %>%
  group_by(temporary_or_permanent, contract_count) %>%
  tally

x = workers %>%
  filter(contract_count > 1) %>%
  dplyr::select(number, date_of_birth, contract_count) %>%
  filter(!is.na(number))
x <- data.frame(x)
out_list <- list()
for(i in 1:nrow(x)){
  one_contract <- x[i,]
  out <- one_contract
  for (z in 2:x$contract_count[i]){
    out <- bind_rows(out, one_contract)
  }
  out$contract_number <- 1:x$contract_count[i]
  out_list[[i]] <- out
}
final <- bind_rows(out_list)
final$start_date <- ""
final$end_date <- ""
final <- final %>%
  rename(total_contracts = contract_count)
write_csv(final, '~/Desktop/workers_with_more_than_one_contract.csv')
