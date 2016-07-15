# First run data_overview.Rmd
monthly <- df %>%
  mutate(year_month = format(date, '%Y-%m')) %>%
  group_by(year_month, number) %>%
  summarise(#full_name = first(full_name),
            absences = length(which(absence)),
            eligibles = n())#,
            # date_of_birth = first(date_of_birth),
            # gender = first(gender),
            # marital_status = first(marital_status),
            # boot_size = first(boot_sizes),
            # live_in = first(live_in),
            # address_1 = first(address_1),
            # address_2 = first(address_2),
            # address_3 = first(address_3),
            # address_4 = first(address_4),
            # no_of_dependants = first(no_of_dependants),
            # first_contract_start = first(first_contract_start),
            # no_children = first(no_children),
            # job_title = first(job_title),
            # job_description = first(job_description),
            # hrs_day = first(hrs_day),
            # shift_worker = first(shift_worker),
            # shift_type = first(shift_type))

monthly <- monthly %>%
  left_join(workers,
            by = 'number')

library(foreign)
write.dta(monthly, '~/Desktop/monthly_panel.dta')
