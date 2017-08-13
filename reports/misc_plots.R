# Run data_overview.Rmd first
library(ggthemes)

x <- df
x$month <- as.numeric(format(x$date, '%m'))
x$year <- as.numeric(format(x$date, '%Y'))
x$day <- as.numeric(format(x$date, '%d'))

y <- x %>%
  group_by(year, month) %>%
  summarise(absences = length(which(absence)),
            total = n()) %>%
  mutate(absenteeism_rate = absences / total * 100) %>%
  filter(year < 2016)

g <- ggplot(data = y,
            aes(x = month, y = absenteeism_rate)) +
  geom_bar(stat = 'identity', 
           fill = 'darkgreen', 
           alpha = 0.6) +
  theme_economist() +
  xlab('Month (2015)') +
  ylab('All worker absenteeism rate (%)') +
  ggtitle('Worker absenteeism at the Xinavane açucarera')
g
