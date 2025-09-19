library(nycflights13)
library(glue)

flights_date = flights %>%
  mutate(date = ymd(glue("{year} {month} {day}"))) %>% 
  count(date)

plt = ggplot(flights_date, aes(x = weekdays(date), y = n))

plt + geom_boxplot()

flights_date = flights_date %>% 
  mutate(day_of_week = weekdays(date),
         day_of_week = fct_reorder(day_of_week, n),
         day_of_week = fct_rev(day_of_week))

ggplot(flights_date, aes(x = date, color = day_of_week, y = n))+ geom_line() + scale_color_viridis_d()
