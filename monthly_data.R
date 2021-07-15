## Monthly data graphs

obj1 <- read_csv(here::here("data/monthly_wq.csv"))
nartsmonth <- obj1 %>% filter(station == "NAR")
grbmonth <- obj1 %>% filter(station == "GRB") %>% 
mutate(date = as.Date(date))
