## easier graphs!!

# library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(SWMPr)
# remotes::install_github("NOAA-EDAB/ecodata")

#add in raw data
raw_dat <- bind_rows(
  import_local(here::here("data-raw/wq"), station_code = "nartswq") %>% 
    qaqc() %>% 
    mutate(station = "nartswq"),
  import_local(here::here("data-raw/wq"), station_code = "wqbmpwq") %>% 
    qaqc() %>% 
    mutate(station = "wqbmpwq"),
  import_local(here::here("data-raw/wq"), station_code = "welinwq") %>% 
    qaqc() %>% 
    mutate(station = "welinwq"),
  import_local(here::here("data-raw/wq"), station_code = "grbgbwq") %>% 
    qaqc() %>% 
    mutate(station = "grbgbwq"))

#new daily data frame
daily <- raw_dat %>% 
  mutate(date = format(as.Date(datetimestamp), "%Y-%m-%d")) %>%
  select(date,
         station,
         Temp = temp, 
         Sal = sal, 
         DO_Pct = do_pct, 
         DO_mgl = do_mgl) %>%
  pivot_longer(cols = c("Temp", "Sal", "DO_Pct", "DO_mgl"), names_to = "Variable", values_to = "value") %>% 
  group_by(station, Variable, date) %>%
  summarize(mean_daily = mean(value, na.rm = TRUE),
            mean_daily = ifelse(is.nan(mean_daily),
                                NA_real_,
                                mean_daily))

#add thresholds
rmdomgl<-daily %>% group_by(station) %>%
  filter(Variable == "DO_mgl" & mean_daily > 0 & mean_daily < 15 | is.na(mean_daily))

rmdopct<-daily %>% group_by(station) %>%
  filter(Variable == "DO_Pct" & mean_daily > 50 & mean_daily < 120 | is.na(mean_daily))

rmtemp<-daily %>% group_by(station) %>%
  filter(Variable == "Temp" & mean_daily > 0 & mean_daily < 30 | is.na(mean_daily))

rmsal<-daily %>% group_by(station) %>%
  filter(Variable == "Sal" & mean_daily > 0 & mean_daily < 32 | is.na(mean_daily))

monthly <- rbind(rmdomgl, rmdopct, rmtemp, rmsal)

#so each station is added in one code chunk
station_names <- as_labeller(
  c('nartswq' = "Narragansett",
    "wqbmpwq"= "Waquoit Bay",
    "welinwq" = "Wells Bay", 
    "grbgbwq" = "Great Bay"))

## grp takes the non-NA for each station, month, and variable and creates a "group" (1, 2, 3, ...)
## counter takes the number of rows for each station, month, and variable
## the first mutate "mean_month" takes all of the data with a maximum count < 10 for each group and makes "NA", otherwise it takes the mean.
## If you try to take the mean of all NAs, it creates a "NaN", which is not exactly like

dat_month <- monthly %>%
  mutate(month_year = format(as.Date(date), "%Y-%m")) %>%
  group_by(station, month_year, Variable, grp = with(rle(!is.na(mean_daily)), 
                                                     rep(seq_along(lengths),
                                                         lengths))) %>%
  mutate(counter = 1:n()) %>%
  ungroup() %>%
  select(-grp) %>%
  group_by(station, month_year, Variable) %>%
  mutate(mean_month = ifelse(max(counter) >= 10,
                             mean(mean_daily, na.rm = TRUE),
                             NA_real_),
         mean_month = ifelse(is.nan(mean_month),
                             NA_real_,
                             mean_month),
         date = as.Date(paste0(month_year, "-1"))) %>% ## Create a dummy date variable for the first of each month
  select(month_year, date, station, Variable, mean_month) %>%
  distinct(.keep_all = TRUE)

## oxygen mg/L
dat_month %>% 
  filter(Variable == "DO_mgl") %>%
  ggplot(aes(date, mean_month))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.08, color = "steelblue", se = F)+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(0, 15)+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (mg/L)") +
  facet_wrap(.~station, labeller = station_names) + 
  ecodata::theme_ts()+
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10))

## % oxygen
dat_month %>% 
  filter(Variable == "DO_Pct") %>%
  ggplot(aes(date, mean_month))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.08, color = "steelblue", se = F)+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(50, 120)+
  labs(x = "Year", y = "Dissolved Oxygen Concentration (%)") +
  facet_wrap(.~station, labeller = station_names) + 
  ecodata::theme_ts()+
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 8), axis.title = element_text(size = 10))

## Temperature
dat_month %>% 
  filter(Variable == "Temp") %>%
  ggplot(aes(date, mean_month))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.08, color = "steelblue", se = F)+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(0, 30)+
  labs(x = "Year", y = "Temperature (C)") +
  facet_wrap(.~station, labeller = station_names) + 
  ecodata::theme_ts()+
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 8), axis.title = element_text(size = 10))

## Salinity
dat_month %>% 
  filter(Variable == "Sal") %>%
  ggplot(aes(date, mean_month))+
  geom_point(color = "lightblue")+
  geom_smooth(method = "loess", span = 0.08, color = "steelblue", se = F)+
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+
  ylim(20, 32)+
  labs(x = "Year", y = "Salinity (psu)") +
  facet_wrap(.~station, labeller = station_names) +
  ecodata::theme_ts()+
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 9), axis.title = element_text(size = 10))