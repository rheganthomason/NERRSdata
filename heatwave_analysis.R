## Marine heatwaves

### Narragansett heatwaves
#devtools::install_github("robwschlegel/heatwaveR")
library(heatwaveR)
library(tidyverse)
library(readxl)
library(stringr)
library(readr)

raw.dir <- here::here("data")

narts_heatwave_csv<-"nar_heatwave.csv"

  
nar.heatwave<-read_csv(file.path(raw.dir,narts_heatwave_csv),
              col_types = cols(temp = col_double(),t = col_date()))
  # gb<-read_csv(file.path(raw.dir,heatwave_gb_csv),
  #              col_types = cols(temp = col_double(),t = col_date()))
  # mab<-read_csv(file.path(raw.dir,heatwave_mab_csv),
  #               col_types = cols(temp = col_double(),t = col_date()))
  
  
  # Narragansett
ts <- heatwaveR::ts2clm(nar.heatwave, climatologyPeriod = c("2007-01-03", "2020-12-31"))
nar.mhw <- heatwaveR::detect_event(ts)
  
mhw<- nar.mhw$clim %>%
  mutate(Site = c("NAR"),
         Year = c("2020"))
#mhw.nar.year <- mhw[5845:6210,] ## Pull out just 2020

heatwaveR::lolli_plot(nar.mhw)

mhw %>%
  filter(Site == "NAR", 
         str_detect(t, "2020")) %>% 
  ggplot(aes(x = t, y = temp))+
  geom_flame(aes(y2 = thresh))+ 
  geom_line(aes(x = t, y = seas, color = "a"), size = 1)+
  geom_line(aes(x = t, y = thresh, color = "c"), size = 1)+
  geom_line(aes(x = t, y = temp, color = "b"))+
  scale_colour_manual(values = c("turquoise4", "sienna3", "black"),
                      labels = c("Climatology","Temperature", "Threshold"))+
  ylab("Temperature (C)")+
  xlab(element_blank())+
  scale_x_date(date_labels = "%b", breaks = "1 month")+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position=c(0.2, 0.8))+
  ecodata::theme_title()



  # #GOM
  # ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  # gom.mhw <- heatwaveR::detect_event(ts)
  # gom.hw<- gom.mhw$event %>%
  #   dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
  #   dplyr::mutate(EPU = "GOM")
  # # MAB
  # ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2011-12-31"))
  # mab.mhw <- heatwaveR::detect_event(ts)
  # mab.hw<- mab.mhw$event %>%
  #   dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
  #   dplyr::mutate(EPU = "MAB")
  # # Cumulative intensity
  # cum.intensity <- rbind(gb.hw, gom.hw, mab.hw) %>%
  #   dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y"))) %>%
  #   dplyr::group_by(Time, EPU) %>%
  #   dplyr::summarise(Value = as.numeric(sum(intensity_cumulative))) %>%
  #   dplyr::mutate(Var = "cumulative intensity") %>%
  #   dplyr::ungroup()
  # #Max intensity
  # max.intensity <- rbind(gb.hw, gom.hw, mab.hw) %>%
  #   dplyr::mutate(Time = as.numeric(format(as.Date(date_start, format="%Y-%m-%d"),"%Y")))  %>%
  #   dplyr::rename(Value = intensity_max) %>%
  #   dplyr::mutate(Var = "maximum intensity")%>%
  #   dplyr::select(Time, EPU, Value, Var)
  # 
  # heatwave<- rbind(cum.intensity, max.intensity) %>%
  #   dplyr:: mutate(Units = "degrees C",
  #                  Time = as.numeric(Time))
  










# #### get_heatwave_year get single year of heatwave
# get_heatwave_year <- function(save_clean = F){
#   # import data
#   gom<-read_csv(file.path(raw.dir,"GOM_OISST.csv"),
#                 col_types = cols(temp = col_double(),t = col_date()))
#   gb<-read_csv(file.path(raw.dir,"GB_OISST.csv"),
#                col_types = cols(temp = col_double(),t = col_date()))
#   mab<-read_csv(file.path(raw.dir,"MAB_OISST.csv"),
#                 col_types = cols(temp = col_double(),t = col_date()))
#   #GB
#  ts <- heatwaveR::ts2clm(gb, climatologyPeriod = c("1982-01-01", "2011-12-31"))
#   gb.mhw <- heatwaveR::detect_event(ts)
#   #GOM
#   ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2011-12-31"))
#   gom.mhw <- heatwaveR::detect_event(ts)
#   #MAB
#   ts <- heatwaveR::ts2clm(mab, climatologyPeriod = c("1982-01-01", "2011-12-31"))
#   mab.mhw <- heatwaveR::detect_event(ts)
#   
#   ### Take just clim
#   #GB
#  mhw<- gb.mhw$clim %>%
#     mutate(EPU = c("GB"),
#            Year = c("2020"))# add EPU column
#   mhw.gb.year <- mhw[13880:14223,]## days in 2020 data set only went to dec 9, 2020
#   #GOM
#   mhw<- gom.mhw$clim %>%
#     mutate(EPU = c("GOM"),
#            Year = c("2020"))# add EPU column
#   mhw.gom.year <- mhw[13880:14223,]## days in 2020 data set only went to dec 9, 2020
#   #MAB
#   mhw<- mab.mhw$clim %>%
#     mutate(EPU = c("MAB"),
#            Year = c("2020"))# add EPU column
#   mhw.mab.year <- mhw[13880:14223,]## days in 2020 data set only went to dec 9, 2020
#   
#   ### 2012
#   #GB
#   mhw<- gb.mhw$clim %>%
#     mutate(EPU = c("GB"),
#            Year = c("2012"),
#            DoY = stringr::str_sub(t,-5))# add EPU column
#   mhw.gb.year2012 <- mhw[10958:11301,]## days in 2012
#   #GOM
#   mhw<- gom.mhw$clim %>%
#     mutate(EPU = c("GOM"),
#            Year = c("2012"))# add EPU column
#   mhw.gom.year2012 <- mhw[10958:11301,]## days in 2012
#   mhw<- mab.mhw$clim %>%
#     mutate(EPU = c("MAB"),
#            Year = c("2012"),
#            DoY = stringr::str_sub(t,-5))# add EPU column
#   mhw.mab.year2012 <- mhw[10958:11301,]## days in 2012
#   
#   heatwave_year<- rbind(mhw.gb.year, mhw.gom.year, mhw.mab.year,
#                         mhw.gb.year2012, mhw.gom.year2012, mhw.mab.year2012)