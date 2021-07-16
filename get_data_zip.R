## Big data pull for all station and sites Narragansett, Wells, Waquoit and Great Bay

library(readxl)
library(tidyverse)


###################################################################
####  NUTRIENTS ###################################################
###################################################################
nut<- list.files(here::here("data-raw/nut"))
#i = "grbgbnut2007.csv"
nut.out<- data.frame()
for (i in nut){

  df<- as_tibble(read.csv(here::here("data-raw/nut",i))) 
  
  StationCode <- unique(df$StationCode)
  
  rawdata<- df %>% dplyr::select(-X, -StationCode,-isSWMP,-Historical,-ProvisionalPlus,-F_Record)
  
  data <- rawdata %>% 
    dplyr::select(DateTimeStamp,!(dplyr::starts_with("F_"))) %>%
    tidyr::pivot_longer(.,cols=-DateTimeStamp,names_to = "Variable",values_to = "Value")
  
  ratings <- rawdata %>%
    dplyr::select(DateTimeStamp,dplyr::starts_with("F_")) %>%
    dplyr::rename_with(stringr::str_replace, 
                       pattern="F_",
                       replacement = "") %>% 
    tidyr::pivot_longer(.,cols=-DateTimeStamp,names_to = "Variable",values_to = "Value")
  
  mainTable <- data %>% dplyr::left_join(.,ratings,by = c("DateTimeStamp","Variable")) %>%
    dplyr::rename(Value = Value.x, Rating = Value.y) %>% 
    dplyr::mutate(Rating = stringr::str_remove_all(Rating,c("<|>|\\([A-Z]{3}\\)|\\[[A-Z]{3}\\]"))) %>%
    dplyr::mutate(Rating = as.numeric(Rating)) %>% 
    drop_na() %>% 
    filter(Rating == c(0,3,5)) %>% 
    mutate(dateonly = stringr::str_extract(DateTimeStamp, "^.{10}"),
           date = lubridate::mdy(dateonly,tz="EST")) %>% 
    group_by(date, Variable) %>% 
    summarise(mean_daily = mean(Value)) %>% 
    ungroup() %>% 
    mutate(station = c(StationCode))
    
  nut.out<- rbind(nut.out, mainTable)
  
}


write.csv(nut.out, file = "data/nutrients_all.csv")

dat<- read_csv(here::here("data", "nutrients_all.csv"))
nut_pep<- dat %>% 
  filter(station == c("grbgbnut", "nartsnut","wqbmpnut", "welinnut" )) 
write.csv(nut_pep, file = "data/nutrient_pep.csv")


###################################################################
####  WATER QUALITY ###############################################
###################################################################

wq<- list.files(here::here("data-raw/wq"))
#i = "grbgbwq2015.csv"
wq.out<- data.frame()
for (i in wq){
  
  df<- read.csv(here::here("data-raw/wq",i)) %>% 
    mutate(dateonly = stringr::str_extract(DateTimeStamp, "^.{10}"),
           date = lubridate::mdy(dateonly,tz="EST"))
    
  
  StationCode <- unique(df$StationCode)
  
  
  rawdata<- df %>% dplyr::select(-DateTimeStamp, -dateonly, -X, -StationCode,-isSWMP,-Historical,-ProvisionalPlus,-F_Record)
  
  data <- rawdata %>% 
    dplyr::mutate_if(is.character, as.numeric) %>% 
    dplyr::select(date,!(dplyr::starts_with("F_"))) %>%
    tidyr::pivot_longer(.,cols=-date,names_to = "Variable",values_to = "Value")
  
  ratings <- rawdata %>%
    dplyr::select(date,dplyr::starts_with("F_")) %>%
    dplyr::rename_with(stringr::str_replace, 
                       pattern="F_",
                       replacement = "") %>% 
    tidyr::pivot_longer(.,cols=-date,names_to = "Variable",values_to = "Value")
  
  
  # both data frames re now same size and have same names. Different values
  # left join 
  mainTable <- data %>% dplyr::left_join(.,ratings,by = c("date","Variable")) %>%
    dplyr::rename(Value = Value.x, Rating = Value.y) %>% 
    dplyr::mutate(Rating = stringr::str_remove_all(Rating,c("<|>|\\([A-Z]{3}\\)|\\[[A-Z]{3}\\]"))) %>%
    dplyr::mutate(Rating = as.numeric(Rating)) %>% 
    drop_na() %>% 
    filter(Rating == c(0,3,5))  %>% 
    group_by(date, Variable) %>% 
    summarise(mean_daily = mean(Value), 
              sd_daily = sd(Value)) %>% 
    ungroup() %>% 
    mutate(station = c(StationCode))
  
  
  
  wq.out<- rbind(wq.out, mainTable)
  
}



wq.out %>% filter(Variable == "DO_mgl" & station == "wqbslwq   ") %>% 
  ggplot(aes(x = date, y = mean_daily))+
  geom_point()



write.csv(wq.out, file = "data/water_quality_all.csv")

dat<- read_csv(here::here("data", "water_quality_all.csv"))
wq_pep<- dat %>% 
  filter(station == c("grbgbwq" , "nartswq","wqbmpwq", "welinwq" )) 


write.csv(wq_pep, file = "data/water_quality_pep.csv")


##################################################
######## METEROLOGY #############################
##################################################

met<- list.files(here::here("data-raw/met"))
#i = "grbgbwq2015.csv"
met.out<- data.frame()
for (i in met){
  
  df<- read.csv(here::here("data-raw/met",i)) %>% 
    mutate(dateonly = stringr::str_extract(DatetimeStamp, "^.{10}"),
           date = lubridate::mdy(dateonly,tz="EST"))
  
  
  StationCode <- unique(df$StationCode)
  
  
  rawdata<- df %>% dplyr::select(-DatetimeStamp, -dateonly, -X, -StationCode,-isSWMP,-Historical,-ProvisionalPlus,-F_Record)
  
  data <- rawdata %>% 
    dplyr::mutate_if(is.character, as.numeric) %>% 
    dplyr::select(date,!(dplyr::starts_with("F_"))) %>%
    tidyr::pivot_longer(.,cols=-date,names_to = "Variable",values_to = "Value")
  
  ratings <- rawdata %>%
    dplyr::select(date,dplyr::starts_with("F_")) %>%
    dplyr::rename_with(stringr::str_replace, 
                       pattern="F_",
                       replacement = "") %>% 
    tidyr::pivot_longer(.,cols=-date,names_to = "Variable",values_to = "Value")
  
  
  # both data frames re now same size and have same names. Different values
  # left join 
  mainTable <- data %>% dplyr::left_join(.,ratings,by = c("date","Variable")) %>%
    dplyr::rename(Value = Value.x, Rating = Value.y) %>% 
    dplyr::mutate(Rating = stringr::str_remove_all(Rating,c("<|>|\\([A-Z]{3}\\)|\\[[A-Z]{3}\\]"))) %>%
    dplyr::mutate(Rating = as.numeric(Rating)) %>% 
    drop_na() %>% 
    filter(Rating == c(0,3,5))  %>% 
    group_by(date, Variable) %>% 
    summarise(mean_daily = mean(Value), 
              sd_daily = sd(Value)) %>% 
    ungroup() %>% 
    mutate(station = c(StationCode))
  
  
  
  met.out<- rbind(met.out, mainTable)
  
}



met.out %>% filter(Variable == "ATemp" & station == "wqbchmet  ") %>% 
  ggplot(aes(x = date, y = mean_daily))+
  geom_point()



write.csv(met.out, file = "data/meterology_all.csv")

###################################################
########### HEATWAVE #############################
##################################################
dat<- read_csv(here::here("data", "water_quality_pep.csv"))

nar_hw<- dat %>% 
  filter(station == "nartswq", 
         Variable == "Temp") %>% 
  select(date, mean_daily) %>% 
  drop_na() %>% 
  rename(t = date, 
         temp = mean_daily)

write.csv(nar_hw, file = "data/nar_heatwave.csv")

wel_hw<- dat %>% 
  filter(station == "welinwq", 
         Variable == "Temp") %>% 
  select(date, mean_daily) %>% 
  drop_na() %>% 
  rename(t = date, 
         temp = mean_daily)

write.csv(wel_hw, file = "data/wel_heatwave.csv")

wqb_hw<- dat %>% 
  filter(station == "wqbmpwq", 
         Variable == "Temp") %>% 
  select(date, mean_daily) %>% 
  drop_na() %>% 
  rename(t = date, 
         temp = mean_daily)

write.csv(wqb_hw, file = "data/wqb_heatwave.csv")

grb_hw<- dat %>% 
  filter(station == "grbgbwq", 
         Variable == "Temp") %>% 
  select(date, mean_daily) %>% 
  drop_na() %>% 
  rename(t = date, 
         temp = mean_daily)

write.csv(grb_hw, file = "data/grb_heatwave.csv")



















