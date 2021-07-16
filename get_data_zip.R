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


nutrients_pep<- nut.out %>% 
  filter(station == c("grbgbnut  " , "nartsnut  ","wqbmpnut  " )) ### Doesn't include Wells yet


write.csv(nutrients_pep, file = "data/nutrient_pep.csv")


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


wq_pep<- wq.out %>% 
  filter(station == c("grbgbwq  " , "nartswq  ","wqbmpwq  " )) ### Doesn't include Wells yet


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



met.out %>% filter(Variable == "ATemp" & station == "wellfmet  ") %>% 
  ggplot(aes(x = date, y = mean_daily))+
  geom_point()



write.csv(met.out, file = "data/meterology_all.csv")























