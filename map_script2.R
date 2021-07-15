# NERRS MAPs

## Map
library(sf)
library(SWMPr)
library(tidyverse)
library(ggrepel)
library(raster)

grb.codes <- SWMPr::site_codes() %>%
  dplyr::filter(nerr_site_id == c("grb"))
nar.codes <- SWMPr::site_codes() %>%
  dplyr::filter(nerr_site_id == c("nar"))
wel.codes <- SWMPr::site_codes() %>%
  dplyr::filter(nerr_site_id == c("wel"))
wqb.codes <- SWMPr::site_codes() %>%
  dplyr::filter(nerr_site_id == c("wqb"))

codes <- rbind(grb.codes, nar.codes, wel.codes, wqb.codes) %>% 
  dplyr::mutate(latitude = as.numeric(latitude), 
                longitude = as.numeric(longitude)*-1) 
site_labels <- codes %>% 
  group_by(reserve_name) %>% 
  slice(1) 

crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77+x_0=0
+y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
coast_shp <- rgdal::readOGR(file.path(here::here("data-raw","coast", "USA.shp")), verbose = F)
crs(coast_shp) <- crs
coast_sf <- as(coast_shp, "sf")

xmin = -72
xmax = -69
ymin = 41
ymax = 44
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)
crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
m <- ggplot()+
  ggplot2::geom_sf(data = coast_sf) +
  ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims)+
  geom_point(aes(longitude, latitude), data = codes, color = "midnightblue")+
  geom_label_repel(data = site_labels, aes( longitude, latitude, label = reserve_name),
                   size = 3) 
xbreaks <- seq(-72, -69, 1)
ybreaks <- pretty(codes$latitude)
xlabels <- paste0(abs(xbreaks), "°W")
ylabels <- paste0(abs(ybreaks), "°N")
m + scale_x_continuous(breaks = xbreaks, labels = xlabels)+ 
  scale_y_continuous(breaks = ybreaks, labels = ylabels)+ 
  labs(x = NULL, y = NULL, title = "NERRs - New England Sites")
