library(readr)
library(ggplot2)
library(sf)
library(SWMPr)
library(tidyverse)

codes <- SWMPr::site_codes() %>% ## Get sites
  dplyr::filter(state == c("me", "nh", "ma", "ri")) %>% # only New England sites
  dplyr::mutate(latitude = as.numeric(latitude), 
                longitude = as.numeric(longitude)*-1) # put in the correct hemisphere

site_labels <- codes %>% 
  group_by(reserve_name) %>% 
  slice(1) # selects one station in each site

#install.packages("maps")
#coordmap <- read_csv("coordmap.csv")

# Map set up 
# Set lat/lon window for maps
xmin = -72
xmax = -69
ymin = 41
ymax = 44
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)
crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
#Initial map
m <- ggplot()+
  #geom_polygon(aes(long, lat, group = group), data = map_data("usa"))+
  ggplot2::geom_sf(data = ecodata::coast) +
  ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims)+
  geom_point(aes(longitude, latitude), data = codes, color = "midnightblue")+
  geom_label(data = site_labels, aes( longitude, latitude, label = reserve_name), ## Adds site labels 
             size = 3, fontface = "bold", nudge_y = 0.18) 
#coord_quickmap(xlim = range(coordmap$lon) +c(-2,2), ylim = range(range(coordmap$lat) +c(-1,1)))

#Change labels
xbreaks <- seq(-72, -69, 1)
ybreaks <- pretty(codes$latitude)
xlabels <- paste0(abs(xbreaks), "°W")
ylabels <- paste0(abs(ybreaks), "°N")
#Title and stuff
m + scale_x_continuous(breaks = xbreaks, labels = xlabels)+ 
  scale_y_continuous(breaks = ybreaks, labels = ylabels)+ 
  labs(x = NULL, y = NULL, title = "NERRs - New England Sites")