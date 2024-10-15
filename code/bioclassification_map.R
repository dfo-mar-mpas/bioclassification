## make a map for the readme

#load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(viridis)
library(patchwork)

#set working directory
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#load in the file depicting 

match_table <- read.csv("output/open data files/name_table.csv")%>%
               mutate(site_index = paste(region,cl,sep="-"))%>%
               dplyr::select(classification,site_index)

#load shapefiles
clusters <- read_sf("output/open data files/bioclassification_clusters.shp")%>%
            mutate(fill_colour = case_when(
              bioregion == "MAR" & cl == 1 ~ '#5A283E', #Maritimes region cluster fills
              bioregion == "MAR" & cl == 2 ~ '#F2300F',
              bioregion == "MAR" & cl == 3 ~ '#649373',
              bioregion == "MAR" & cl == 4 ~ '#1B5656',
              bioregion == "MAR" & cl == 5 ~ '#9FA682',
              bioregion == "MAR" & cl == 6 ~ '#E1BD6D',
              
              bioregion == "NGSL" & cl == 1 ~ '#C93312',#Northern Gulf of St. Lawrence fills
              bioregion == "NGSL" & cl == 2 ~ '#FAEFD1',
              bioregion == "NGSL" & cl == 3 ~ '#DC863B',
              
              bioregion == "NL" & cl == 1 ~ '#85B5C0',#Newfoundland Shelves fills
              bioregion == "NL" & cl == 2 ~ '#3F7F92',
              bioregion == "NL" & cl == 3 ~ '#E1AF00',
              bioregion == "NL" & cl == 4 ~ '#3F0A25',
              bioregion == "NL" & cl == 5 ~ '#E23B35',
              
              bioregion == "SGSL" & cl == 1 ~ '#899DA4', #Southern Gulf of St. Lawerence fills
              bioregion == "SGSL" & cl == 2 ~ '#002F2F',
              bioregion == "SGSL" & cl == 3 ~ '#7F1100',
              bioregion == "SGSL" & cl == 4 ~ '#725900',
              
              TRUE ~ NA),
              site_index = paste(region,cl,sep="-"))%>%
              left_join(.,match_table)
            
uncertainty <- read_sf("output/open data files/bioclassification_uncertain_clusters.shp")

#create a basemap
basemap <- ne_states(country = "Canada",returnclass = "sf")%>%
  dplyr::select(name_en,geometry)%>%
  st_as_sf()%>%
  st_union()%>%
  st_as_sf()%>%
  mutate(country="Canada")%>%
  rbind(.,ne_states(country = "United States of America",returnclass = "sf")%>%
          dplyr::select(name_en,geometry)%>%
          st_as_sf()%>%
          st_union()%>%
          st_as_sf()%>%
          mutate(country="US"),
        ne_states(country = "Greenland",returnclass = "sf")%>%
          dplyr::select(name_en,geometry)%>%
          st_as_sf()%>%
          st_union()%>%
          st_as_sf()%>%
          mutate(country="Greenland"),
        ne_states(country = "Saint Pierre and Miquelon",returnclass = "sf")%>%
          dplyr::select(name_en,geometry)%>%
          st_as_sf()%>%
          st_union()%>%
          st_as_sf()%>%
          mutate(country="France"))%>%
  st_transform(CanProj)

##create regional cluster maps

mar_clusters <- clusters%>%
                filter(bioregion == "MAR")%>%
                mutate(classification = factor(classification,levels=classification))%>%
                arrange(classification)
              

mar_lims <- mar_clusters%>%
            st_bbox()%>%
            st_as_sfc()%>%
            st_buffer(50*1000)%>%
            st_bbox()

mar_map <- ggplot()+
          geom_sf(data=basemap)+
          geom_sf(data=mar_clusters,aes(fill=classification))+
          scale_fill_manual(values  = mar_clusters%>%pull(fill_colour))+
          theme_bw()+
          coord_sf(xlim=mar_lims[c(1,3)],ylim=mar_lims[c(2,4)])
        
