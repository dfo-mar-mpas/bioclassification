## make a map for the readme

#load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(viridis)
library(patchwork)
library(ggspatial)
library(ggnewscale)

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

#common plotting function
cluster_map <- function(x,basemap=basemap,buf=50){
  
  # x is the region flagged out by the 'bioregion' variable
  # basemap is the rnaturalearth shapefile for a global coastline from the envrionment
  # buf is a spatial plotting buffer in km
  
  temp_clusters <- clusters%>%
    filter(bioregion == x)%>%
    mutate(classification = factor(classification,levels=classification))%>%
    arrange(classification)
  
  temp_lims <- temp_clusters%>%
    st_bbox()%>%
    st_as_sfc()%>%
    st_buffer(buf*1000)%>%
    st_bbox()
  
  temp_map <- ggplot()+
    geom_sf(data=basemap)+
    geom_sf(data=temp_clusters,aes(fill=classification))+
    scale_fill_manual(values  = temp_clusters%>%pull(fill_colour))+
    theme_bw()+
    coord_sf(xlim=temp_lims[c(1,3)],ylim=temp_lims[c(2,4)])+
    annotation_scale(location="bl")+
    theme(legend.title = element_blank())
  
  return(temp_map)
}

#regional Region ----------


mar_map <- cluster_map("MAR",basemap)
sgsl_map <- cluster_map("SGSL",basemap)
ngsl_map <- cluster_map("NGSL",basemap)
nl_map <- cluster_map("NL",basemap)

combo_map <- mar_map + 
             sgsl_map + 
             ngsl_map + 
             nl_map + 
             plot_layout(ncol=2,nrow=2) & 
             theme(legend.position = "none",axis.text = element_blank())

ggsave("output/combo_map.jpg",combo_map,height=8,width=8,units="in",dpi=300)


all_lims <- clusters%>%
            st_bbox()%>%
            st_as_sfc()%>%
            st_buffer(30*1000)%>%
            st_bbox()

all_clusters <- clusters%>%
                mutate()
                group_by(bioregion)%>%
                mutate( classification = case_when(classification == "Slope" & bioregion == "MAR" ~ "Slope (MAR)", #space to differentiate from NL
                                                   classification == "Laurentian Channel/Shelf Break" & bioregion == "MAR" ~ "Laurentian Channel/Shelf Break (MAR)",
                                                   classification == "Slope" & bioregion == "NL" ~ "Slope (NL)", #space to differentiate from MAR
                                                   classification == "Laurentian Channel/Shelf Break" & bioregion == "NL" ~ "Laurentian Channel/Shelf Break (MAR)",
                                                   TRUE ~ classification),
                        classification = factor(classification,levels=classification))%>%
                arrange(classification)%>%
                ungroup()

classification_colors <- all_clusters %>%
                          distinct(classification, fill_colour) %>%  # Get unique combinations of classification and color
                          deframe() 
all_map <- ggplot()+
  geom_sf(data=basemap)+
  
  geom_sf(data=all_clusters%>%filter(bioregion=="MAR"),aes(fill=classification))+
  scale_fill_manual(values = classification_colors)+
  labs(fill="Scotian Shelf-Bay of Fundy")+
  new_scale_fill()+
  
  geom_sf(data=all_clusters%>%filter(bioregion=="SGSL"),aes(fill=classification))+
  scale_fill_manual(values = classification_colors)+
  labs(fill="Southern Gulf")+
  new_scale_fill()+
  
  geom_sf(data=all_clusters%>%filter(bioregion=="NGSL"),aes(fill=classification))+
  scale_fill_manual(values = classification_colors)+
  labs(fill="Northern Gulf")+
  new_scale_fill()+
  
  geom_sf(data=all_clusters%>%filter(bioregion=="NL"),aes(fill=classification))+
  scale_fill_manual(values = classification_colors)+
  labs(fill="Newfoundland Shelves")+
 
  coord_sf(xlim=all_lims[c(1,3)],ylim=all_lims[c(2,4)])+
  annotation_scale(location="bl")+
  annotation_north_arrow(location="tr")+
  theme_bw()

ggsave("output/allmap.jpg",all_map,height=8*1.5,width=6*1.5,units="in",dpi=300)
