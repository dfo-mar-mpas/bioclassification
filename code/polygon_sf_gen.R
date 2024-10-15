#load libraries
library(tidyverse)
library(terra)
library(sf)
library(rnaturalearth)
library(raster)
library(purrr)

#set working directory
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#custom function to convert rasters to polygon shapefiles
sf_cluster_fun <- function(x,region = NA,target_crs=CanProj){
  
  #x = filepath 
  #region = region abbreviation if not specified one will be pulled from the filepath
  #target_crs= the projection for the clusters
  
  target_crs <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  if(is.na(region)){region = str_split(basename(x),"_")[[1]][1]}
  
  cluster_out <- raster(x)%>% # predicted distribution of 6 major clusters
    as(., 'SpatialPolygonsDataFrame') %>% # convert to sf object
    st_as_sf()%>%
    st_transform(target_crs)%>%
    dplyr::rename(cl = category)%>%
    group_by(cl)%>%
    summarize(geometry=st_union(geometry))%>%
    ungroup()%>%
    mutate(region = region)
  
  return(cluster_out)
  
}
sf_cluster_uncertainty <- function(x,region=NA,target_crs=CanProj){
  
  if(is.na(region)){region = str_split(basename(x),"_")[[1]][1]}
  
  uncertain_cluster <- read_sf(x)%>%
                       st_transform(CanProj)%>%
                       rename(prop_assignment = 1)%>%
                       summarize(geometry=st_union(geometry))%>%
                       st_simplify()%>%
                       mutate(region = region)
  
  return(uncertain_cluster)
  
}

#read in the output form the analysis
filenames <- dir("output/",full.names = TRUE)[grepl("PredClust_map",dir("Output/")) & !grepl("xml",dir("Output/"))]

#convert to clusters and updated metadata
clusters <- map(filenames, sf_cluster_fun)%>%
            do.call("rbind",.)%>%
            mutate(bioregion = case_when( #clean up the names and add the specified cluster names from the paper
                            region == 'Gulf' ~ "SGSL",
                            region == "Maritimes" ~ "MAR",
                            region == "QC" ~ "NGSL",
                            TRUE ~ "NL"),
                   classification = case_when(
                            bioregion == 'SGSL' & cl == 1 ~ 'Magdalen Shallows',
                            bioregion == 'SGSL' & cl == 2 ~ 'Inshore/Magdalen Is.',
                            bioregion == 'SGSL' & cl == 3 ~ 'Laurentian Channel',
                            bioregion == 'SGSL' & cl == 4 ~ "Northumberland Strait/St. George's Bay",
                            bioregion == 'MAR' & cl == 6 ~ 'WSS/Outer BoF',
                            bioregion == 'MAR' & cl == 5 ~ 'WSS: Banks/Inner BoF',
                            bioregion == 'MAR' & cl == 4 ~ 'ESS: Banks',
                            bioregion == 'MAR' & cl == 3 ~ 'ESS',
                            bioregion == 'MAR' & cl == 2 ~ 'Laurentian Channel/Shelf Break',
                            bioregion == 'MAR' & cl == 1 ~ 'Slope',
                            bioregion == 'NL' & cl == 1 ~ 'Inner Shelf',
                            bioregion == 'NL' & cl == 2 ~ 'Outer Shelf',
                            bioregion == 'NL' & cl == 3 ~ 'Grand Banks',
                            bioregion == 'NL' & cl == 4 ~ 'Slope',
                            bioregion == 'NL' & cl == 5 ~ 'Laurentian Channel/Shelf Break',
                            bioregion == 'NGSL' & cl == 1 ~ 'Deep Channels',
                            bioregion == 'NGSL' & cl == 3 ~ 'Channel Heads & Slopes',
                            bioregion == 'NGSL' & cl == 2 ~ 'Shallow Banks & Straits',
                            TRUE ~ NA_character_))

#get the uncertainty shapefiles for each 

uncertain_files <- dir("output/",full.names = TRUE)[grepl(".shp",dir("output/"))]

uncertain_clusters <- map(uncertain_files,sf_cluster_uncertainty)%>%
                      do.call("rbind",.)%>%
                      mutate(bioregion = case_when( #clean up the names and add the specified cluster names from the paper
                        region == 'Gulf' ~ "SGSL",
                        region == "Maritimes" ~ "MAR",
                        region == "QC" ~ "NGSL",
                        TRUE ~ "NL"),
                        classification="Probability Assignment < 0.7")

## write the shapefiles

#note the names in 'classification' are too long for shapefile format. These will be made available in a matching table
write_sf(clusters%>%dplyr::select(region,bioregion,cl,geometry),"output/open data files/bioclassification_clusters.shp")
write_sf(uncertain_clusters%>%dplyr::select(region,bioregion,geometry),"output/open data files/bioclassification_uncertain_clusters.shp")

#matching table
match_table <- clusters%>%
               data.frame()%>%
               dplyr::select(region,bioregion,cl,classification)

write.csv(match_table,file="utput/open data files/name_table.csv",row.names=FALSE)

          