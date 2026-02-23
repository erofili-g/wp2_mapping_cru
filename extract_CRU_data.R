library(readr)
library(tidyverse)
library(ggplot2)
library(cruts)
library(raster)# to read the cru nc files
library(ncdf4)# 
library(terra)
library(tidyterra)
library(sf) # to read shape files

rm(list=ls())
## fix margins for better plotting
#par("mar")  5.1 4.1 4.1 2.1
par(mar=c(1,1,1,1))

## load shape file with district boundaries -- you need to change the path according to where your data are

## ZAMBIA
zamb_districts<-st_read("C:/Work_extra/astra/wp2_mapping_cru/shapefiles/geoBoundaries-ZMB-ADM2-all/geoBoundaries-ZMB-ADM2.shp")

names(zamb_districts)
head(zamb_districts)
extent(zamb_districts)

# visualise country and district borders
ggplot() + 
  geom_sf(data = zamb_districts) +coord_sf() 


## convert the shapefile to vector in order to plot layer with the CRU raster datasets
zamb_vec<-vect("C:/Work_extra/astra/wp2_mapping_cru/geoBoundaries-ZMB-ADM2-all/geoBoundaries-ZMB-ADM2.shp")


### load CRU ncdef data
# create vector to serve as year index
#num_years <- rep(1:23, each=12)
###############################################################################################################################################################
########################    precipitation

## load CRU data (previously downloaded and extracted): note you need to change the path


pre_rasta01_10 <- cruts2raster("C:/Work_extra/astra/wp2_mapping_cru/cru_data/cru_2001_2024_pre/cru_ts4.08.2001.2010.pre.dat.nc", 
                            timeRange=c("2001-01-01","2010-12-31"), type='brick')

pre_rasta11_20 <- cruts2raster("C:/Work_extra/astra/wp2_mapping_cru/cru_data/cru_2001_2024_pre/cru_ts4.08.2011.2020.pre.dat.nc", 
                            timeRange=c("2011-01-01","2020-12-31"), type='brick')

pre_rasta21_24 <- cruts2raster("C:/Work_extra/astra/wp2_mapping_cru/cru_data/cru_2001_2024_pre/cru_ts4.09.2021.2024.pre.dat.nc", 
                            timeRange=c("2021-01-01","2024-12-31"), type='brick')

## put all 3 periods together
pre_rasta <- stack(pre_rasta01_10, pre_rasta11_20, pre_rasta21_24)

## crop to SA extent and district boundaries to get the correct country outline
pre_crop<-mask(crop(pre_rasta, extent(zamb_districts)), zamb_districts)

dim(pre_crop) 

# precipitation in Dec 2023 
plot(pre_crop$X2023.12.16, asp=1, axes=F, box=F)
print(pre_crop) #(nrow/ncol are the pixel size, ncell is nrow x ncol pixels and nlayers is the total number of months)

num_years<- rep(1:(nlayers(pre_crop)/12), each=12)

# total annual precipitation and then average over the whole period 

# sum annual precipitation across months for each year
pre_tot <- stackApply(pre_crop, indices=num_years, fun=sum)
print(pre_tot) # annual total  by pixel
dim(pre_tot)


# extracts values from a raster for a set of locations and fun=summarises the extracted data by geometry --> 
## calculate the average precipitation by district (averages over pixels)
pre_tot_district<-extract(pre_tot, zamb_districts, weights=T, fun=mean, na.rm=T)

# With each row a polygon (district) and each column a time step.
print(pre_tot_district)

# average annual precipitation by district
zamb_vec$pre_mean<-apply(pre_tot_district, 1, mean)

# map of average annual precipitation
ggplot()+geom_sf(zamb_vec, mapping = aes(fill=pre_mean), 
                 color = "white")+scale_fill_gradient(low="light blue", high="dark blue")


#########################################################################################################################################################################
####################  temperature
## monthly mean temperature

temp_rasta01_10 <- cruts2raster("C:/Work_extra/astra/wp2_mapping_cru/cru_data/cru_2001_2024_temp/cru_ts4.08.2001.2010.tmp.dat.nc", 
                          timeRange=c("2001-01-01","2010-12-31"), type='brick')

temp_rasta11_20 <- cruts2raster("C:/Work_extra/astra/wp2_mapping_cru/cru_data/cru_2001_2024_temp/cru_ts4.08.2011.2020.tmp.dat.nc", 
                            timeRange=c("2011-01-01","2020-12-31"), type='brick')

temp_rasta21_23 <- cruts2raster("C:/Work_extra/astra/wp2_mapping_cru/cru_data/cru_2001_2024_temp/cru_ts4.09.2021.2024.tmp.dat.nc", 
                            timeRange=c("2021-01-01","2024-12-31"), type='brick')


temp_rasta <- stack(temp_rasta01_10, temp_rasta11_20, temp_rasta21_23)
print(temp_rasta)# dimensions are ok --- missing values?

temp_crop<-mask(crop(temp_rasta, extent(zamb_districts)), zamb_districts)
print(temp_crop)

# get annual average pixel temperatures
temp_mean_an <- stackApply(temp_crop, indices=rep(1:(nlayers(temp_crop)/12), each=12), fun=mean) 
print(temp_mean_an)

#  annual average district temperature
temp_mean<-extract(temp_mean_an, zamb_districts, weights=T, fun=mean, na.rm=T)

#With each row a polygon (state) and each column a time step.
print(temp_mean)
dim(temp_mean)


## add the average district temperature 2001-2023 in the district vector data for easy plotting
zamb_vec$temp_mean<-apply(temp_mean, 1, mean)



######################################################################################################################################################################
##### average monthly max temperature

tempmax_rasta01_10 <- cruts2raster("C:/Work_extra/astra/wp2_mapping_cru/cru_data/cru_2001_2024_tmx/cru_ts4.09.2001.2010.tmx.dat.nc", 
                                timeRange=c("2001-01-01","2010-12-31"), type='brick')

tempmax_rasta11_20 <- cruts2raster("C:/Work_extra/astra/wp2_mapping_cru/cru_data/cru_2001_2024_tmx/cru_ts4.09.2011.2020.tmx.dat.nc", 
                                timeRange=c("2011-01-01","2020-12-31"), type='brick')

tempmax_rasta21_24 <- cruts2raster("C:/Work_extra/astra/wp2_mapping_cru/cru_data/cru_2001_2024_tmx/cru_ts4.09.2021.2024.tmx.dat.nc", 
                                timeRange=c("2021-01-01","2024-12-31"), type='brick')


tempmax_rasta <- stack(tempmax_rasta01_10, tempmax_rasta11_20, tempmax_rasta21_24)

print(tempmax_rasta)# dimensions are ok

tempmax_crop<-mask(crop(tempmax_rasta, extent(zamb_districts)), zamb_districts)
print(tempmax_crop)

# get average maximum temperatures
tempmax_mean_an <- stackApply(tempmax_crop, indices=rep(1:(nlayers(tempmax_crop)/12), each=12), fun=mean) 
print(tempmax_mean_an)

#  annual average district max temperature
tempmax_mean<-extract(tempmax_mean_an, zamb_districts, weights=T, fun=mean, na.rm=T)

#With each row a polygon (state) and each column a time step.
print(tempmax_mean)

## add the average district temperature 2001-2024 in the district vector data for easy plotting
zamb_vec$tempmax<-apply(tempmax_mean, 1, mean)

#######################################################################################################################
#### minimum temperature


tempmin_rasta01_10 <- cruts2raster("C:/Work_extra/astra/wp2_mapping_cru/cru_data/cru_2001_2024_tmn/cru_ts4.09.2001.2010.tmn.dat.nc", 
                                   timeRange=c("2001-01-01","2010-12-31"), type='brick')

tempmin_rasta11_20 <- cruts2raster("C:/Work_extra/astra/wp2_mapping_cru/cru_data/cru_2001_2024_tmn/cru_ts4.09.2011.2020.tmn.dat.nc", 
                                   timeRange=c("2011-01-01","2020-12-31"), type='brick')

tempmin_rasta21_24 <- cruts2raster("C:/Work_extra/astra/wp2_mapping_cru/cru_data/cru_2001_2024_tmn/cru_ts4.09.2021.2024.tmn.dat.nc", 
                                   timeRange=c("2021-01-01","2024-12-31"), type='brick')


tempmin_rasta <- stack(tempmin_rasta01_10, tempmin_rasta11_20, tempmin_rasta21_24)
print(tempmin_rasta) # check dimensions 

## crop to Zambia district extent
tempmin_crop<-mask(crop(tempmin_rasta, extent(zamb_districts)), zamb_districts)
print(tempmin_crop)

# get average annual minimum temperatures
tempmin_mean_an <- stackApply(tempmin_crop, indices=rep(1:(nlayers(tempmin_crop)/12), each=12), fun=mean) 
print(tempmin_mean_an)

#  annual average district min temperature
tempmin_mean<-extract(tempmin_mean_an, zamb_districts, weights=T, fun=mean, na.rm=T)

#With each row a polygon (state) and each column a time step.
print(tempmin_mean)

## add the average district temperature 2001-2023 in the district vector data for easy plotting
zamb_vec$tempmin<-apply(tempmin_mean, 1, mean)


#####################################################################################################################################################################

# save shapefile

writeVector(zamb_vec, "C:/Work_extra/astra/wp2_mapping_cru/shapefiles/Zambia_cru.shp", overwrite=T)
# save csv
names(zamb_vec)
write.csv(zamb_vec, file="C:/Work_extra/astra/wp2_mapping_cru/cru_data_Zambia.csv", row.names = F)


######################################################################################################################################################################
################################### MAPS   ################################################################################################################

## one district has no data (all values are zero?) Peter to check -- I think this district needs to excluded/set to missing so that it doesn't affect the scale

ggplot()+
  geom_sf(zamb_vec, mapping = aes(fill=pre_mean), color = "white")+
  scale_fill_gradient(low="light blue", high="dark blue")+
  theme(legend.position="right")+
  ggtitle("Average annual precipitation 2001-2024")+
  guides(fill = guide_legend(title = "mm/year"))
  

ggplot()+
  geom_sf(zamb_vec, mapping = aes(fill=temp_mean), color = "white")+
  scale_fill_gradient(low="yellow", high="red")+
  theme(legend.position="right")+
  ggtitle("Average temperature 2001-2024")+
  guides(fill = guide_legend(title = "Degrees Celsius"))
  

ggplot()+
  geom_sf(zamb_vec, mapping = aes(fill=tempmin), color = "white")+
  scale_fill_gradient(low="orange", high="brown")+
  theme(legend.position="right")+
  ggtitle("Average min temperature 2001-2024")+
  guides(fill = guide_legend(title = "mm/year"))


