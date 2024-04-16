mainfolder="./"
setwd(mainfolder)

# import packages
library(sf)          
library(terra)      
library(spData)        
library(spDataLarge)
library(tidyverse)
library(ggplot2)

# import data
hf_zone0 <- read_sf(dsn = "./data/assigment_geo/G341022合肥经济技术开发区.txt")
hf_zone1 <- read_sf(dsn = "./data/assigment_geo/G342020合肥高新技术产业开发区区块一.txt")
hf_zone2 <- read_sf(dsn = "./data/assigment_geo/G342020合肥高新技术产业开发区区块二.txt")
print(st_crs(hf_zone0))

companies <- read.csv("./data/assigment_geo/hefei.txt", sep="\t", header = TRUE)
companies_sf <- companies |> 
  st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4326")

# 判断是否有重合区域
intersections_01 <- st_intersects(hf_zone0, hf_zone1)
intersections_02 <- st_intersects(hf_zone0, hf_zone2)
intersections_12 <- st_intersects(hf_zone1, hf_zone2)
plot(st_intersection(hf_zone0, hf_zone2))
# 0，2有重合
combined_zones <- rbind(hf_zone0, hf_zone1, hf_zone2)
unified_zone <- st_union(st_geometry(combined_zones))
intersections_companies <- st_union(st_geometry(rbind(unified_zone, companies_sf)))

hf_zone <- ggplot() +
  geom_sf(data = hf_zone0, fill = "blue", alpha = 0.3, color = gray(0.5)) +
  geom_sf(data = hf_zone1, fill = "red", alpha = 0.3, color = gray(0.5)) +
  geom_sf(data = hf_zone2, fill = "green", alpha = 0.3, color = gray(0.5)) +
  geom_sf(data = unified_zone, color = "black", fill = NA) +
  theme_minimal() +
  labs(title = "合肥市开发区")+
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5, hjust = 1),
        axis.ticks.x = element_blank(), axis.title.y = element_blank(),
        axis.text.y =  element_text(size = 5), axis.ticks.y = element_blank()
        )
print(hf_zone)


# 计算在开发区内的企业数量
intersections_companies <- st_intersects(unified_zone, companies_sf)
companies_in_zone <- companies_sf[unlist(intersections_companies), ]
count_in_zone <- sum(st_intersects(unified_zone, companies_sf, sparse = FALSE))

# 画出开发区内的企业
companies_in_hf_zone <- ggplot() +
  geom_sf(data = unified_zone, fill = "blue", alpha = 0.3, color = gray(0.5)) +
  geom_sf(data = companies_in_zone, color = "darkgreen", alpha = 0.3, size = 0.5) +
  theme_minimal() +
  labs(title = "合肥市开发区")+
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5, hjust = 1),
        axis.ticks.x = element_blank(), axis.title.y = element_blank(),
        axis.text.y =  element_text(size = 5), axis.ticks.y = element_blank()
  )
print(companies_in_hf_zone)
ggsave(companies_in_hf_zone, file='companies_in_hf_zone.jpg', width=8, height=4,scale=2.5,dpi=100)

# 计算开发区周边企业数量
unified_zone_utm <- st_transform(unified_zone, crs = 32651)
companies_sf_utm <- st_transform(companies_sf, crs = 32651)
buffer_1km <- st_buffer(unified_zone_utm, dist = 1000)
plot(buffer_1km)
count_1km <- sum(st_intersects(buffer_1km, companies_sf_utm, sparse = FALSE))

buffer_3km <- st_buffer(unified_zone_utm, dist = 3000)
plot(buffer_3km)
count_3km <- sum(st_intersects(buffer_3km, companies_sf_utm, sparse = FALSE))

buffer_5km <- st_buffer(unified_zone_utm, dist = 5000)
plot(buffer_5km)
count_5km <- sum(st_intersects(buffer_5km, companies_sf_utm, sparse = FALSE))
