#load all the library needed
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(rgdal)
library(stringr)
library(tidyverse)
library(sf)
library(ggplot2)
library(plyr)
library(janitor)
library(FNN)
library(spdep)
library(magrittr)
library(tidyr)

# read in civic artworks  data 
civic0 <- st_read("Final_data/Civic Art Collection/geo_export_e7c94114-a818-4d9c-8f4e-cfd40f0ec73f.shp")
#transform  civic artworks  data' crs to San Francisco's local crs
#the column 'current_lo' contains the location information of the civic artworks, filter out the 'Public Display' ones.
civic <- civic0 %>% 
  st_transform(., 7131) %>% 
  filter(current_lo=='Public Display')


#read in privately owned public Artworks data(from 1% Art Program)
private_csv <- read_csv("Final_data/Public_Art__from_1__Art_Program.csv") 
#split geometry column in privately owned public art data into two columns-'latitude' and 'longitude'
private0 <- private_csv %>% tidyr::extract(the_geom, c("Latitude", "Longitude"), "\\(([^,]+), ([^)]+)\\)")
#make the latitude' and 'longitude' columns numeric
private0$Latitude <- as.numeric(as.character(private0$Latitude))
private0$Longitude <- as.numeric(as.character(private0$Longitude))
# change privately owned public art data into sf object and transform the crs to to San Francisco's local crs
private <- st_as_sf(private0, coords = c("Longitude", "Latitude"),crs=4326) %>% 
  st_transform(.,7131)

#read in boundaries of census block groups in San Francisco
#transform the crs to to San Francisco's local crs
a <- st_read("Final_data/cb_2019_06_bg_500k/cb_2019_06_bg_500k.shp")
a2 <- a %>%
  filter(str_detect(GEOID, "6075"))
a3<- a2 %>% st_transform(., 7131)
#drop the 060750179021 block group (includes an artificial island away form San Francisco mainland and a small uninhabited tip of western Alameda Island.
#drop the 060750604002 block group (includes The Farallon Islands lies 348 km outside the San Francisco mainland)
#drop other weird geometry
a4 <- a3[!a3$GEOID=='060750179021',]
a4 <- a4[!a4$GEOID=='060816075001',]
a4 <- a4[!a4$GEOID=='060759804011',]
a4 <- a4[!a4$GEOID=='060590756075',]

#read in block group population data from 2015-2019 American Community Survey 5-Year Estimates
pop2019 <- read_csv("Final_data/ACSDT5Y2019_block _group_population.csv")
#join the block group population attribute to the block group boundaries data by matching identical  block group ID 
blockgroupMerged <- a4 %>% 
  left_join(pop2019, 
            by = c("GEOID" = "idFixed"))

#add the centroid location of every block group into the block group data
blockgroupMerged$centroids<- st_centroid(blockgroupMerged) %>% st_geometry()


#get the matrix object of point coordinate for civic artworks, privately owned public art data and block group centroid
private_point <- private %>% st_coordinates()
centroid_point <- blockgroupMerged$centroids %>% st_coordinates()
civic_point <- civic %>% st_coordinates()

#For each block group centroid,  calculate the its distance to the nearest privately owned public artwork point
distance_private <- get.knnx(private_point, centroid_point,k=1)
distance_private_df <- as.data.frame(do.call(cbind, distance_private ))
#For each block group centroid,  calculate the its distance to the nearest civic art point
distance_civic <- get.knnx(civic_point, centroid_point,k=1)
distance_civic_df <- as.data.frame(do.call(cbind, distance_civic )) 

#add the distance to the nearest privately owned public art point as a new column to the block group data
blockgroupMerged2 <- blockgroupMerged %>% 
  mutate(mindistance_private=(distance_private_df$V2))
#add the distance to the nearest Civic Art point as a new column to the block group data
blockgroupMerged2 <- blockgroupMerged2 %>% 
  mutate(mindistance_civic=(distance_civic_df$V2))

#measure the scarcity of civic artworks, add as a new column to the block group data
blockgroupMerged2 <- blockgroupMerged2 %>% 
  mutate(scarcity_civic=(Estimate_Total*mindistance_civic))
#measure the scarcity of privately owned public artworks, add as a new column to the block group data
blockgroupMerged2 <- blockgroupMerged2 %>% 
  mutate(scarcity_private=(Estimate_Total*mindistance_private))

#========================= a series maps =======================

# map of the boundaries of cencus block groups
blockgroup_b <- tm_shape(blockgroupMerged2) +
  tm_borders(lwd = 1.5)+
  tm_compass(position = c("LEFT", "TOP"),size=1)+
  tm_scale_bar(position = c("LEFT", "TOP"), width=0.15)+
  tm_layout(frame = FALSE)
blockgroup_b 


#map of distribution of civic artworks
tmap_mode("plot")
CAdistribution <- tm_shape(blockgroupMerged2) +
  tm_borders(lwd = 1.5)+
  tm_shape(civic) +
  tm_symbols(col = "red",border.col = "white",size = 0.2)+
  tm_add_legend('symbol', 
                col = "red",
                border.col = "white",
                labels = "Civic Artworks", size=0.6)+
  tm_compass(position = c("LEFT", "TOP"),size=1)+
  tm_scale_bar(position = c("LEFT", "TOP"), width=0.15)+
  tm_layout(frame = FALSE)
CAdistribution


#map of distribution of privately owned public artworks
tmap_mode("plot")
POPAdistribution <- tm_shape(blockgroupMerged2) +
  tm_borders(lwd = 1.5)+
  tm_shape(private) +
  tm_symbols(col = "blue",border.col = "white",size = 0.2)+
  tm_add_legend('symbol', 
                col = "blue",
                border.col = "white",
                labels = "Privately Owned Public Artworks", size=0.6)+
  tm_compass(position = c("LEFT", "TOP"),size=1)+
  tm_scale_bar(position = c("LEFT", "TOP"), width=0.15)+
  tm_layout(frame = FALSE)
POPAdistribution


# map  of the census block group population distribution
tmap_mode("plot")
t1 <- tm_shape(blockgroupMerged2) +
  tm_polygons("Estimate_Total",style="jenks",
              palette="Greys")+
  tm_layout(legend.position= c("right", "top"))+
  tm_compass(position = c("LEFT", "TOP"),size=1)+
  tm_scale_bar(position = c("LEFT", "TOP"), width=0.15)+
  tm_layout(frame = FALSE)
t1


#map of distance to the nearest privately owned public artworks
t2 <- tm_shape(blockgroupMerged2) +
  tm_polygons("mindistance_private",style="jenks",
              palette="-Blues")+
  tm_layout(legend.position= c("RIGHT", "TOP"),legend.width=0.2)+
  tm_compass(position = c("LEFT", "TOP"),size=1)+
  tm_scale_bar(position = c("LEFT", "TOP"), width=0.15)+
  tm_layout(frame = FALSE)
t2


#map of istance to the nearest civic artworks
t3 <- tm_shape(blockgroupMerged2) +
  tm_polygons("mindistance_civic",style="jenks",
              palette="-Reds")+
  tm_layout(legend.position= c("RIGHT", "TOP"),legend.width=0.2,legend.text.size=0.7)+
  tm_compass(position = c("LEFT", "TOP"),size=1.8,text.size=0.6)+
  tm_scale_bar(position = c("LEFT", "TOP"), width=0.15,text.size=0.7)+
  tm_layout(frame = FALSE)
t3



#===============create a series of histograms===================
# histogram of population
hist_pop <- ggplot(blockgroupMerged2, 
                   aes(x=(Estimate_Total))) + 
  geom_histogram(color="white", binwidth = 500)+
  scale_x_continuous(breaks = seq(0, 12000, 2000))+
  scale_y_continuous(breaks = seq(0, 200, 20))+
  labs(title="Histogram of census block group population", 
       x="Population of census block group", 
       y="Count of census block group", size=5)
hist_pop


#histogram of the distance to the nearest POPA for census block group centroid
hist_mindistance_private <- ggplot(blockgroupMerged2, 
                                   aes(x=(mindistance_private))) + 
  geom_histogram(color="white",binwidth = 500)+
  scale_x_continuous(breaks = seq(0, 9000, 1000),
                     limits=c(0, 9000))+
  labs(title="Histogram of the distance to the nearest POPA for census block group centroid", 
       x="Distance to the nearest POPA (meter)", 
       y="Count of census block group")
hist_mindistance_private



#histogram of the distance to the nearest CA point for census block group centroid
hist_mindistance_civic <- ggplot(blockgroupMerged2, 
                                 aes(x=(mindistance_civic))) + 
  geom_histogram(color="white",binwidth = 100)+
  scale_x_continuous(breaks = seq(0, 2200, 500))+
  labs(title="Histogram of the distance to the nearest CA for census block group centroid", 
       x="Distance to the nearest CA (meter)", 
       y="Count of census block group")
hist_mindistance_civic



#============calculate Bivariate Moran’s I and map the spatial correlation============================
# ===========(the codes of this part is offered by rafapereirabr: https://gist.github.com/rafapereirabr/5348193abf779625f5e8c5090776a228)============================

# Variables to use in the correlation: scarcity of civic artworks and scarcity of privately owned public artworks of each block group
x <- blockgroupMerged2$scarcity_civic
y <- blockgroupMerged2$scarcity_private

# Bivariate Moran's I
moran_I <- function(x, y = NULL, W){
  if(is.null(y)) y = x
  
  xp <- scale(x)[, 1]
  yp <- scale(y)[, 1]
  W[which(is.na(W))] <- 0
  n <- nrow(W)
  
  global <- (xp%*%W%*%yp)/(n - 1)
  local  <- (xp*W%*%yp)
  
  list(global = global, local  = as.numeric(local))
}


# Permutations for the Bivariate Moran's I
simula_moran <- function(x, y = NULL, W, nsims = 2000){
  
  if(is.null(y)) y = x
  
  n   = nrow(W)
  IDs = 1:n
  
  xp <- scale(x)[, 1]
  W[which(is.na(W))] <- 0
  
  global_sims = NULL
  local_sims  = matrix(NA, nrow = n, ncol=nsims)
  
  ID_sample = sample(IDs, size = n*nsims, replace = T)
  
  y_s = y[ID_sample]
  y_s = matrix(y_s, nrow = n, ncol = nsims)
  y_s <- (y_s - apply(y_s, 1, mean))/apply(y_s, 1, sd)
  
  global_sims  <- as.numeric( (xp%*%W%*%y_s)/(n - 1) )
  local_sims  <- (xp*W%*%y_s)
  
  list(global_sims = global_sims,
       local_sims  = local_sims)
}

# Adjacency Matrix (Queen)
nb <- poly2nb(blockgroupMerged2)
lw <- nb2listw(nb, style = "B", zero.policy = T)
W  <- as(lw, "symmetricMatrix")
W  <- as.matrix(W / Matrix::rowSums(W))
W[which(is.na(W))] <- 0

# Calculating the index and its simulated distribution
# for global and local values
m <- moran_I(x, y, W)

# Global Moral
global_moran <- m[[1]][1]


# Local values
m_i <- m[[2]] 

# local simulations
local_sims <- simula_moran(x, y, W)$local_sims

# global pseudo p-value  
# get all simulated global moran
global_sims <- simula_moran(x, y, W)$global_sims

# Proportion of simulated global values taht are higher (in absolute terms) than the actual index 
moran_pvalue <- sum(abs(global_sims) > abs( global_moran )) / length(global_sims)


# Identifying the significant values 
alpha <- .05  # for a 95% confidence interval
probs <- c(alpha/2, 1-alpha/2)
intervals <- t( apply(local_sims, 1, function(x) quantile(x, probs=probs)))
sig       <- ( m_i < intervals[,1] )  | ( m_i > intervals[,2] )

# Preparing for plotting
# Convert shape file into sf object
map_sf     <- blockgroupMerged2
map_sf$sig <- sig

# Identifying the LISA clusters
xp <- scale(x)[,1]
yp <- scale(y)[,1]

patterns <- as.character( interaction(xp > 0, W%*%yp > 0) )
patterns <- patterns %>% 
  str_replace_all("TRUE","High") %>% 
  str_replace_all("FALSE","Low")

patterns[map_sf$sig==0] <- "Not significant"
map_sf$patterns <- patterns

# Rename Bivariate Moran's I clusters
map_sf$patterns2 <- factor(map_sf$patterns, levels=c("High.High", "High.Low", "Low.High", "Low.Low", "Not significant"),
                           labels=c("High scarcity of CA (Low access to CA) - High scarcity of POPA (Low access to POPA)", 
                                    "High scarcity of CA (Low access to CA) - Low scarcity of POPA (High access to POPA)", 
                                    "Low scarcity of CA (High access to CA) - High scarcity of POPA (Low access to POPA)",
                                    "Low scarcity of CA (High access to CA)- Low scarcity of POPA (High access to POPA)", 
                                    "Not significant"))

### PLOT
ggplot() +
  geom_sf(data=map_sf, aes(fill=patterns2), color="NA") +
  scale_fill_manual(values = c("red", "pink", "light blue", "dark blue", "grey80")) + 
  guides(fill = guide_legend(title="Bivariate Moran's I clusters")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())





#============bivariate choropleths mapping============================
# ====(the codes of this part is offered by Timo grossenbacher: https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/)=========

# create 3 buckets for scarcity of civic artworks
quantiles_gini <- blockgroupMerged2 %>%
  pull(scarcity_civic) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for scarcity of privately owned artworks
quantiles_mean <- blockgroupMerged2 %>%
  pull(scarcity_private) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create color scale that encodes two variables
# red for scarcity of CA and blue for scarcity of POPA
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", # high scarcity of CA, high scarcity of POPA
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low scarcity of CA, high scarcity of POPA
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium scarcity of CA, medium scarcity of POPA
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high scarcity of CA, low scarcity of POPA
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" # low scarcity of CA, low scarcity of POPA
) %>%
  gather("group", "fill")

# create a new copy of blockgroupMerged2 to work on
# cut into groups defined above and join fill
blockgroupMerged3 <- blockgroupMerged2
blockgroupMerged3 %<>%
  mutate(
    civic_quantiles = cut(
      scarcity_civic,
      breaks = quantiles_gini,
      include.lowest = TRUE
    ),
    private_quantiles = cut(
      scarcity_private,
      breaks = quantiles_mean,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      as.numeric(civic_quantiles), "-",
      as.numeric(private_quantiles)
    )
  ) %>%
  # join the actual hex values per "group"
  left_join(bivariate_color_scale, by = "group")


# create map
map <- ggplot(
  # use the same dataset as before
  data = blockgroupMerged3
) +
  # use the "alpha hack" (as the "fill" aesthetic is already taken)
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = F) + # suppress legend
  # color municipalities according to their scarcity of CA & scarcity of POPA combination
  geom_sf(
    aes(
      fill = fill
    ),
    # use thin white stroke for municipalities
    color = "white",
    size = 0.1
  ) +
  # as the sf object municipality_prod_geo has a column with name "fill" that contains the literal color as hex code for each municipality, we can use scale_fill_identity here
  scale_fill_identity()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())
map

# separate the groups
bivariate_color_scale %<>%
  separate(group, into = c("scarcity_civic", "scarcity_private"), sep = " - ") %>%
  mutate(scarcity_civic = as.integer(scarcity_civic),
         scarcity_private = as.integer(scarcity_private))
#map the legend
legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = scarcity_civic,
      y = scarcity_private,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x="Higher scarcity of CA  >>>>>",
       y="Higher scarcity of POPA  >>>>>")+
  # make font small enough
  theme(
    axis.title = element_text(size = 20)
  ) +
  # quadratic tiles
  coord_fixed()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())
legend
