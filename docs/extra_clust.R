####Read in packages for this code####
install.packages("fields")
install.packages("dplyr")
install.packages("ggplot2")
library(fields)
library(ggplot2)
library(dplyr)

####Read in country files####
#Firstly Australia
aus <- read.csv("post_codes_github.csv")
#Secondly NZ
nz <- read.csv("nz_postal_codes.csv")

#How many postcodes do we have?
length(unique(aus$postcode))
length(unique(nz$postcode))

####Geoclustering setup####
#Here we set the country
country <- aus
country <- as.data.frame(aus)
##Firstly perform a 'sense-check' that these points are in Aus-NZ
country$longitude <- as.numeric(country$longitude)
country$latitude <- as.numeric(country$latitude)
plot(country$longitude,country$latitude, xlab = "Longitude", ylab= "Latitude", main = "Plot of latitude and longitude of postcodes in set")

#(For Australia only) there are some islands that flatten our graph. We can remove extreme lat/long to help with this
country <- country[(country$longitude < 155 & country$longitude > 110),]
country <- country [country$latitude < 0 ,]

#We could just do this by postcode
#country <- as.data.frame(country[!duplicated(country$postcode),])

#Here is some code that will only get rid of duplicated lat-long combinations.
#I define something called 'll_id' which is the lat-long ID. The combination of lat-long together as a string-ID
country$ll_id <- paste(country$longitude, country$latitude)
country <- country %>% select(longitude, latitude, ll_id, everything())
country <- as.data.frame(country[!duplicated(country$ll_id),])

####(For Australia only) We could even divide this by state
#Possible states = ACT NSW NT QLD SA TAS VIC WA

# state = "QLD"
# country <- country[country$state == state,]

####Distance Matrix####

#We are using the rdist function from fields package
library(fields)
dist <- rdist.earth(country,miles = F,R=6378)

##Useful link http://stackoverflow.com/questions/21095643/approaches-for-spatial-geodesic-latitude-longitude-clustering-in-r-with-geodesic
##The earth radius we get from http://www.ga.gov.au/scientific-topics/positioning-navigation/geodesy/geodetic-datums/gda

#There are a number of other packages you can use for geospatial distance including DBSCAN, Eath.dist from package fossil, amap, geosphere

##Hierarchical clusters
fit <- hclust(as.dist(dist), method = "ward.D")
#Here we plot the dendrogram but it doesn't look very good
#plot(fit)

#Important to set the number of clusters
num_clusters = 35
country$clusters <- cutree(fit,k = num_clusters)

#Note put in this line to remove the legend + theme(legend.position="none")

ggplot() +
  geom_point(data=country, aes(x=longitude, y=latitude, color=factor(clusters)), size=4)+
  scale_color_discrete("Cluster") 
  coord_fixed()
