# library packages

require(sf)
require(igraph)
require(corrplot)

# source helper functions

source('~/UCL_CASA_1819/USim/Coursework/USim_Part_1/Helpers_3.R', echo=FALSE)

# load data

network_data <- read.csv("data/london_network.csv", header = TRUE)
station_names <- read.csv("data/london_stations.csv", header = TRUE)

# create igraph object

network <- return_igraph(network_data, station_names)

# plot tube network

plot(network,vertex.size=3,vertex.label.cex=.5,vertex.color="white")

# create correlation matrix

correlation_chart <- corr_of_stats(network)

# run node_chopper function to calculate effect. 



effect_data <- node_chopper(network, node_function, network_function, depth = 5)

# build table of effects across stats and rank

