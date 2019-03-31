eigen_wrapper <- function(igraph_object, weighted = NULL){

  require(igraph)
  
  if(is.null(weighted)){
    
    return(unlist(eigen_centrality(igraph_object)[1]))
    
  }
  else
  {
    return(unlist(eigen_centrality(igraph_object, weights = weighted)[1]))
  }
}

# function that takes raw data and returns ready igraph object

return_igraph <- function(raw_data, names) {
  
  tube_graph <- graph.data.frame(raw_data, directed = FALSE)
  
  matched_names <- match(V(tube_graph)$name, names$id)
  names_list <- names$station_name[matched_names]
  tube_graph <- set.vertex.attribute(tube_graph, "name", value = as.character(names_list))
  
  E(tube_graph)$weight <- 1/network_data$distance
  
  return(tube_graph)
}



# function that takes a network and returns the correlations between various network stats across stations

corr_of_stats <- function(input_graph) {
  
  
  node_stats <- data.frame(names(V(input_graph)))
  names(node_stats)[1] <- "station_name"
  # network stats
  n_stations <- length(V(input_graph))
  n_lines <- length(E(input_graph))
  independent_clusters <- clusters(input_graph)$no
  
  node_stats$closeness <- closeness(input_graph)
  node_stats$topo_closeness <- closeness(input_graph, weights = NA)
  node_stats$betweenness <- betweenness(input_graph)
  node_stats$topo_betweennes <- betweenness(input_graph, weights = NA) 
  node_stats$eigen_centrality <- eigen_wrapper(input_graph)
  node_stats$topo_eigen_centrality <- eigen_wrapper(input_graph, weighted = NA)
  node_stats$degree <- degree(input_graph)

  corr_data <- node_stats
  print(summary(corr_data))
  corr_data$station_name <- NULL
  corr_data_calc <- cor(corr_data)
  corr_plot <- corrplot(corr_data_calc, method = "number")
  
  
  return(corr_plot)
}


# this is a just a test run
node_stats_calc <- function(input_graph, fun = betweenness) {
  
  foo = match.fun(fun)
  
  node_stats <- data.frame(names(V(input_graph)))
  names(node_stats)[1] <- "station_name"
  # network stats
  
  node_stats$stat <- foo(input_graph)
  
  return(node_stats)
}

#Recursive function for calculating node removal effects
  
node_chopper <- function(igraph_object, node_function, network_function, depth, unconn) {
  
  # check that it's an igraph object
  if(class(igraph_object) != "igraph") {
    return("i_graph_object must be of class igraph")
  } else {
    
    # if igraph object is null (all nodes have been deleted) return null
    if(is.null(igraph_object)) {
      return(NULL)
    } else {
      
      # if depth == 0 return null if enough nodes have been deleted to complete the analysis
      if(depth == 0){
        
      } else {
        
        # match functions
        
        net_fun <- match.fun(network_function)
        node_fun <- match.fun(node_function)
        
        # calculate pre-deletion network measurement statistic 
        network_stat_1 = net_fun(igraph_object, unconnected = unconn)
        
        # call node_function on igraph object
        
        node_stats = node_stats_calc(igraph_object, fun = node_fun)
        
        # station_name = max station stat
        
        target <- node_stats[which.max(node_stats$stat),]
        target <- as.character(target[[1]])
        
        # delete station
        
        igraph_object = delete.vertices(igraph_object, c(target))
        
        #calculate post_deletion network statistic
        network_stat_2 = net_fun(igraph_object, unconnected = unconn)
        
        # calc change in network statistics due to deletion
        # a positive change means that trips have gotten longer
        network_change = network_stat_2 - network_stat_1
        
        # join deleted station name and effect
        value = data.frame(target, network_change, components(igraph_object)$no)
        
        # return value
        return (rbind(value, node_chopper(igraph_object, node_function, network_function, (depth - 1), unconn = unconn)))
        
      } 
    }  
  }
}


# function that calls A_Function with different values for the two referenced functions. 

meta_function <- function(igraph_object, node_stat_list, network_state_list) {
  
  
}

