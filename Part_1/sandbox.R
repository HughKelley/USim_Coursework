a <- c(1,2,3,4,5,6,7,8)
b <- c("a","b","c","d","e","f","g","h")
c <- c("red", "green", "blue", "brown", "purple", "yellow", "orange", "grey")

df <- data.frame(a,b,c)

target <- df[which.max(df$a),]
  
print(target)

print(target["c"])

actual_target <- as.character(target[["c"]])

print(actual_target)

###########################################################################

test_string <- c("grey")



ratio <- node_stats$betweenness/max(node_stats$betweenness)
a <- 1-((ratio * 2/3) + 1/3)
palette=hsv(h = a, s = 1, v = 1)
radius_vertex=3
width_lines=6

#we can plot the graph using the measure of betweenness
plot(tube_graph,vertex.size=ratio*radius_vertex+.01,vertex.color=palette,vertex.label.cex=.1)


################################################################################################
################################################################################################

# 
# addPercent <- function(x, mult = 100, FUN = round, ...){
#   percent <- FUN(x * mult, ...)
#   paste(percent, "%", sep = ")
# }
# 
# addPercent(new.numbers, FUN = signif, digits = 3)

# this works

f_add<- function(x,y){ x + y }
f_subtract<- function(x,y){ x - y }
f_multi<- function(x,y){ x * y }

test_function <- function(x,y,FUN) {
  
  fun <- match.fun(FUN)
  
  output <- fun(x,y)
  
  return(output)
  
}

a <- test_function(3,4,f_multi)
print(a)

################################################################################################

# now with graph functions

graph_stat <- function(graph, graph_function)
{
  a_function = match.fun(graph_function)
  output = a_function(graph)
  
  return(output)
}


test_ouput <- graph_stat(network, "degree")





