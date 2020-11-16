extract_dendrogram_data <- function(allocations,max_level = 10){
  
  node_data <- data.frame(id = numeric(), parent = numeric(),size = numeric(),level_id = numeric())
  
  for(i in 1:max_level){
    # Add nodes.
    cluster_sizes <- table(allocations[[i]])
    n_clusts <- length(cluster_sizes)
    node_data_current <- data.frame(level = i, id = nrow(node_data) + 1:n_clusts,
                                    parent = NA,size = NA)
    
    
    # Look for parents in the last round
    if(i != 1){
      prev_level_nodes <- sort(node_data[node_data$level == i-1,]$id)
      table_counts <- table(allocations[[i]],allocations[[i-1]])
      parents <- apply(table_counts!=0, 1, which)
      allocations[[i]] <- rank(parents,ties.method = "first")[allocations[[i]]]
      table_counts <- table(allocations[[i]],allocations[[i-1]])
      cluster_sizes <- table(allocations[[i]])
      parents <- apply(table_counts!=0, 1, which)
      node_data_current$parent <- prev_level_nodes[parents]
      node_data_current$size <- c(cluster_sizes)
      node_data_current <- node_data_current %>% arrange(parent)
      node_data_current$level_id <- 1:n_clusts
    }else{
      node_data_current$level_id <- node_data_current$level
      node_data_current$size <- c(table(allocations[[i]]))
    }
    node_data <- rbind(node_data, node_data_current)
  }
  
  max_level <- max(node_data$level)
  node_data$x_val2 <- NA
  
  node_data$x_val2[node_data$level == max_level] <- node_data$level_id[node_data$level == max_level]
  for(level in (max_level-1):1){
    for(i in node_data$id[node_data$level == level]){
      node_data$x_val2[node_data$id==i] <- mean(node_data$x_val2[node_data$parent == i],na.rm = TRUE)
    }
  }
  
  segment_data <- data.frame(id = numeric(),x = numeric(), y = numeric(), xend = numeric(), yend = numeric())
  
  for(i in 1:nrow(node_data)){
    if(!is.na(node_data$parent[i])){
      segment_data_temp <- data.frame(
        id = i,
        x = node_data$x_val2[i],
        y = node_data$level[i],
        xend = node_data$x_val2[node_data$id==node_data$parent[i]],
        yend = node_data$level[node_data$id==node_data$parent[i]]
      )
      segment_data <- rbind(
        segment_data,
        segment_data_temp
      )
    }
  }
  
  return(list(
    node_data = node_data,
    segment_data = segment_data
  ))
}
