start_node <- "66 66"
max_length <- 65
COUNTER <- 0
for(i in 1:nrow(map_m)) {
  for(j in 1:ncol(map_m)) {
    #eligible if it is not a stone
    if(map_m[i,j] != "#") {
      #name this node 
      end_node <- paste(i,j)
      
      #BFS
      paths <- list()
      queue <- list(c(start_node))
      
      while (length(queue) > 0) {
        current_path <- queue[[1]]
        queue <- queue[-1]
        
        current <- tail(current_path, 1)
        
        neighbors <- graph[[current]]
        for (neighbor in neighbors) {
          if (!(neighbor %in% current_path)) {
            new_path <- c(current_path, neighbor)
            if (neighbor == end_node && length(new_path) <= max_length) {
              paths <- c(paths, list(new_path))
            } else if (length(new_path) < max_length) {
              queue <- c(queue, list(new_path))
            }
          }
        }
      }
      lp <- lapply(paths, length) |> as.numeric()
      if(length(lp) > 0) {
        print(lp)
        if(lp[1] %% 2 == 1)
          COUNTER <- COUNTER + 1
      }
    }
  }
}
