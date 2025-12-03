setwd("~/portfolio/AoC_2023/day_21/")
map <- readLines("day_21_input.txt")

#wrangle data into matrix 
map_m <- strsplit(map[1], split="") |> unlist()
for(i in 2:length(map)) {
  row <- strsplit(map[i], split="") |> unlist()
  print(row)
  map_m <- rbind(map_m, row)
}

#for each position, add visited then find out where to go next
visit <- function(i, j) {
  #declare all 4 directions
  move <- list(c(i-1, j), c(i+1, j), c(i, j-1), c(i, j+1))
  #if available to move, return the location of these 
  next_moves <- list()
  for(d in move) {
    if((d[1] %in% 1:nrow(map_m)) & (d[2] %in% 1:ncol(map_m))) {
      if(map_m[d[1], d[2]] != "#") {
        next_moves <- c(next_moves, list(c(d[1], d[2])))
      }
    }
  }
  return(next_moves)
}

a <- Sys.time()
start <- which(map_m == "S", arr.ind = TRUE)
places_to_visit <- visit(start[1], start[2])
for(step in 1:63) {
  next_places_to_visit <- list()
  for(p in places_to_visit) {
    next_places_to_visit <- c(next_places_to_visit, visit(p[1], p[2]))
  }
  places_to_visit <- next_places_to_visit
}
b <- Sys.time()

b-a
next_places_to_visit |> unique() |> length()


a <- Sys.time()
visit(66,66)
b <- Sys.time()

b-a


#Make into a graph - transverse matrix
graph <- list()
for(i in 1:nrow(map_m)) {
  for(j in 1:ncol(map_m)) {
    #map neighbours if it is not a stone
    if(map_m[i,j] != "#") {
      #name this node 
      current_node <- paste(i,j)
      #connected nodes
      connected_nodes <- c()
      #declare all 4 directions
      move <- list(c(i-1, j), c(i+1, j), c(i, j-1), c(i, j+1))
      #if available to move, return the location of these 
      for(d in move) {
        if((d[1] %in% 1:nrow(map_m)) & (d[2] %in% 1:ncol(map_m))) {
          if(map_m[d[1], d[2]] != "#") {
            connected_nodes <- c(connected_nodes, paste(c(d[1], d[2]), collapse = " "))
          }
        }
      }
      #add to my list
      graph[[current_node]] <- connected_nodes
    }
  }
}

#viz
pheatmap(ifelse(map_m == ".", 0, 1), cluster_rows = FALSE, cluster_cols = FALSE,
         show_rownames = FALSE )

#depth first search from each . to s
which(map_m == "S", arr.ind = TRUE)
start_node <- "66 66"
max_length <- 8
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
            
            if (neighbor == end_node && length(new_path) < max_length) {
              paths <- c(paths, list(new_path))
            } else if (length(new_path) < max_length) {
              queue <- c(queue, list(new_path))
            }
          }
        }
      }
      lp <- lapply(paths, length) |> as.numeric()
      if(length(lp) > 0) print(lp)
      if(7 %in% lp)
        COUNTER <- COUNTER + 1
    }
  }
}






