find_paths <- function(graph, start, end, max_length) {
  paths <- list()
  visited <- character()
  
  dfs <- function(current, path) {
    visited <<- c(visited, current)
    
    neighbors <- graph[[current]]
    for (neighbor in neighbors) {
      if (!(neighbor %in% visited)) {
        new_path <- c(path, neighbor)
        
        if (neighbor == end && length(new_path) < max_length) {
          paths <<- c(paths, list(new_path))
        } else if (length(new_path) < max_length) {
          dfs(neighbor, new_path)
        }
      }
    }
    
    visited <<- setdiff(visited, current)
  }
  
  dfs(start, c(start))
  return(paths)
}

# Example graph represented as an adjacency list
graph <- list(
  'A' = c('B', 'C'),
  'B' = c('A', 'D', 'E'),
  'C' = c('A', 'F', 'G'),
  'D' = c('B'),
  'E' = c('B', 'H'),
  'F' = c('C'),
  'G' = c('C'),
  'H' = c('E')
)

start_node <- 'A'
end_node <- 'G'
max_path_length <- 4

result_paths <- find_paths(graph, start_node, end_node, max_path_length)
print(paste("Paths from", start_node, "to", end_node, "shorter than", max_path_length, ":", result_paths))
