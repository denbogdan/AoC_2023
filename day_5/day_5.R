setwd("~/Projects/AoC_2023/day_5/")
data <- readLines("day_5_input.txt")
#delineate last map by adding an extra empty line
data <- c(data, "")

##Question 1 -----------------------------------------------------------------------
#parse data - maps
maps_list <- list()
maps_names <- c()
map_build <- c(0,0,0)
for(i in 3:length(data)) {
  row <- strsplit(data[i], split=" ") |> unlist() |> as.numeric()
  if(!is.na(row[1])) {
    map_build <- rbind(map_build, row)
  }
  else {
    if(length(row) == 0) {
      map_build <- as.data.frame(map_build)
      map_build <- map_build[2:nrow(map_build),]
      colnames(map_build) <- c("destination", "source", "range_len")
      maps_list <- c(maps_list, list(map_build))
      map_build <- c(0,0,0)
    } else {
      maps_names <- c(maps_names, data[i])
    }
  }
}

#parse data - seeds
seeds <- strsplit(data[1], split=" ") |> unlist()
seeds <- seeds[2:length(seeds)] |> as.numeric()

#write function that takes a number and a map and produces its new equivalent
value_to_map <- function(n, map) {
  #find start of source
  map <- map[order(map$source),]
  map$source_end <- map$source + map$range_len
  source_end<- map$source_end[which(map$source_end>=n)]
  if(length(source_end) == 0)
    mapped <- n
  else {
    source_end <- source_end[1]
    source_start <- map[which(map$source_end == source_end), "source"]
    if(n < source_start) {
      mapped <- n
    } else {
      inc <- n-source_start
      mapped <- map[which(map$source == source_start), "destination"] + inc
    }
  }
  return(mapped)
}

#solve for all seeds
start_time <- Sys.time()
#map all seeds to the end 
locations <- c()
for(i in 1:length(seeds)) {
  n <- seeds[i]
  #go through all the maps 
  for(j in 1:length(maps_list)) {
    map <- maps_list[[j]] 
    n <- value_to_map(n, map)
  }
  #append
  locations <- c(locations, n)
}
end_time <- Sys.time()

end_time - start_time

##Answer 1 -------------------------------------------------------------------------
locations |> as.numeric() |> min()


##Question 2 -----------------------------------------------------------------------
#check number of new seeds
sum(seeds[seq(2, length(seeds), 2)])

#work backwards - find lowest locations in the last map


