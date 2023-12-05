setwd("~/Projects/AoC_2023/day_5/")
data <- readLines("day_5_input.txt")

#make into shape
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
      maps_list <- c(maps_list, as.data.frame(map_build))
      map_build <- c(0,0,0)
    } else {
      maps_names <- c(maps_names, data[i])
    }
  }
}
