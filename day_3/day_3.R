setwd("~/Projects/AoC_2023/day_3/")
map <- readLines("day_3_input.txt")

#unique symbols in this map 
split_list <- sapply(map, function(x) strsplit(x, split = ""))
symbols <- lapply(split_list, unique)
names(symbols) <- 1:length(symbols)
symbols_dict <- unique(unlist(symbols)) |> sort()

#parse input as a data frame
#columns are number/symbol, row, start pos, end pos
map_df <- c(0,0,0,0)
for(i in 1:length(map)) {
  #split row
  row <- strsplit(map[i], split="\\.") |> unlist()
  
  #for each element of this row, increase j and pointer accordingly
  for(j in 1:length(row)) {
    #skip all the ones without numbers
    nb <- c()
    pos <-c()
    nb_spare <- c()
    pos_spare <-c()
    if(row[j] != "") {
      #split into vector of characters
      split_number <- strsplit(row[j], split="") |> unlist()
      symbol_found <- FALSE
      for(z in 1:length(split_number)) {
        if(split_number[z] %in% symbols_dict[1:11]) {
          map_df <- rbind(map_df, c(split_number[z] |> unlist(), i, j+z-1, j+z-1))
          symbol_found <- TRUE
        } else {
          if(!symbol_found) {
            nb <- c(nb, split_number[z])
            pos <- c(pos, j+z-1) 
          } else {
            nb_spare <- c(nb_spare, split_number[z])
            pos_spare <- c(pos_spare, j+z-1)
          }
        }
      }
      #make into number
      nb_real <- paste0(nb, collapse="")
      #add to df
      map_df <- rbind(map_df, c(nb_real, i, pos[1], pos[length(pos)]))
      #and the spare
      if(length(nb_spare) >0) {
        nb_real <- paste0(nb_spare, collapse="")
        map_df <- rbind(map_df, c(nb_real, i, pos_spare[1], pos_spare[length(pos_spare)]))
      }
        
    }
  }
}

#tidy map
map_df <- map_df %>% as.data.frame() %>% dplyr::filter(V1 != "") %>%
  dplyr::filter(V2 != 0)

#for each number in map, look for neighbors and add if any
keep <- c()
for(i in 1:15) {
  n <- map_df[i, 1] |> as.numeric()
  print(n)
  l <- map_df[i,2] |> as.numeric()
  start <- ifelse(map_df[i,3] != "1", map_df[i,3], 2) |> as.numeric()
  end <- ifelse(map_df[i,4] != "140", map_df[i,4], 139) |> as.numeric()
  #find all neighbours of this number
  neighbours <- c(0,0,0,0)
  #line above and below
  found <- map_df %>% dplyr::filter(V1 %in% symbols_dict[1:11]) %>%
    dplyr::filter(V3 >= (start-1) & V4 <= (end+1)) 
  if(l == 1) found <- found %>% dplyr::filter(V2=="2")
  if(l == length(map)) found <- found %>% dplyr::filter(V2==as.character(length(map)-1))
  if(!(l %in% c(1, length(map)))) found <- found %>% dplyr::filter(V2 %in% as.character(c(i-1, i+1)))
  if(nrow(found)>0) neighbours <- rbind(neighbours, found)
  #sides
  found <- map_df %>% dplyr::filter(V1 %in% symbols_dict[1:11]) %>%
      dplyr::filter((V2==as.character(n) & V3==as.character(start-1)) | (V2==as.character(n) & V3==as.character(end+1)))
  if(nrow(found)>0) neighbours <- rbind(neighbours, found)
  print(neighbours)
  if(!is.null(dim(neighbours))) keep <- c(keep, n)
}


