setwd("~/Projects/AoC_2023/day_3/")
map <- readLines("day_3_input.txt")

#unique symbols in this map 
split_list <- sapply(map, function(x) strsplit(x, split = ""))
symbols <- lapply(split_list, unique)
names(symbols) <- 1:length(symbols)
symbols_dict <- unique(unlist(symbols)) |> sort()
symbols_dict <- symbols_dict[c(1, 3:11)]

symbols_dict <- symbols_dict[2:5]

##Question 1 -----------------------------------------------------------------------
#write function to parse numbers accompanied by any symbols
parse_string <- function(string, symbols_dict) {
  parser <- c()
  #split it into individual characters
  sn <- strsplit(string, split="") |> unlist()
  i=1
  n <- c()
  while(i <= length(sn)) {
    if(sn[i] %in% symbols_dict) {
      parser <- c(parser, paste0(n, collapse=""))
      n <- c()
      parser <- c(parser, sn[i])
    }
    else {
      n <- c(n, sn[i])
    }
    i <- i+1
  }
  parser <- c(parser, paste0(n, collapse=""))
  parser <- parser[which(parser != "")]
  return(parser)
}

#make map of all the numbers and symbols
map_df <- c(0,0,0,0)
for(i in 1:length(map)) {
  #split row
  row <- strsplit(map[i], split="(?=[.])", perl = TRUE) |> unlist()

  #transverse and further split
  row_split <- c()
  for(j in 1:length(row)) {
    if(row[j] != ".") {
      insert <- parse_string(row[j], symbols_dict)
      row_split <- c(row_split, insert)
    } else {
      row_split <- c(row_split, row[j])
    }
  }  
  
  #make df where columns are number/symbol, row, start pos, end pos
  inc <- 0
  for(j in 1:length(row_split)) {
    if(row_split[j] != "") {
      if(row_split[j] %in% symbols_dict) {
        map_df <- rbind(map_df, c(row_split[j], i, j+inc, j+inc))
      } else {
        map_df <- rbind(map_df, c(row_split[j], i, j+inc, j+inc+nchar(row_split[j])-1))
        inc <- inc+nchar(row_split[j])-1
      }
    }
  }
}

#tidy map
map_df <- map_df %>% as.data.frame() %>% dplyr::filter(V1 != "") %>%
  dplyr::filter(V2 != 0) %>% dplyr::filter(V1 != ".")

#separate into symbols and numbers
s_df <- map_df %>% dplyr::filter(V1 %in% symbols_dict)
nb_df <- map_df %>% dplyr::filter(!(V1 %in% symbols_dict))

#look for neighbours to validate number
keep <- c()
for(i in 1:nrow(nb_df)) {
  n <- nb_df[i, 1] |> as.numeric()
  l <- nb_df[i,2] |> as.numeric()
  start <-  nb_df[i,3] |> as.numeric()
  end <- nb_df[i,4] |> as.numeric()
  
  #find all locations neighbouring this number
  #neighbours list is in the format line, position
  neighbours <- 0
  #left
  if(start>1) {
    match <- s_df %>% dplyr::filter(V2 == l & V3 == start-1) %>% nrow()
    if(match > 0) neighbours <- neighbours+1
  }
  #right
  if(end<nchar(map[1])) {
    match <- s_df %>% dplyr::filter(V2 == l & V3 == end+1) %>% nrow()
    if(match > 0) neighbours <- neighbours+1
  }
  #top and bottom
  for(j in 1:nchar(n)) {
    #top
    if(l>1) {
      match <- s_df %>% dplyr::filter(V2 == l-1 & V3 == start+j-1) %>% nrow()
      if(match > 0) neighbours <- neighbours+1
    }
    #bottom
    if(l<length(map)) {
      match <- s_df %>% dplyr::filter(V2 == l+1 & V3 == start+j-1) %>% nrow()
      if(match > 0) neighbours <- neighbours+1
    }
  }
  #diagonals - top
  if(start>1 & l>1) {
    match <- s_df %>% dplyr::filter(V2 == l-1 & V3 == start-1) %>% nrow()
    if(match > 0) neighbours <- neighbours+1
  }
  if(end<nchar(map[1]) & l>1) {
    match <- s_df %>% dplyr::filter(V2 == l-1 & V3 == end+1) %>% nrow()
    if(match > 0) neighbours <- neighbours+1
  }
  #diagonals - bottom
  if(start>1 & l<length(map)) {
    match <- s_df %>% dplyr::filter(V2 == l+1 & V3 == start-1) %>% nrow()
    if(match > 0) neighbours <- neighbours+1
  }
  if(end<nchar(map[1]) & l<length(map)) {
    match <- s_df %>% dplyr::filter(V2 == l+1 & V3 == end+1) %>% nrow()
    if(match > 0) neighbours <- neighbours+1
  }
  
  if(neighbours > 0) keep <- c(keep, n)
}

##Answer 1 -------------------------------------------------------------------------
sum(keep)

##Question 2 -----------------------------------------------------------------------
#find stars only
stars <- s_df %>% dplyr::filter(V1=="*")

keep_stars <- c()
for(i in 1:nrow(stars)) {
  l <- stars[i,2] |> as.numeric()
  pos <-  stars[i,3] |> as.numeric()
  
  #find all locations neighbouring this star
  #neighbours list is in the format line, position
  neighbours <- c()
  unique_numbers <- c(0,0,0,0)
  #left
  if(pos>1) {
    match <- nb_df %>% dplyr::filter(V2 == l & V4 == pos-1 ) %>% dplyr::distinct()
    if(nrow(match) > 0) {
      neighbours <- c(neighbours, as.numeric(match$V1))
      unique_numbers <- rbind(unique_numbers, match)
    }
  }
  #right
  if(pos<nchar(map[1])) {
    match <- nb_df %>% dplyr::filter(V2 == l & V3 == pos+1) %>% dplyr::distinct()
    if(nrow(match) > 0) {
      neighbours <- c(neighbours, as.numeric(match$V1))
      unique_numbers <- rbind(unique_numbers, match)
    }
  }

  #top
  if(l>1) {
    match <- nb_df %>% dplyr::filter(V2 == l-1 & (V3 == pos | V4==pos)) %>% dplyr::distinct()
    if(nrow(match) > 0) {
      neighbours <- c(neighbours, as.numeric(match$V1))
      unique_numbers <- rbind(unique_numbers, match)
    }
  }
  #bottom
  if(l<length(map)) {
    match <- nb_df %>% dplyr::filter(V2 == l+1 & (V3 == pos | V4==pos)) %>% dplyr::distinct()
    if(nrow(match) > 0) {
      neighbours <- c(neighbours, as.numeric(match$V1))
      unique_numbers <- rbind(unique_numbers, match)
    }
  }

  #diagonals - top
  if(pos>1 & l>1) {
    match <- nb_df %>% dplyr::filter(V2 == l-1 & (V4 == pos-1 | V3 == pos-1)) %>% dplyr::distinct()
    if(nrow(match) > 0) {
      neighbours <- c(neighbours, as.numeric(match$V1))
      unique_numbers <- rbind(unique_numbers, match)
    }
  }
  if(pos<nchar(map[1]) & l>1) {
    match <- nb_df %>% dplyr::filter(V2 == l-1 & (V3 == pos+1 | V4 == pos+1)) %>% dplyr::distinct()
    if(nrow(match) > 0) {
      neighbours <- c(neighbours, as.numeric(match$V1))
      unique_numbers <- rbind(unique_numbers, match)
    }
  }

  #diagonals - bottom
  if(pos>1 & l<length(map)) {
    match <- nb_df %>% dplyr::filter(V2 == l+1 & (V4 == pos-1 | V3 == pos-1)) %>% dplyr::distinct()
    if(nrow(match) > 0) {
      neighbours <- c(neighbours, as.numeric(match$V1))
      unique_numbers <- rbind(unique_numbers, match)
    }
  }
  if(pos<nchar(map[1]) & l<length(map)) {
    match <- nb_df %>% dplyr::filter(V2 == l+1 & (V3 == pos+1 | V4 == pos+1)) %>% dplyr::distinct()
    if(nrow(match) > 0) {
      neighbours <- c(neighbours, as.numeric(match$V1))
      unique_numbers <- rbind(unique_numbers, match)
    }
  }
  
  p <- 0
  if(nrow(unique_numbers) == nrow(unique_numbers %>% dplyr::distinct())) {
    if(length(neighbours)==2) {
      p <- prod(neighbours)
      }
  } else {
    if(length(neighbours) == 3 & length(unique(neighbours)) == 1) { 
      n <- unique(neighbours) |> as.numeric()
      p <-  n*n }
    if(length(neighbours)>2 & length(unique(neighbours)) !=1) {
      p <- prod(unique(neighbours))
    }
  }

  keep_stars <- c(keep_stars, p)
}

##Answer 2 -------------------------------------------------------------------------
sum(keep_stars)
