setwd("~/portfolio/AoC_2023/day_3/")
map <- readLines("day_3_input.txt")

#unique symbols in this map 
split_list <- sapply(map, function(x) strsplit(x, split = ""))
symbols <- lapply(split_list, unique)
names(symbols) <- 1:length(symbols)
symbols_dict <- unique(unlist(symbols))
symbols_dict <- symbols_dict[12:length(symbols_dict)]

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

#make map
map_df <- c(0,0,0,0)
for(i in 1:length(map)) {
  #split row
  row <- strsplit(map[i], split="\\.") |> unlist()

  #transverse and further split
  row_split <- c()
  for(j in 1:length(row)) {
    if(row[j] != "") {
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
  dplyr::filter(V2 != 0)

#look for neighbours to validate number
keep <- c()
for(i in 1:5) {
  n <- map_df[i, 1] 
  l <- map_df[i,2] 
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

