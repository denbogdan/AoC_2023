setwd("~/Projects/AoC_2023/day_8/")
sky <- readLines("dummy.txt")

#make into data matrix 
split_list <- sapply(sky, function(x) strsplit(x, split = "")) 
sky_matrix <- split_list[[1]]
for(i in 2: length(sky)) {
  sky_matrix <- rbind(sky_matrix, split_list[[i]])
}

#expansion problem
#find all the empty rows
row_tally <- apply(sky_matrix, MARGIN = 1, table)
spaces <- lapply(row_tally, function(x) {x["."]}) |> unlist()
rows_to_expand <- which(spaces == nrow(sky_matrix)) |> as.numeric() 
rows_to_expand <- rows_to_expand+seq(0:(length(rows_to_expand)-1))
#find all the empty columns
col_tally <- apply(sky_matrix, MARGIN = 2, table)
spaces <- lapply(col_tally, function(x) {x["."]}) |> unlist()
cols_to_expand <- which(spaces == ncol(sky_matrix)) |> as.numeric() 
cols_to_expand <- cols_to_expand+seq(0:(length(cols_to_expand)-1))

#build augmented sky map - first add rows
#initialise all spaces
new_sky <- matrix(".", nrow=(nrow(sky_matrix)+length(rows_to_expand)), ncol=ncol(sky_matrix))
new_sky[-rows_to_expand,] <- sky_matrix
#then add columns
#initialise all spaces
new_sky_final <- matrix(".", nrow=nrow(new_sky), ncol=ncol(new_sky)+length(cols_to_expand))
new_sky_final[,-cols_to_expand] <- new_sky

#find the locations of all galaxies
#make into df with i,j as positions
g_df <- c(0,0)
for(i in 1:nrow(new_sky_final)) {
  for(j in 1:ncol(new_sky_final)) {
    if(new_sky_final[i,j] == "#")
      g_df <- rbind(g_df, c(i,j))
  }
}

#tidy df
g_df <- g_df %>% as.data.frame() %>% dplyr::filter(V2 != 0)

#generate all unique pairs
unique_pairs <- combn(1:nrow(g_df), 2)

#for each unique pair, the shortest distance is the sum of vertical and horizontal differences
all_distances <- c()
for(i in 1:ncol(unique_pairs)) {
  pair <- unique_pairs[,i]
  #select rows from coordinates data frame
  A <- g_df[pair[1],]
  B <- g_df[pair[2],]
  #distance - Pythagoras who???
  d <- abs(A-B) |> sum()
  #save distance
  all_distances <- c(all_distances, d)
}

##Answer 1 -------------------------------------------------------------------------
all_distances |> sum()


##Question 2 -----------------------------------------------------------------------
#the number of pairs will stay the same - the indeces will change
#need to map in between which rows to insert one million rows incrementally
#find all the empty rows
row_tally <- apply(new_sky_final, MARGIN = 1, table)
spaces <- lapply(row_tally, function(x) {x["."]}) |> unlist()
rows_to_expand <- which(spaces == ncol(new_sky_final)) |> as.numeric()
#find all the empty columns
col_tally <- apply(new_sky_final, MARGIN = 2, table)
spaces <- lapply(col_tally, function(x) {x["."]}) |> unlist()
cols_to_expand <- which(spaces == nrow(new_sky_final)) |> as.numeric() 

#for each of these rows and cols to be augmented, modify df describing the coordinates with the required amount of millions
g_df_changed <- g_df
#rows 
millions_to_add <- 0
for(i in 1:length(rows_to_expand)) {
  #row that introduces and increment
  r <- rows_to_expand[i]
  print(r)
  #find all the subsequent rows affected
  affected_rows <- seq(r,nrow(new_sky_final))
  #find all these rows in df and augment
  g_df_changed$V1[which(g_df$V1 %in% affected_rows)] <- g_df_changed$V1[which(g_df$V1 %in% affected_rows)] + millions_to_add
  #increase numbers of millions
  millions_to_add <- millions_to_add + 1000000-1
  print(millions_to_add)
}

#columns 
millions_to_add <- 0
for(i in 1:length(cols_to_expand)) {
  #row that introduces and increment
  c <- cols_to_expand[i]
  print(c)
  #find all the subsequent rows affected
  affected_cols <- seq(c,ncol(new_sky_final))
  #find all these rows in df and augment
  g_df_changed$V1[which(g_df$V1 %in% affected_cols)] <- g_df_changed$V1[which(g_df$V1 %in% affected_cols)] + millions_to_add
  #increase numbers of millions
  millions_to_add <- millions_to_add + 1000000-1
  print(millions_to_add)
}

all_distances <- c()
for(i in 1:ncol(unique_pairs)) {
  pair <- unique_pairs[,i]
  #select rows from coordinates data frame
  A <- g_df[pair[1],]
  B <- g_df[pair[2],]
  #distance - Pythagoras who???
  d <- abs(A-B) |> sum()
  #save distance
  all_distances <- c(all_distances, d)
}

##Answer 2 -------------------------------------------------------------------------
all_distances |> sum()