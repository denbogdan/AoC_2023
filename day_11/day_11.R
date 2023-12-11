setwd("~/Projects/AoC_2023/day_11/")
sky <- readLines("day_11_input.txt")

#make base input into data matrix 
split_list <- sapply(sky, function(x) strsplit(x, split = "")) 
sky_matrix <- split_list[[1]]
for(i in 2: length(sky)) {
  sky_matrix <- rbind(sky_matrix, split_list[[i]])
}

#find all the empty rows - for base input
row_tally <- apply(sky_matrix, MARGIN = 1, table)
spaces <- lapply(row_tally, function(x) {x["."]}) |> unlist()
rows_to_expand <- which(spaces == ncol(sky_matrix)) |> as.numeric()
#find all the empty columns - for base input
col_tally <- apply(sky_matrix, MARGIN = 2, table)
spaces <- lapply(col_tally, function(x) {x["."]}) |> unlist()
cols_to_expand <- which(spaces == nrow(sky_matrix)) |> as.numeric() 

#document all the galaxies in a data frame of coordinates i,j
g_df <- c(0,0)
for(i in 1:nrow(sky_matrix)) {
  for(j in 1:ncol(sky_matrix)) {
    if(sky_matrix[i,j] == "#")
      g_df <- rbind(g_df, c(i,j))
  }
}

#tidy df
g_df <- g_df %>% as.data.frame() %>% dplyr::filter(V2 != 0)

#generate all unique pairs of galaxies
unique_pairs <- combn(1:nrow(g_df), 2)


##Question 1 -----------------------------------------------------------------------
#we are going to modify these coordinates by adding the appropriate number of rows and columns 
#the inset is the offset by which each position changes with every new blank line discovered
g_df_changed <- g_df
#each row or column that 'doubles' effectively introduces 1 more of itself
inserts <- 1
#rows 
for(i in 1:length(rows_to_expand)) {
  inc <- (inserts*(i-1))
  #row that introduces an increment
  r <- rows_to_expand[i] + inc
  #find all the subsequent rows affected
  affected_rows <- seq(r+1,(nrow(sky_matrix)+inc))
  #find all these rows in df and augment
  g_df_changed$V1[which(g_df_changed$V1 %in% affected_rows)] <- g_df_changed$V1[which(g_df_changed$V1 %in% affected_rows)] + inserts
}

#columns 
for(i in 1:length(cols_to_expand)) {
  inc <- (inserts*(i-1))
  #row that introduces an increment
  c <- cols_to_expand[i] + inc
  #find all the subsequent columns affected
  affected_cols <- seq(c+1,(ncol(sky_matrix)+inc))
  #find all these columns in df and augment
  g_df_changed$V2[which(g_df_changed$V2 %in% affected_cols)] <- g_df_changed$V2[which(g_df_changed$V2 %in% affected_cols)] + inserts
}

all_distances <- c()
for(i in 1:ncol(unique_pairs)) {
  pair <- unique_pairs[,i]
  #select points from coordinates data frame
  A <- g_df_changed[pair[1],]
  B <- g_df_changed[pair[2],]
  #distance - Pythagoras who???
  d <- abs(A-B) |> sum()
  #save distance
  all_distances <- c(all_distances, d)
}

##Answer 1 -------------------------------------------------------------------------
all_distances |> sum()


##Question 2 -----------------------------------------------------------------------
#same progam - all that needs to change is the number of inserts
g_df_changed <- g_df
#each row or column that 'multiplies by a million' effectively introduces million-1 more of itself
inserts <- 1000000-1
#rows 
for(i in 1:length(rows_to_expand)) {
  inc <- (inserts*(i-1))
  #row that introduces an increment
  r <- rows_to_expand[i] + inc
  #find all the subsequent rows affected
  affected_rows <- seq(r+1,(nrow(sky_matrix)+inc))
  #find all these rows in df and augment
  g_df_changed$V1[which(g_df_changed$V1 %in% affected_rows)] <- g_df_changed$V1[which(g_df_changed$V1 %in% affected_rows)] + inserts
}

#columns 
for(i in 1:length(cols_to_expand)) {
  inc <- (inserts*(i-1))
  #row that introduces an increment
  c <- cols_to_expand[i] + inc
  #find all the subsequent columns affected
  affected_cols <- seq(c+1,(ncol(sky_matrix)+inc))
  #find all these columns in df and augment
  g_df_changed$V2[which(g_df_changed$V2 %in% affected_cols)] <- g_df_changed$V2[which(g_df_changed$V2 %in% affected_cols)] + inserts
}

all_distances <- c()
for(i in 1:ncol(unique_pairs)) {
  pair <- unique_pairs[,i]
  #select points from coordinates data frame
  A <- g_df_changed[pair[1],]
  B <- g_df_changed[pair[2],]
  #distance - Pythagoras who???
  d <- abs(A-B) |> sum()
  #save distance
  all_distances <- c(all_distances, d)
}

##Answer 2 -------------------------------------------------------------------------
all_distances |> sum()
