sky <- readLines("dummy.txt")

#make into data matrix 
split_list <- sapply(sky, function(x) strsplit(x, split = "")) 
sky_matrix <- split_list[[1]]
for(i in 2: length(sky)) {
  sky_matrix <- rbind(sky_matrix, split_list[[i]])
}

#find all the empty rows
row_tally <- apply(sky_matrix, MARGIN = 1, table)
spaces <- lapply(row_tally, function(x) {x["."]}) |> unlist()
rows_to_expand <- which(spaces == ncol(sky_matrix)) |> as.numeric()
#find all the empty columns
col_tally <- apply(sky_matrix, MARGIN = 2, table)
spaces <- lapply(col_tally, function(x) {x["."]}) |> unlist()
cols_to_expand <- which(spaces == nrow(sky_matrix)) |> as.numeric() 


g_df <- c(0,0)
for(i in 1:nrow(sky_matrix)) {
  for(j in 1:ncol(sky_matrix)) {
    if(sky_matrix[i,j] == "#")
      g_df <- rbind(g_df, c(i,j))
  }
}

#tidy df
g_df <- g_df %>% as.data.frame() %>% dplyr::filter(V2 != 0)

#generate all unique pairs
unique_pairs <- combn(1:nrow(g_df), 2)

g_df_changed <- g_df
inserts <- 1
#rows 
#inc <- 0
for(i in 1:length(rows_to_expand)) {
  #print(i)
  inc <- (inserts*(i-1))
  #print(inc)
  #row that introduces and increment
  r <- rows_to_expand[i] + inc
  #find all the subsequent rows affected
  affected_rows <- seq(r+1,nrow(new_sky_final))
  #find all these rows in df and augment
  g_df_changed$V1[which(g_df_changed$V1 %in% affected_rows)] <- g_df_changed$V1[which(g_df_changed$V1 %in% affected_rows)] + inserts
}

#columns 
#inc <- 0
for(i in 1:length(cols_to_expand)) {
  inc <- (inserts*(i-1))
  #row that introduces and increment
  c <- cols_to_expand[i] + inc
  print(c)
  #find all the subsequent rows affected
  affected_cols <- seq(c+1,ncol(new_sky_final))
  print(affected_cols)
  #find all these rows in df and augment
  g_df_changed$V2[which(g_df_changed$V2 %in% affected_cols)] <- g_df_changed$V2[which(g_df_changed$V2 %in% affected_cols)] + inserts
}

all_distances <- c()
for(i in 1:ncol(unique_pairs)) {
  pair <- unique_pairs[,i]
  #select rows from coordinates data frame
  A <- g_df_changed[pair[1],]
  B <- g_df_changed[pair[2],]
  #distance - Pythagoras who???
  d <- abs(A-B) |> sum()
  #save distance
  all_distances <- c(all_distances, d)
}

##Answer 2 -------------------------------------------------------------------------
all_distances |> sum()

