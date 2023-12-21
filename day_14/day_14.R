setwd("~/portfolio/AoC_2023/day_14/")
data <- readLines("dummy.txt")

#make base input into data matrix 
split_list <- sapply(data, function(x) strsplit(x, split = "")) 
dish_matrix <- split_list[[1]]
for(i in 2: length(data)) 
  dish_matrix <- rbind(dish_matrix, split_list[[i]])

##Question 1 -----------------------------------------------------------------------
#add a . at the end of the matrix (south side)
dish_matrix <- rbind(dish_matrix, rep(".", ncol(dish_matrix)))

#for each column rearrange everything but the #
s <- 0
for(i in 1:ncol(dish_matrix)) {
  #select column
  column <- dish_matrix[,i]
  print(column)
  #split column by #
  #strsplit(paste0(column, collapse=""), split = "#") |> unlist()
  #arrange moving bits 
  portion <- c()
  column_ordered <- c()
  for(j in 1:length(column)) {
    if(column[j] != "#") {
      portion <- c(portion, column[j])
    } else {
      if(column[j] == "#") {
        #arrange and score what we have already
        if(length(portion) > 0) {
          #reorder
          moving <- portion |> sort() |> rev()
          column_ordered <- c(column_ordered, moving, "#")
          portion <- c()
        } else column_ordered <- c(column_ordered, "#")
      }
    }
  }
  
  #for the last portion, if there is no # add the remaining but
  moving <- portion |> sort() |> rev()
  column_ordered <- c(column_ordered, moving)
  
  #travel through and add up
  column_ordered <- rev(column_ordered)[2:length(column_ordered)]
  for(j in 1:length(column_ordered)) {
    if(column_ordered[j] == "O")
      s <- s + j
  }
}

##Answer 1 -------------------------------------------------------------------------
s

##Question 2 -----------------------------------------------------------------------
#write function that takes vector and returns it reordered
#my version
reorder_line <- function(line) {
  portion <- c()
  ordered <- c()
  for(j in 1:length(line)) {
    if(line[j] != "#") {
      portion <- c(portion, line[j])
    } else {
      if(line[j] == "#") {
        #arrange and score what we have already
        if(length(portion) > 0) {
          #reorder
          moving <- portion |> sort() |> rev()
          ordered <- c(ordered, moving, "#")
          portion <- c()
        } else ordered <- c(ordered, "#")
      }
    }
  }
  
  #for the last portion, if there is no # add the remaining but
  moving <- portion |> sort() |> rev()
  ordered <- c(ordered, moving)

  #return new line  
  return(ordered)
}

tilt_dish <- function(matrix, direction) {
  if(direction == "north") {
    #add a line of . at the south border - to be removed
    matrix <- rbind(matrix, rep(".", ncol(matrix)))
    for(i in 1:ncol(matrix)) {
      matrix[,i] <- reorder_line(matrix[,i])
    }
    #remove line at the south
    matrix <- matrix[1:(nrow(matrix)-1),]
  }
  
  if(direction == "east") {
    #add a line of . at the east border - to be removed
    matrix <- cbind(rep(".", nrow(matrix)), matrix)
    for(i in 1:nrow(matrix)) {
      matrix[i,] <- rev(reorder_line(rev(matrix[i,])))
    }
    #remove line at the east
    matrix <- matrix[,2:ncol(matrix)]
  }
  
  if(direction == "south") {
    #add a line of . at the north border - to be removed
    matrix <- rbind(rep(".", ncol(matrix)), matrix)
    for(i in 1:ncol(matrix)) {
      matrix[,i] <- rev(reorder_line(rev(matrix[,i])))
    }
    #remove line at the north
    matrix <- matrix[2:nrow(matrix),]
  }
  
  if(direction == "west") {
    #add a line of . at the west border - to be removed
    matrix <- cbind(matrix, rep(".", nrow(matrix)))
    for(i in 1:nrow(matrix)) {
      matrix[i,] <- reorder_line(matrix[i,])
    }
    #remove line at the west
    matrix <- matrix[,1:(ncol(matrix)-1)]
  }
  
  return(matrix)
}

m <- dish_matrix
for(i in 1:1000000000) {
  m <- tilt_dish(m, "north")
  m <- tilt_dish(m, "west")
  m <- tilt_dish(m, "south")
  m <- tilt_dish(m, "east")
}

#score the result
s <- 0
for(i in 1:ncol(m)) {
  l <- rev(m[i])
  for(j in 1:length(l)) {
    if(l[j] == "O")
      s <- s + j
  }
}

#memoise 
memoised_reorder_line <- memoise(reorder_line)
#test(memoisation)
microbenchmark(reorder_line(m[,1]))
microbenchmark(memoised_reorder_line(m[,1]))


tilt_dish <- function(matrix, direction) {
  if(direction == "north") {
    #add a line of . at the south border - to be removed
    matrix <- rbind(matrix, rep(".", ncol(matrix)))
    for(i in 1:ncol(matrix)) {
      matrix[,i] <- memoised_reorder_line(matrix[,i])
    }
    #remove line at the south
    matrix <- matrix[1:(nrow(matrix)-1),]
  }
  
  if(direction == "east") {
    #add a line of . at the east border - to be removed
    matrix <- cbind(rep(".", nrow(matrix)), matrix)
    for(i in 1:nrow(matrix)) {
      matrix[i,] <- rev(memoised_reorder_line(rev(matrix[i,])))
    }
    #remove line at the east
    matrix <- matrix[,2:ncol(matrix)]
  }
  
  if(direction == "south") {
    #add a line of . at the north border - to be removed
    matrix <- rbind(rep(".", ncol(matrix)), matrix)
    for(i in 1:ncol(matrix)) {
      matrix[,i] <- rev(memoised_reorder_line(rev(matrix[,i])))
    }
    #remove line at the north
    matrix <- matrix[2:nrow(matrix),]
  }
  
  if(direction == "west") {
    #add a line of . at the west border - to be removed
    matrix <- cbind(matrix, rep(".", nrow(matrix)))
    for(i in 1:nrow(matrix)) {
      matrix[i,] <- memoised_reorder_line(matrix[i,])
    }
    #remove line at the west
    matrix <- matrix[,1:(ncol(matrix)-1)]
  }
  
  return(matrix)
}

memoised_tilt_dish <- memoise(tilt_dish)

m <- dish_matrix
for(i in 1:1000000000) {
  m <- memoised_tilt_dish(m, "north")
  m <- memoised_tilt_dish(m, "west")
  m <- memoised_tilt_dish(m, "south")
  m <- memoised_tilt_dish(m, "east")
}

#George's version
r_line <- memoise(function(line) {
  l <- stringr::str_locate_all(paste0(line, collapse = ""), pattern = "[^#]+")[[1]]
  for(j in 1:nrow(l)) {
    st <- sort(line[l[j,1] : l[j,2]], decreasing=TRUE)
    line[l[j,1] : l[j,2]] <- st
  }
  return(line)
})

tilt_cycle <- memoise(function(matrix) {
  #tilt north
  for(i in 1:ncol(matrix)) 
    matrix[,i] <- r_line(matrix[,i])
  
  #tilt west
  for(i in 1:nrow(matrix)) 
    matrix[i,] <- r_line(matrix[i,])
  
  #tilt south
  for(i in 1:ncol(matrix)) 
    matrix[,i] <- rev(r_line(rev(matrix[,i])))
  
  #tilt east
  for(i in 1:nrow(matrix)) 
    matrix[i,] <- rev(r_line(rev(matrix[i,])))
  
  return(matrix)
})

m <- dish_matrix
for(i in 1:1000000000) {
  m <- tilt_cycle(m)
}

