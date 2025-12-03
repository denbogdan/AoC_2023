setwd("~/portfolio/AoC_2023/day_13/")
data <- readLines("dummy.txt")

#delineate last map by adding an extra empty line
data <- c(data, "")

#parse data as a list of matrices
maps_list <- list()
maps_names <- c()
map_build <- rep(".", nchar(data[1]))
for(i in 1:length(data)) {
  row <- strsplit(data[i], split="") |> unlist() 
  if(!is.na(row[1])) {
    map_build <- rbind(map_build, row)
  }
  else {
    if(length(row) == 0) {
      map_build <- as.data.frame(map_build)
      maps_list <- c(maps_list, list(map_build))
      map_build <- rep(".", nchar(data[1]))
    } 
  }
}

#given each map, find two adjacent columns 
sum <- 0
for(x in maps_list) {
  #get rid of the dot trail
  x <- x[2:nrow(x),]
  #initialise variable that indicates whether instance should be added to the sum
  found <- FALSE
  for(j in 1:(ncol(x)-1)) {
    if(sum(x[,j] != x[,j+1]) == 0) {
      if((j == (ncol(x)-1)) | j==1) {
        found <- TRUE
        sum <- sum + j
      } else {
        found <- TRUE
        #once a match is found, check it is a perfect reflection
        #split the map in half and compare the shortest half with its equivalent
        #left half
        lh <- x[,1:j]
        #right half
        rh <- x[,(j+1):ncol(x)]
        #go each way to check identity 
        lim <- min(ncol(lh), ncol(rh)) - 1
        for(k in 1:lim) {
          c1 <- lh[,j-k]
          c2 <- rh[,k+1]
          if(sum(c1 != c2) != 0) {
            found <- FALSE
            break
          }
        }
        if(found) sum <- sum + j
      }
    }
  }
  
  for(i in 1:(nrow(x)-1)) {
    if(sum(x[i,] != x[i+1,]) == 0) {
      if((i == (nrow(x)-1)) | i==1) {
        found <- TRUE
        sum <- sum + (100*i)
      } else {
        found <- TRUE
        #once a match is found, check it is a perfect reflection
        #split the map in half and compare the shortest half with its equivalent
        #lower half
        lh <- x[1:i,]
        #upper half
        uh <- x[(i+1):nrow(x),]
        #go each way to check identity 
        lim <- min(nrow(lh), nrow(uh))-1
        for(k in 1:lim) {
          r1 <- lh[i-k,]
          r2 <- uh[k+1,]
          if(sum(r1 != r2) != 0) {
            found <- FALSE
            break
          }
        }
        if(found) sum <- sum + (100*i)
      }
    }
  }
}

###############
process_map <- function(x) {
  sum <- 0
  #get rid of the dot trail
  x <- x[2:nrow(x),]
  #initialise variable that indicates whether instance should be added to the sum
  found <- FALSE
  for(j in 1:(ncol(x)-1)) {
    if(sum(x[,j] != x[,j+1]) == 0) {
      print(j)
      if((j == (ncol(x)-1)) | j==1) {
        found <- TRUE
        sum <- sum + j
      } else {
        found <- TRUE
        #once a match is found, check it is a perfect reflection
        #split the map in half and compare the shortest half with its equivalent
        #left half
        lh <- x[,1:j]
        #right half
        rh <- x[,(j+1):ncol(x)]
        #go each way to check identity 
        lim <- min(ncol(lh), ncol(rh)) - 1
        for(k in 1:lim) {
          c1 <- lh[,j-k]
          c2 <- rh[,k+1]
          if(sum(c1 != c2) != 0) {
            found <- FALSE
            break
          }
        }
        if(found) sum <- sum + j
      }
    }
  }
  
  for(i in 1:(nrow(x)-1)) {
    if(sum(x[i,] != x[i+1,]) == 0) {
      if((i == (nrow(x)-1)) | i==1) {
        found <- TRUE
        sum <- sum + (100*i)
      } else {
        found <- TRUE
        #once a match is found, check it is a perfect reflection
        #split the map in half and compare the shortest half with its equivalent
        #lower half
        lh <- x[1:i,]
        #upper half
        uh <- x[(i+1):nrow(x),]
        #go each way to check identity 
        lim <- min(nrow(lh), nrow(uh))-1
        for(k in 1:lim) {
          r1 <- lh[i-k,]
          r2 <- uh[k+1,]
          if(sum(r1 != r2) != 0) {
            found <- FALSE
            break
          }
        }
        if(found) sum <- sum + (100*i)
      }
    }
  }
  return(sum)
}


process_map(maps_list[[1]])
process_map(maps_list[[4]])
maps_list[[4]]


process_map_2 <- function(x) {
  found <- FALSE
  while(!found) {
    for(j in 1:(ncol(x)-1)) {
      if(sum(x[,j] != x[,j+1]) == 0) {
        if((j == (ncol(x)-1)) | j==1) {
          found <- TRUE
        } else {
          found <- TRUE
          #once a match is found, check it is a perfect reflection
          #split the map in half and compare the shortest half with its equivalent
          #left half
          lh <- x[,1:j]
          #right half
          rh <- x[,(j+1):ncol(x)]
          #go each way to check identity 
          lim <- min(ncol(lh), ncol(rh)) - 1
          for(k in 1:lim) {
            c1 <- lh[,j-k]
            c2 <- rh[,k+1]
            if(sum(c1 != c2) != 0) {
              found <- FALSE
              break
            }
          }
        }
      }
    }
    if(found) return(j)
    if(!found) {
      found <- TRUE
      return(0)
    }
  }
}

process_map_2(maps_list[[1]])
process_map_2(maps_list[[4]])
maps_list[[4]]



sum <- 0 
for(x in maps_list) {
  x <- x[2:nrow(x),]
  #look for vertical
  sum <- sum + process_map_2(x)
  #look for horizontal 
  sum <- sum + (100*process_map_2(t(x)))
}

sum
