setwd("~/Projects/AoC_2023/day_2/")
data <- read.delim("day_2_input.txt", header=FALSE, sep=":")
#max cubes
max_cubes <- c("red" = 12, "green" = 13, "blue" = 14)

##Question 1 -----------------------------------------------------------------------
saved <- c()
for(i in 1:nrow(data)) {
  #select row
  row <- data$V2[i]
  #split row into draws
  row_split <- strsplit(row, split = ";") |> unlist()
  #initialise game as OK
  game_OK <- TRUE
  #look out for any combinations not permitted
  for(j in 1:length(row_split)) {
    if(game_OK) {
      #tidy the draw
      draw <- gsub(" ", "", row_split[j])
      draw <- strsplit(draw, split=",") |> unlist()
      numbers <- gsub("[^0-9.-]", "", draw) |> as.numeric()
      names(numbers) <- gsub("[[:digit:]]","",draw)
      #compare with max cubes
      for(z in 1:length(numbers)) {
        if(max_cubes[names(numbers[z])] < numbers[z]) {
          game_OK <- FALSE
          break
        }
      }
    }
  }
  if(game_OK) saved <- c(saved, i)
}

##Answer 1 -------------------------------------------------------------------------
sum(saved)

##Question 2 -----------------------------------------------------------------------
powers <- c()
for(i in 1:nrow(data)) {
  #select row
  row <- data$V2[i]
  #split row into draws
  row_split <- strsplit(row, split = ";") |> unlist()
  #initialise max for the game
  game_max <- c(0,0,0)
  names(game_max) <- c("red", "green", "blue")
  #go through each draw to find the max number of each colour
  for(j in 1:length(row_split)) {
      #tidy the draw
      draw <- gsub(" ", "", row_split[j])
      draw <- strsplit(draw, split=",") |> unlist()
      numbers <- gsub("[^0-9.-]", "", draw) |> as.numeric()
      names(numbers) <- gsub("[[:digit:]]","",draw)
      #find the max for each colour
      for(z in 1:length(numbers)) {
        if(game_max[names(numbers[z])] < numbers[z]) {
          game_max[names(numbers[z])] <- numbers[z] 
      }
    }
  }
  powers <- c(powers, prod(game_max))
}

##Answer 2 -------------------------------------------------------------------------
sum(powers)
