setwd("~/Projects/AoC_2023/day_9/")
nb <- read.delim("day_9_input.txt", header=FALSE, sep=" ")

##Question 1 -----------------------------------------------------------------------
all_predictions <- c()
for(l in 1:nrow(nb)) {
  line <- nb[l,] |> as.numeric()
  prediction <- 0
  #find all the numbers that are at the end of the line
  line_enders <- rev(line)[1]
  while(sum(line != 0)) {
    new_line <- c()
    for(i in 1:(length(line)-1)) {
      new_line <- c(new_line, line[i+1]-line[i])
    }
    line_enders <- c(line_enders, rev(new_line)[1])
    line <- new_line
  }
  
  #sum line enders
  all_predictions <- c(all_predictions, sum(line_enders))
}

##Answer 1 -------------------------------------------------------------------------
sum(all_predictions)


##Question 2 -----------------------------------------------------------------------
all_predictions <- c()
for(l in 1:nrow(nb)) {
  line <- nb[l,] |> as.numeric()
  prediction <- 0
  #find all the numbers that are at the start of the line
  line_starters <- line[1]
  while(sum(line != 0)) {
    new_line <- c()
    for(i in 1:(length(line)-1)) {
      new_line <- c(new_line, line[i+1]-line[i])
    }
    line_starters <- c(line_starters, new_line[1])
    line <- new_line
  }
  new_line_starters <- c()
  #use line starters to calculate value
  value <- 0
  for(e in rev(line_starters)) {
    new_line_starters <- c(new_line_starters, e-value)
    value <- e-value
  }
  x <- rev(new_line_starters)[1]
  #sum line starters
  all_predictions <- c(all_predictions, x)
}

##Answer 2 -------------------------------------------------------------------------
sum(all_predictions)


