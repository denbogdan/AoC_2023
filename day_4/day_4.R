setwd("~/portfolio/AoC_2023/day_4/")
data <- read.delim("day_4_input.txt", header=FALSE, sep="|")

#tidy data 
data$V1 <- gsub(".*: ", "", data$V1)
data$V2 <- gsub(".*: ", "", data$V2)

##Question 1 -----------------------------------------------------------------------
points <- c()
for(i in 1:nrow(data)) {
  n <- 0
  #select winning
  winning <- data$V1[i]
  winning <- strsplit(winning, split=" ") |> unlist() |> as.numeric()
  #select draw
  draw <- data$V2[i]
  draw <- strsplit(draw, split=" ") |> unlist() |> as.numeric()
  draw <- draw[!is.na(draw)]
  #count
  overlap <- draw[draw %in% winning]
  n <- length(overlap)
  print(n)
  #calculate points
  points <- c(points, ifelse(n>0,2^(n-1),0))
}

##Answer 1 -------------------------------------------------------------------------
sum(points)


##Question 2 -----------------------------------------------------------------------
win_loss <- c()
copies <- c()
for(i in 1:nrow(data)) {
  n <- 0
  #select winning
  winning <- data$V1[i]
  winning <- strsplit(winning, split=" ") |> unlist() |> as.numeric()
  #select draw
  draw <- data$V2[i]
  draw <- strsplit(draw, split=" ") |> unlist() |> as.numeric()
  draw <- draw[!is.na(draw)]
  #count
  overlap <- draw[draw %in% winning]
  n <- length(overlap)
  #calculate points
  if(n>0) 
    win_loss <- c(win_loss, 1)
  else
    win_loss <- c(win_loss, 0)
  copies <- c(copies, n)
}

#write recursive function to count the cards
count_cards <- function(i) {
  win_loss_status <- win_loss[i]
  copies_to_generate <- copies[i]
  tally <<- tally+1
  if(win_loss_status == 1) {
    for(j in 1:copies_to_generate) {
      if(i+j <= length(win_loss))
        count_cards(i+j)
    }
  }
}

all_copies <- c()
for(i in 1:length(win_loss)) {
  tally <- 0
  count_cards(i)
  all_copies <- c(all_copies, tally)
}

##Answer 2 -------------------------------------------------------------------------
sum(all_copies)
