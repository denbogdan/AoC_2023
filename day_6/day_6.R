setwd("~/Projects/AoC_2023/day_6/")
data <- read.delim("day_6_input.txt", header = FALSE)
rownames(data) <- c("time", "distance")

##Question 1 -----------------------------------------------------------------------
wins <- c()
for(r in 1:ncol(data)) {
  w <- 0
  for(i in 1:(data["time",r]-1)) {
    #simulate what would happen if you held the button this long
    speed <- i
    #remaining seconds
    seconds <- data["time",r]-i
    #distance resulting
    distance <- seconds*speed
    #if a win, add to tally
    if(distance > data["distance",r])  w <- w+1
  }
  wins <- c(wins, w)
}

##Answer 1 -------------------------------------------------------------------------
prod(wins)


##Question 2 -----------------------------------------------------------------------
time <- paste0(data[1,], collapse = "") |> as.numeric()
distance_to_beat <- paste0(data[2,], collapse = "") |> as.numeric()

w <- 0
for(i in 1:(time-1)) {
  #simulate what would happen if you held the button this long
  speed <- i
  #remaining seconds
  seconds <- time-i
  #distance resulting
  distance <- seconds*speed
  #if a win, add to tally
  if(distance > distance_to_beat)  w <- w+1
}

##Answer 2 -------------------------------------------------------------------------
w

