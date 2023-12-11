setwd("~/Projects/AoC_2023/day_8/")
data <- readLines("day_8_input.txt")

#parse instructions 
inst <- strsplit(data[1], split="") |> unlist()
inst <- ifelse(inst=="L", 1, 2)
#make sure inst are long enough
inst <- rep(inst, 1000000)

#parse locations as data frame
locations <- data[3:length(data)]
locations <- gsub(" = ", ",", locations)
locations <- gsub("\\(", "", locations)
locations <- gsub("\\)", "", locations)
locations <- gsub(" ", "", locations)
x <- locations[1] 
x <- strsplit(x, split = ",") |> unlist()
l_df <- x[c(2,3)]
l_names <- x[1]
for(i in 2:length(locations)) {
  x <- locations[i] 
  x <- strsplit(x, split = ",") |> unlist()
  l_df <- rbind(l_df, x[c(2,3)])
  l_names <- c(l_names, x[1])
}

l_df <- as.data.frame(l_df, row.names = l_names)


##Question 1 -----------------------------------------------------------------------
#start at AAA
place <- "AAA"
index <- 1
while(place != "ZZZ") {
  #select where to go
  place <- l_df[place,inst[index]]
  #increment index
  index <- index+1
}

##Answer 1 -------------------------------------------------------------------------
index-1


##Question 2 -----------------------------------------------------------------------
#find all start places
all_locations <- strsplit(rownames(l_df), split = "")
places <- l_df[(lapply(all_locations, function(x) { x[3] == "A"}) |> unlist()) ,]
places <- rownames(places)
#for each place, calculate the number of steps required
steps_required <- c()
for (p in places) {
  index <- 1
  final_z <- FALSE
  while(!final_z) {
    #select where to go
    p <- l_df[p,inst[index]]
    #find the last letter
    pl <- strsplit(p, "") |> unlist()
    last_letter <- pl[3]
    if(last_letter == "Z")
      final_z <- TRUE
    #increment index
    index <- index+1
  }
  steps_required <- c(steps_required, index-1)
}

#sanity check all values require an integer number of repeats through the instructions
steps_required

#find the lowest common multiple
library(pracma)
L <- Lcm(steps_required[1], steps_required[2])
for(i in 3:length(steps_required)) {
  L <- Lcm(L, steps_required[i])
}

##Answer 2 -------------------------------------------------------------------------
options(scipen = 999)
L

#iterative solution - way too slow
#final_z <- FALSE
# while(!final_z) {
#   places <- l_df[places,inst[index]]
#   #find the last letters
#   pl <- strsplit(places, "")
#   last_letters <- lapply(pl, function(x) x[3]) |> unlist()
#   if(length(setdiff(last_letters, rep("Z", length(places)))) == 0)
#     final_z <- TRUE
#   #increment index
#   index <- index+1
# }
