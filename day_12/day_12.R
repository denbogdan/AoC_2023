setwd("~/portfolio/AoC_2023/day_12/")
data <- read.delim("day_12_input.txt", sep=" ", header = FALSE)

l <- list()
for(i in 1:nrow(data)) {
  x <- strsplit(data[i,1], split="") |> unlist()
  #l <- c(l, length(x))
  tally <- table(x)
  total_damaged <- strsplit(data[1,2], split=",") |> unlist() |> as.numeric() |> sum()
  damaged_to_place <- total_damaged - tally["#"]
  working_to_place <- tally["?"] - damaged_to_place
  
  arrange <- c(rep("#", damaged_to_place), rep(".", working_to_place))
  l <- c(l, list(arrange))
}


