setwd("~/Projects/AoC_2023/day_7/")
bids <- read.delim("day_7_input.txt", sep = " ", header=FALSE)
#parse the input hands
hands <- strsplit(bids$V1, split="")

##Question 1 -----------------------------------------------------------------------
#rules
cards <- c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2") 
kinds <- list("5kind" = c(5), "4kind" = c(4, 1), "FH" = c(3, 2), "3kind" = c(3, 1, 1),
              "twopair" = c(2, 2, 1), "onepair" = c(2, 1, 1, 1), "HC" = c(1, 1, 1, 1, 1)) 

#for each hand determine its kind
ks <- c()
for(i in 1:length(hands)) {
  #count unique cards 
  h <- hands[[i]] |> table() |> as.numeric() 
  h <- sort(h, decreasing = TRUE)
  #determine the kind
  type <- kinds[kinds %in% list(h)] |> names()
  print(type)
  #add type to vect
  ks <- c(ks, type)
}

#count how many of each there are in input
ks_table <- table(ks)
ks_table_vect <- ks_table |> as.numeric()
names(ks_table_vect) <- names(ks_table)

#for each hand, determine how many are weaker
all_ranks <- c()
all_hands <- c()
for(i in 1:length(hands)) {
  #select hand
  h <- hands[[i]]
  #select kind
  k <- ks[[i]]
  #determine which kind (if any) is stronger and count those
  rank <- 0
  #determine index of kind
  k_i <- which(names(kinds) == k)
  if(k_i != 7) {
    #add up all the cards of a weaker kind 
    rank <- rank + sum(ks_table_vect[names(ks_table_vect) %in% names(kinds)[(k_i+1):7]])
  }
  #within the same kind, find how many are weaker
  #subset only the same kind
  for(pos in which(ks == k)) {
    if(pos != i) {
      h_to_compare <- hands[[pos]]
      stronger <- TRUE
      #go through all characters until you find whether it is weaker or not
      for(j in 1:length(h)) {
        #find rank of current hand
        r_h <- which(cards==h[j])
        r_h_to_compare <- which(cards==h_to_compare[j])
        if(r_h < r_h_to_compare) {
          stronger <- FALSE
          break
        } else {
          if(r_h > r_h_to_compare) break
        }
      }
      if(!stronger) rank <- rank+1
    } else rank <- rank + 1 
  }
  all_ranks <- c(all_ranks, rank)
  all_hands <- c(all_hands, paste0(h, collapse=""))
}

names(all_ranks) <- all_hands
#sanity check
length(unique(all_ranks))

##Answer 1 -------------------------------------------------------------------------
sum(all_ranks*bids$V2)

##Question 2 -----------------------------------------------------------------------
#rule change
cards <- c("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J") 

#for each hand, determine its kind under the new rules
ks <- c()
for(i in 1:length(hands)) {
  #count unique cards 
  h <- hands[[i]] |> table() |> as.numeric() 
  names(h) <- names(hands[[i]] |> table())
  h <- sort(h, decreasing = TRUE)
  #determine the kind
  if("J" %in% hands[[i]]) {
    #determine current type
    type <- kinds[kinds %in% list(h |> as.numeric())] |> names() 
    possible_types <- c(which(names(kinds)==type))
    if(type != "5kind") {
      #run through all card types present and decide which one yields the highest card possible
      #find J positions
      Js_pos <- which(hands[[i]] == "J")
      for(z in names(h)) {
        hand_playing <- hands[[i]]
        hand_playing[Js_pos] <- z
        #determine new type
        h_new <- hand_playing |> table() |> as.numeric() 
        names(h_new) <- names(hand_playing |> table())
        h_new <- sort(h_new, decreasing = TRUE)
        type_new <- kinds[kinds %in% list(h_new |> as.numeric())] |> names() 
        #find its rank
        type_new_rank <- which(names(kinds)==type_new)
        #add to list
        possible_types <- c(possible_types, type_new_rank)
      }
    }
    #find the highest type
    type <- names(kinds)[min(possible_types)]
  } else {
    type <- kinds[kinds %in% list(h |> as.numeric())] |> names()
  }
  #add type to vect
  ks <- c(ks, type)
}
