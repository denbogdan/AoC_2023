setwd("~/portfolio/AoC_2023/day_1/")
data <- read.delim("day_1_input.txt", header=FALSE)

##Question 1 -----------------------------------------------------------------------
numbers <- c()
for (i in 1:length(data$V1)) {
  #select row
  s <- data$V1[i]
  
  #select first digit
  first_digit <- gsub(".*?([0-9]).*", "\\1", s)
  
  #reverse string
  rev <- strsplit(s, "") |> unlist() |> rev()
  rev <- paste(rev, collapse = "")
  #select "last" digit
  last_digit <- gsub(".*?([0-9]).*", "\\1", rev)
  
  #make number
  nb <- paste0(first_digit, last_digit, collapse = "") |> as.numeric()
  #add to vect
  numbers <- c(numbers, nb)
}

##Answer 1 -------------------------------------------------------------------------
sum(numbers)

##Question 2 -----------------------------------------------------------------------
digits <- c("one" = 1, "two" = 2, "three" = 3, "four" = 4,
            "five" = 5, "six" = 6, "seven" = 7, "eight" = 8, "nine" = 9)
numbers <- c()

for (i in 1:length(data$V1)) {
  #select row
  s <- strsplit(data$V1[i], "") |> unlist()

  ## FIND FIRST DIGIT
  first_digit <- "n"
  #select first digit OR spelled number
  pre <- c()
  for(j in 1:length(s)) {
      if(is.na(as.numeric(s[j]))) {
        pre <- c(pre, s[j])
        pre_pasted <- paste(pre, collapse="")
        #look for a meaningful number until one is found
        for(n in names(digits)) {
          if(length(grep(n, pre_pasted)) == 1) {
            first_digit <- as.character(digits[n])
            break
          }
        }
    if(first_digit != "n") break
  } else {
      first_digit <- s[j]
      break
  }
}

  ## FIND LAST DIGIT
  #reverse string
  rev_s <- strsplit(s, "") |> unlist() |> rev()
  #initiate last digit
  last_digit <- "n"
  #select first digit or spelled number
  post <- c()
  for(j in 1:length(rev_s)) { 
    if(is.na(as.numeric(rev_s[j]))) {
      post <- c(post, rev_s[j]) 
      post_pasted <- paste(post |> rev(), collapse="") 
      
      for(n in names(digits)) { 
        if(length(grep(n, post_pasted)) == 1) {
          last_digit <- as.character(digits[n])
          break
        }
      }
      if(last_digit != "n") break
    } else {
      last_digit <- rev_s[j]
      break
    }
  }

  #make number
  nb <- paste0(first_digit, last_digit, collapse = "") |> as.numeric()
  #add to vect
  numbers <- c(numbers, nb)
}

##Answer 2 -------------------------------------------------------------------------
sum(numbers)


