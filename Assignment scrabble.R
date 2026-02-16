words <- scan(file = "scrabble4letter.txt", what = "")
#1 All words that start with a "z"
grep("^[zZ]", words, value = TRUE)

#2 All words that have "zz" in them 
grep("zz", words, value = TRUE)

#3 All words that do not have a "a," "e," "i," "o," or "u"
grep("^[^aeiouAEIOU]+$", words, value = TRUE)

#4 In which letter position are you most likely to find an "r"
r_positions <- c(
  "^r...$", 
  "^.r..$", 
  "^..r.$", 
  "^...r$"  
)

names(r_positions) <- c("Position 1", "Position 2", "Position 3", "Position 4")

r_counts <- sapply(r_positions, function(pattern) {

  length(grep(pattern, words))
})

position_r <- names(which.max(r_counts))
position_r
print(r_counts)
#5 Words that rhyme with "mitt"

grep("[^ao|un]itt?$", words, value = TRUE)
#adding "quit" manually 
grep("[^ao|un]itt?$|^quit$", words, value = TRUE)

