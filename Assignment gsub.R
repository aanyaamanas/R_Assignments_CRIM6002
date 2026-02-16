getwd()
words <- scan(file = "scrabble4letter.txt", what = "")

#Using grep 
#1 All words with double letters 
grep("(.)\\1", words, value = TRUE)

#2 Palindromes 
grep("^(.)(.)\\2\\1$", words, value = TRUE)

#Using gsub to make edits 
#1 Replace all double "I"s with a single I 
replacing <- grep("ii", words, value = TRUE)
gsub("ii", "i", replacing)

#2 Capitalise all words starting with a "g"
capitalise <- grep("^g", words, value = TRUE)
gsub("^g", "G", capitalise)


#3 Delete last 3 letters of every word
gsub(".{3}$", "", words)

#4 Using table to tabulate the most common starting letter for 4 letter words
first_letter <- gsub(".{3}$", "", words)
sort(table(first_letter))
