#Setting working directory to project folder 
getwd()

leoka <- read.csv("leoka.csv")

#Previewing Data
head(leoka)
names(leoka)

#2 Printing first 3 rows
leoka[1:3,]

library(dplyr)
#3 How many officers were killed in Ohio in 2007?
ohio <- leoka[leoka$State == "Ohio",]
ohio_2007 <- ohio$X2007
print(ohio_2007)

#4 How many officers were killed in the East North Central region in 2012?
ENC <- leoka[leoka$Region2 == "East North Central",]
ENC_2012 <- ENC$X2012
print(ENC_2012)
sum(ENC_2012, na.rm = TRUE)

#5 Add a column showing the total number of officers killed in 
#each state (that is, add up all the years for each state)
leoka$total <- rowSums(leoka[ , 4:24], na.rm=TRUE)

print(leoka$total)

#6 Print the state having the largest number of officers killed
a <- leoka[leoka$total == max(leoka$total), ]
print(a[1])

#7 Print the region (Region 1) having the largest total number of officers killed
print(a[2])

#8 Find all the states that have had no officers killed between 2003 and 2023
no_deaths <- leoka[leoka$total == 0, "State"]
print(no_deaths)

#9 In which year and in which state were nine officers killed? 
#(Ideas: #1 explore which() and its arr.ind parameter, #2 explore pivot_longer())
c <- which(leoka[, 4:24] == 9, arr.ind = TRUE)
print(c)
state <- leoka$State[c[1]]
year <-colnames(leoka) [c[2]+3]
cat("State:",state, "Year:", year, "\n")

#10 Sort the regions (Region 1) from smallest to largest in terms of the 
#number of officers killed in 2022

NE_T <- sum(leoka$X2022[leoka$Region1 == "NORTHEAST"])
MID_T <- sum(leoka$X2022[leoka$Region1 == "MIDWEST"])
SOUTH_T <- sum(leoka$X2022[leoka$Region1 == "SOUTH"])
WEST_T <- sum(leoka$X2022[leoka$Region1 == "WEST"])

totals <- c(NE_T, MID_T, SOUTH_T, WEST_T)
regions <- c("NORTHEAST", "MIDWEST", "SOUTH", "WEST")
ascending_r <- sort(totals)
ascending_r
print(ascending_r)

#alternative way to answer the question

leoka |>
  group_by(Region1) |>
  summarise(e=sum(X2022)) |>
  arrange(e)

#11 Create a bar chart (using barplot()) showing the number of officers killed by region (Region 1)
#In addition to your script, upload a Word doc, Powerpoint slide, 
#or pdf that shows you inserting your bar chart into a document using a vector graphics
#format (SVG, EMF, or PDF)... not a bitmap image (no JPG, PNG, GIF, BMP). 
#Google Slides has become more difficult in working with vector graphics formats 
#and Google Docs seems to block them completely. If you do not like the Word/
#Powerpoint options, then you can submit a PDF with your plot using Quarto. 
#I've created a Quarto template for assignment #1 Download Quarto template for
#assignment #1if you want to try it out.

region_totals <- rowsum(leoka$total, leoka$Region1)

barplot(
  region_totals[,1],
  col = "pink",
  main = "Total Officers Killed by Region",
  ylab = "Number of Officers Killed",
  xlab = "Region"
)

#12 Plot the number of total number of officers killed by year
year_totals <- colSums(leoka[ , 4:24], na.rm = TRUE)

barplot(
  year_totals,
  col = "lightblue",
  main = "Total Officers Killed by Year (2003–2023)",
  ylab = "Number of Officers Killed",
  xlab = "Year",
  las = 2
)

alt 

totals <- leoka |>
  

#13 Which region (Region2) showed the largest percent decrease in 2023 over 
#the average of 2003-2022? (Lots of directions to go here. 
#Perhaps first consider making a new variable that has the 2003-2022 
#totals in each row. Then consider pivot_longer()/group_by()/summarize())


# total officers killed in each state from 2003–2022
leoka$total_2003_2022 <- rowSums(leoka[, 4:23], na.rm = TRUE)



# average per year (divide by 20 years)
leoka$avg_2003_2022 <- leoka$total_2003_2022 / 20


library(dplyr)

region2_summary <- leoka %>%
  group_by(Region2) %>%
  summarize(
    avg_2003_2022 = sum(avg_2003_2022, na.rm = TRUE),  # baseline average
    total_2023 = sum(X2023, na.rm = TRUE)              # 2023 total
  )


region2_summary <- region2_summary %>%
  mutate(
    percent_change = ((total_2023 - avg_2003_2022) / avg_2003_2022) * 100
  )


worst_region <- region2_summary %>%
  arrange(percent_change) %>%
  slice(1)

print(worst_region)




