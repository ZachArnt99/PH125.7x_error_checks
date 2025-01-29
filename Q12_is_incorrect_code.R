library(tidyverse)
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

#This method leaves 20 rows with duplicate playerID. 
which(duplicated(bat_02$playerID))

#Look at alomasa02 in rows 7 and 8, for example.
head(bat_02, n=10)

#To compare hitting performance in time frame A vs time frame B by playerID, we need to group_by(playerID) and reframe in each time frame.
bat_02_modified <- Batting |> filter(yearID == 2002) |> 
  mutate(pa = AB + BB, singles = H - X2B - X3B - HR) |>
  select(playerID, pa, singles, BB) |> group_by(playerID) |> 
  reframe(total_pa_2002 = sum(pa), singles_rate_2002 = sum(singles)/total_pa_2002,
          bb_rate_2002 = sum(BB)/total_pa_2002) |> filter(total_pa_2002 >= 100)

#By filtering at the end, we also capture batting data for 6 players who had 100 or more personal appearances in 2002, even though they had less than 100 personal appearances per stint.
ab <- which(bat_02_modified$playerID %in% bat_02$playerID == FALSE)
six_more <- bat_02_modified[ab,]
six_more
six_more_by_stint <- Batting[which(Batting$playerID %in% six_more$playerID & Batting$yearID == 2002),c("playerID","yearID","stint","AB","BB")]
six_more_by_stint

#Now, our 2002 data set has 436 rows with no duplicates instead of 450 rows with 20 duplicates.
bat_02_modified
sum(duplicated(bat_02_modified$playerID))

#Use the same approach to gather the data for 1999-2001
bat_other <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = H - X2B - X3B - HR) |> 
  select(playerID, pa, singles, BB) |> group_by(playerID) |>
  reframe(total_pa_other = sum(pa), singles_rate_other = sum(singles)/total_pa_other, bb_rate_other = sum(BB)/total_pa_other) |> filter(total_pa_other >= 100) 
#Check the data for duplicates.
nrow(bat_other)
length(unique(bat_other$playerID))

#Answer Question 9
#Using the recommended code, I got 46, but I think 48 is a better answer.
sum(bat_other$singles_rate_other > 0.2)
#Using the recommended code, I got 3, but I think 2 is a better answer.
sum(bat_other$bb_rate_other > 0.2)

#Join the data into one tibble
joined <- inner_join(bat_02_modified, bat_other, by = "playerID")
names(joined)

#Answer Question 10
#Using the recommended code, I got 0.5525, but I think 0.5634 is a better answer.
cor(joined$singles_rate_2002,joined$singles_rate_other)
#Using the recommended code, I got 0.7259, but I think 0.7296 is a better answer.
cor(joined$bb_rate_2002,joined$bb_rate_other)

#Answer Question 11
#There is an oval-shaped envelope with positive slope in both plots, indicating positively correlated bivariate normal distributions. The second plot has a thinner oval, so the correlation is stronger.
ggplot(data = joined, aes(x = singles_rate_2002, y = singles_rate_other)) + geom_point()
ggplot(data = joined, aes(x = bb_rate_2002, y = bb_rate_other)) + geom_point()

#Answer Question 12
#Using the recommended code, I got 0.5826, but I think 0.6226 is a better answer.
model_singles <- lm(joined$singles_rate_2002 ~ joined$singles_rate_other)
summary(model_singles)
#Using the recommended code, I got 0.8164, but I think 0.8173 is a better answer.
model_bb <- lm(joined$bb_rate_2002 ~ joined$bb_rate_other)
summary(model_bb)
