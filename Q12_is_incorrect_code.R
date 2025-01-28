library(tidyverse)
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
#Check the data
nrow(bat_02)
length(unique(bat_02$playerID))
#It looks like some playerIDs are duplicated. That's a problem.

#To compare hitting performance in time frame A vs time frame B by playerID, we group_by(playerID) in each time frame.
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  group_by(playerID) |> filter(pa >= 100) %>% select(playerID, singles, bb) |>
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))
#Check the data
nrow(bat_02)
length(unique(bat_02$playerID))

bat_other <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  group_by(playerID) |> filter(pa >= 100) %>% select(playerID, singles, bb) |> 
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))
#Check the data
nrow(bat_other)
length(unique(bat_other$playerID))

#Make unique column names where appropriate
names(bat_02)
names(bat_02)[2:3] <-  paste(names(bat_02[2:3]), "2002", sep = "_")
names(bat_02)
names(bat_other)
names(bat_other)[2:3] <- paste(names(bat_other)[2:3],"other", sep = "_")
names(bat_other)

#Answer Question 9
sum(bat_other$mean_singles_other > 0.2)
sum(bat_other$mean_bb_other > 0.2)

#Join the data into one tibble
joined <- inner_join(bat_02, bat_other, by = "playerID")
names(joined)

#Answer Question 10
cor(joined$mean_singles_2002,joined$mean_singles_other)
cor(joined$mean_bb_2002,joined$mean_bb_other)

#Answer Question 11
ggplot(data = joined, aes(x = mean_singles_2002, y = mean_singles_other)) + geom_point()
ggplot(data = joined, aes(x = mean_bb_2002, y = mean_bb_other)) + geom_point()

#Answer Question 12
model_singles <- lm(joined$mean_singles_2002 ~ joined$mean_singles_other)
summary(model_singles)
model_bb <- lm(joined$mean_bb_2002 ~ joined$mean_bb_other)
summary(model_bb)
