data <- read.csv("~/Downloads/hackathon_data_basketball_2023.csv")

data["offBall_actions"][data["offBall_actions"] == 0] <- NA

library(tidyverse)
library(tidyr)

Nading <- data[data$player_off_a == 'Nading, Samuel',]

table(Nading$iso_actions)
#273 Isolation Actions
273/1596
table(Nading$pick_actions)
#944 Pick_actions
944/1596
table(Nading$post_actions)
#120 post_actions
120/1596
table(Nading$offBall_actions)
#259 off_ball actions
259/1596

273 + 944 +120+259
str(Nading)
mean(Nading$player_off_ht)
#Nading is 79 inches tall

table(Nading$iso_pts)
#Scores on 43% of Isolation
table(Nading$pick_pts)
table(Nading$post_pts)

#Number of Offensive and Defensive Players
length(unique(data$player_off_a))
length(unique(Nading$player_defMatchup_a))
#Check Data Types
str(data)
#Format for how to change to numeric
data$tov<- as.numeric(data$tov)

str(Nading)

mean(Nading$qSQ)
#Gets rid of the character columns 
#so we can use column means
new_Nading <- Nading[c(-1,-2)]

#Nading_Iso
Nading_Iso <- new_Nading %>% drop_na(iso_actions)
iso_avg_points <- mean(Nading_Iso$iso_pts)
iso_avg_qsq <- mean(Nading_Iso$qSQ, na.rm = TRUE)

#Nading_Pick
Nading_Pick <- new_Nading %>% drop_na(pick_actions)
pick_avg_points <- mean(Nading_Pick$pick_pts)
pick_avg_qsq <- mean(Nading_Pick$qSQ, na.rm = TRUE)

#Nading_post
Nading_Post <- new_Nading %>% drop_na(post_actions)
post_avg_points <- mean(Nading_Post$post_pts)
post_avg_qsq <- mean(Nading_Post$qSQ, na.rm = TRUE)

#Nading_Off_ball
Nading_Off <- new_Nading %>% drop_na(offBall_actions)
offBall_avg_points <- mean(Nading_Off$offBall_pts)
offBall_avg_qsq <- mean(Nading_Off$qSQ, na.rm = TRUE)

#all in one df
column <- c(iso_avg_points, iso_avg_qsq, pick_avg_points, pick_avg_qsq, post_avg_points
            , post_avg_qsq, offBall_avg_points, offBall_avg_qsq)
all <- data.frame(column)
rownames(all) <- c("iso_avg_points", "iso_avg_qsq", "pick_avg_points", "pick_avg_qsq", "post_avg_points"
                   , "post_avg_qsq", "offBall_avg_points", "offBall_avg_qsq")



full_Nading_Iso <- new_Nading %>% drop_na(iso_actions)

#all_iso
All_Iso <- data %>% drop_na(iso_actions)
all_iso_avg_points <- mean(All_Iso$iso_pts)
all_iso_avg_qsq <- mean(All_Iso$qSQ, na.rm = TRUE)

#All_pick
All_Pick <- data %>% drop_na(pick_actions)
all_pick_avg_points <- mean(All_Pick$pick_pts)
all_pick_avg_qsq <- mean(All_Pick$qSQ, na.rm = TRUE)

#all_post
All_Post <- data %>% drop_na(post_actions)
all_post_avg_points <- mean(All_Post$post_pts)
all_post_avg_qsq <- mean(All_Post$qSQ, na.rm = TRUE)

#all_Off_ball
All_Off <- data %>% drop_na(offBall_actions)
all_offBall_avg_points <- mean(All_Off$offBall_pts)
all_offBall_avg_qsq <- mean(All_Off$qSQ, na.rm = TRUE)

names(all)[1] = "Nading"
everyone_avg = c(all_iso_avg_points, all_iso_avg_qsq, all_pick_avg_points, all_pick_avg_qsq, all_post_avg_points
                 , all_post_avg_qsq, all_offBall_avg_points, all_offBall_avg_qsq)

all <- cbind(all, everyone_avg)
all$difference <- all$Nading - all$everyone_avg


length(unique(Nading$player_defMatchup_a))
#Nading Defenders
Nading_Defenders <- as.data.frame(sort(table(Nading$player_defMatchup_a), decreasing = TRUE))

Nading$player_defMatchup_a <- factor(Nading$player_defMatchup_a, levels = unique(Nading$player_defMatchup_a))
Nading_Def_Iso <- aggregate(iso_pts ~ player_defMatchup_a, data = Nading, FUN = mean, na.rm = TRUE, drop = FALSE)
Nading_Def_Iso <- Nading_Def_Iso[order(Nading_Def_Iso$iso_pts),]

Nading_Def_Pick <- aggregate(pick_pts ~ player_defMatchup_a, data = Nading, FUN = mean, na.rm = TRUE, drop = FALSE)
Nading_Def_Pick <- Nading_Def_Pick[order(Nading_Def_Pick$pick_pts),]

Nading_Def_Post <- aggregate(post_pts ~ player_defMatchup_a, data = Nading, FUN = mean, na.rm = TRUE, drop = FALSE)

Nading_Def_Off <- aggregate(offBall_pts ~ player_defMatchup_a, data = Nading, FUN = mean, na.rm = TRUE, drop = FALSE)

Nading_Def_qSQ <- aggregate(qSQ ~ player_defMatchup_a, data = Nading, FUN = mean, na.rm = TRUE, drop = FALSE)
#Dataframe of all avg stats for defenders against Nading
Total_Nading_Def_Stats <- merge(Total_Nading_Def_Stats, Nading_Def_qSQ, by = 'player_defMatchup_a')

Total_Nading_Def_Stats$Rank <- (Total_Nading_Def_Stats$iso_pts * .171) + (Total_Nading_Def_Stats$pick_pts * .591) + (Total_Nading_Def_Stats$post_pts * .075) + (Total_Nading_Def_Stats$offBall_pts * .162)

Total_Nading_Def_Stats$na_Rank <- ((ifelse(is.na(Total_Nading_Def_Stats$iso_pts), 0, Total_Nading_Def_Stats$iso_pts) * .171) + 
                                  (ifelse(is.na(Total_Nading_Def_Stats$pick_pts), 0, Total_Nading_Def_Stats$pick_pts) * .591) + 
                                  (ifelse(is.na(Total_Nading_Def_Stats$post_pts), 0, Total_Nading_Def_Stats$post_pts) * .075) + 
                                  (ifelse(is.na(Total_Nading_Def_Stats$offBall_pts), 0, Total_Nading_Def_Stats$offBall_pts) * .162)) 

Total_Nading_Def_Stats <- Total_Nading_Def_Stats[order(Total_Nading_Def_Stats$Rank),]



#All defenders ranking based on Nading Tendencies   
data$player_defMatchup_a <- factor(data$player_defMatchup_a, levels = unique(data$player_defMatchup_a))
data_Def_Iso <- aggregate(iso_pts ~ player_defMatchup_a, data = data, FUN = mean, na.rm = TRUE, drop = FALSE)
data_Def_Iso <- data_Def_Iso[order(data_Def_Iso$iso_pts),]

data_Def_Pick <- aggregate(pick_pts ~ player_defMatchup_a, data = data, FUN = mean, na.rm = TRUE, drop = FALSE)
data_Def_Pick <- data_Def_Pick[order(data_Def_Pick$pick_pts),]

data_Def_Post <- aggregate(post_pts ~ player_defMatchup_a, data = data, FUN = mean, na.rm = TRUE, drop = FALSE)

data_Def_Off <- aggregate(offBall_pts ~ player_defMatchup_a, data = data, FUN = mean, na.rm = TRUE, drop = FALSE)

data_Def_qSQ <- aggregate(qSQ ~ player_defMatchup_a, data = data, FUN = mean, na.rm = TRUE, drop = FALSE)
#Dataframe of all avg stats for defenders against Nading
Total_Def_Stats <- merge(Total_Def_Stats, data_Def_Off,by = 'player_defMatchup_a')

Total_Def_Stats$Rank <- (Total_Def_Stats$iso_pts * .171) + (Total_Def_Stats$pick_pts * .591) + (Total_Def_Stats$post_pts * .075) + (Total_Def_Stats$offBall_pts * .162)

Total_Def_Stats$na_Rank <- ((ifelse(is.na(Total_Def_Stats$iso_pts), 0, Total_Def_Stats$iso_pts) * .171) + 
                                     (ifelse(is.na(Total_Def_Stats$pick_pts), 0, Total_Def_Stats$pick_pts) * .591) + 
                                     (ifelse(is.na(Total_Def_Stats$post_pts), 0, Total_Def_Stats$post_pts) * .075) + 
                                     (ifelse(is.na(Total_Def_Stats$offBall_pts), 0, Total_Def_Stats$offBall_pts) * .162)) 

Total_Def_Stats <- Total_Def_Stats[order(Total_Def_Stats$Rank),]
