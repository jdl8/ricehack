library(dplyr)
all_possessions <- read.csv("/Users/jonahlubin/Downloads/hackathon_data_basketball_2023.csv")

#Cleaning of Data
all_possessions$tov[all_possessions$tov == "null"] <- 0
all_possessions$tov <- as.numeric(all_possessions$tov)
all_possessions$qSQ[all_possessions$qSQ == "null"] <- NA
all_possessions$qSQ <- as.numeric(all_possessions$qSQ)
all_possessions[all_possessions == "null"] <- NA
all_possessions$fga <- as.numeric(all_possessions$fga)
all_possessions$fg <- as.numeric(all_possessions$fg)
all_possessions$fg3 <- as.numeric(all_possessions$fga3)
all_possessions$fg3 <- as.numeric(all_possessions$fg3)
all_possessions$iso_actions <- as.numeric(all_possessions$iso_actions)
all_possessions$iso_pts <- as.numeric(all_possessions$iso_pts)
all_possessions$pick_actions <- as.numeric(all_possessions$pick_actions)
all_possessions$pick_pts <- as.numeric(all_possessions$pick_pts)
all_possessions$post_actions <- as.numeric(all_possessions$post_actions)
all_possessions$post_pts <- as.numeric(all_possessions$post_pts)
all_possessions$passes <- as.numeric(all_possessions$passes)
all_possessions$assistOppCreated <- as.numeric(all_possessions$assistOppCreated)
all_possessions$touches <- as.numeric(all_possessions$touches)
all_possessions$touches_direct <- as.numeric(all_possessions$touches_direct)
all_possessions$touches_catchAndShoot <- as.numeric(all_possessions$touches_catchAndShoot)
all_possessions$touches_paint <- as.numeric(all_possessions$touches_paint)
all_possessions$touches_time <- as.numeric(all_possessions$touches_time)
all_possessions$fta <- as.numeric(all_possessions$fta)
all_possessions$ft_player <- as.numeric(all_possessions$ft_player)
all_possessions$fouls <- as.numeric(all_possessions$fouls)


#Iso Play Analysis for All Players
iso_plays <- subset(all_possessions, all_possessions$iso_actions>0)
avg_pts_iso_plays <- mean(iso_plays$iso_pts)

avg_tos_iso_plays <- mean(iso_plays$tov)


#Pick Play Analysis for All Players
pick_plays <- subset(all_possessions, all_possessions$pick_actions>0)
avg_pts_pick_plays <- mean(pick_plays$pick_pts)

avg_tos_pick_plays <- mean(pick_plays$tov)


#Post Play Analysis for All Players
post_plays <- subset(all_possessions, all_possessions$post_actions>0)
avg_pts_post_plays <- mean(post_plays$post_pts)

avg_tos_post_plays <- mean(post_plays$tov)


#Post Play Analysis for All Players
offb_plays <- subset(all_possessions, all_possessions$offBall_actions>0)
avg_pts_offb_plays <- mean(offb_plays$offBall_pts)

avg_tos_offb_plays <- mean(offb_plays$tov)


#Data Frame Avg Points Per Play Type for All Players
avg_pts_per_play_type <-data.frame(
  play_type = c("Iso Plays", "Pick Plays", "Post Plays", "Off Ball Plays"),
  avg_pts = c(avg_pts_iso_plays, avg_pts_pick_plays, avg_pts_post_plays, avg_pts_offb_plays)
)
avg_pts_per_play_type <- avg_pts_per_play_type[order(-avg_pts_per_play_type$avg_pts),]



#Data Frame Avg TOs Per Play Type for All Players
avg_tos_per_play_type <-data.frame(
  play_type = c("Iso Plays", "Pick Plays", "Post Plays", "Off Ball Plays"),
  avg_tos = c(avg_tos_iso_plays, avg_tos_pick_plays, avg_tos_post_plays, avg_tos_offb_plays)
)
avg_tos_per_play_type <- avg_tos_per_play_type[order(-avg_tos_per_play_type$avg_tos),]

All_QSQ_Per_Play <- data.frame(
  play_type = c("Iso qSQ", "Pick qSQ", "Post qSQ", "Off Ball qSQ"),
  qSQs = c(mean(iso_plays$qSQ, na.rm = TRUE), mean(pick_plays$qSQ, na.rm = TRUE), mean(post_plays$qSQ, na.rm = TRUE), mean(offb_plays$qSQ, na.rm = TRUE))
)




#Nading Offensive Tendencies

Nading_Possessions <- subset(all_possessions, all_possessions$player_off_a == "Nading, Samuel")
Nading_Iso <- subset(Nading_Possessions, Nading_Possessions$iso_actions>0)
Nading_Pick <- subset(Nading_Possessions, Nading_Possessions$pick_actions>0)
Nading_Post <- subset(Nading_Possessions, Nading_Possessions$post_actions>0)
Nading_offBall <- subset(Nading_Possessions, Nading_Possessions$offBall_actions>0)
Nading_QSQ_Per_Play <- data.frame(
  play_type = c("Iso qSQ", "Pick qSQ", "Post qSQ", "Off Ball qSQ"),
  qSQs = c(mean(Nading_Iso$qSQ, na.rm = TRUE), mean(Nading_Pick$qSQ, na.rm = TRUE), mean(Nading_Post$qSQ, na.rm = TRUE), mean(Nading_offBall$qSQ, na.rm = TRUE))
)


Nading_Over_All_QSQ_Per_Play<- data.frame(
  play_type = c("Iso qSQ", "Pick qSQ", "Post qSQ", "Off Ball qSQ"),
  qSQs = c(mean(Nading_Iso$qSQ, na.rm = TRUE) - mean(iso_plays$qSQ, na.rm = TRUE), 
                                                 mean(Nading_Pick$qSQ, na.rm = TRUE) - mean(pick_plays$qSQ, na.rm = TRUE), 
                                                 mean(Nading_Post$qSQ, na.rm = TRUE) - mean(post_plays$qSQ, na.rm = TRUE),
                                                 mean(Nading_offBall$qSQ, na.rm = TRUE) - mean(offb_plays$qSQ, na.rm = TRUE))
)


Nading_FT_subset <- subset(Nading_Possessions, Nading_Possessions$fta >0)
Nading_FT_Percent <- sum(Nading_FT_subset$ft_player) / sum(Nading_FT_subset$fta)

All_FT_subset <- subset(all_possessions, all_possessions$fta >0)
All_FT_Percent <- sum(All_FT_subset$ft_player) / sum(All_FT_subset$fta)

Nading_Points_subset <- subset(Nading_Possessions, Nading_Possessions$fga>0 | Nading_Possessions$fta>0)
Nading_Points <- sum(Nading_Points_subset$ptsScored_team)


#Assist subset analysis

assists_subset <- subset(all_possessions, all_possessions$assistOppCreated>0)
assist_iso <- subset(assists_subset, assists_subset$iso_actions>0)
assist_pick <- subset(assists_subset, assists_subset$pick_actions>0)
assist_post <- subset(assists_subset, assists_subset$post_actions>0)
assist_offb <- subset(assists_subset, assists_subset$offBall_actions>0)

Nading_assists_subset <- subset(Nading_Possessions, Nading_Possessions$assistOppCreated>0)
Nading_assist_iso <- subset(Nading_assists_subset, Nading_assists_subset$iso_actions>0)
Nading_assist_pick <- subset(Nading_assists_subset, Nading_assists_subset$pick_actions>0)
Nading_assist_post <- subset(Nading_assists_subset, Nading_assists_subset$post_actions>0)
Nading_assist_offb <- subset(Nading_assists_subset, Nading_assists_subset$offBall_actions>0)


#Nading Defenders Analysis

Nading_Defenders <- as.data.frame(sort(table(Nading_Possessions$player_defMatchup_a), decreasing = TRUE))
Nading_Defenders_qsq <- aggregate(qSQ ~ player_defMatchup_a, data = Nading_Possessions, FUN = mean, na.rm = TRUE)

Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_qsq, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)

Nading_Defenders_tov <- aggregate(tov ~ player_defMatchup_a, data = Nading_Possessions, FUN = sum, na.rm = TRUE)
Nading_Defenders_tov_rate <- aggregate(tov ~ player_defMatchup_a, data = Nading_Possessions, FUN = mean, na.rm = TRUE)

Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_tov, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_tov_rate, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)

Nading_Defenders_iso_pts <- aggregate(iso_pts ~ player_defMatchup_a, data = Nading_Possessions, FUN = sum, na.rm = TRUE)
Nading_Defenders_iso_freq <- aggregate(iso_actions ~ player_defMatchup_a, data = Nading_Possessions, FUN = sum, na.rm = TRUE)
Nading_Defenders_iso_assist <- aggregate(assistOppCreated ~ player_defMatchup_a, data = Nading_Iso, FUN = mean, na.rm = TRUE)
Nading_Defenders_pick_pts <- aggregate(pick_pts ~ player_defMatchup_a, data = Nading_Possessions, FUN = sum, na.rm = TRUE)
Nading_Defenders_pick_freq <- aggregate(pick_actions ~ player_defMatchup_a, data = Nading_Possessions, FUN = sum, na.rm = TRUE)
Nading_Defenders_pick_assist <- aggregate(assistOppCreated ~ player_defMatchup_a, data = Nading_Pick, FUN = mean, na.rm = TRUE)
Nading_Defenders_post_pts <- aggregate(post_pts ~ player_defMatchup_a, data = Nading_Possessions, FUN = sum, na.rm = TRUE)
Nading_Defenders_post_freq <- aggregate(post_actions ~ player_defMatchup_a, data = Nading_Possessions, FUN = sum, na.rm = TRUE)
Nading_Defenders_post_assist <- aggregate(assistOppCreated ~ player_defMatchup_a, data = Nading_Post, FUN = mean, na.rm = TRUE)
Nading_Defenders_offb_pts <- aggregate(offBall_pts ~ player_defMatchup_a, data = Nading_Possessions, FUN = sum, na.rm = TRUE)
Nading_Defenders_offb_freq <- aggregate(offBall_actions ~ player_defMatchup_a, data = Nading_Possessions, FUN = sum, na.rm = TRUE)
Nading_Defenders_offb_assist <- aggregate(assistOppCreated ~ player_defMatchup_a, data = Nading_offBall, FUN = mean, na.rm = TRUE)


Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_iso_pts, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_iso_freq, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_iso_assist, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_pick_pts, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_pick_freq, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_pick_assist, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_post_pts, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_post_freq, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_post_assist, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_offb_pts, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_offb_freq, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_offb_assist, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
names(Nading_Defenders) <- c("Defender Name", "Frequency", "qSQ", "Turnovers Against", "Turnover Rate Against", "Iso Pts", "Iso Freq", "Iso Assist", "Pick Pts", "Pick Freq", "Pick Assist", "Post Pts", "Post Freq", "Post Assist", "Off Ball Pts", "Off Ball Freq", "Off Ball Assist")


Nading_Defenders$avg_assist_points_allowed_iso <- Nading_Defenders$`Iso Assist` * mean(Nading_assist_iso$ptsScored_team)
Nading_Defenders$avg_assist_points_allowed_pick <- Nading_Defenders$`Pick Assist` * mean(Nading_assist_pick$ptsScored_team)
Nading_Defenders$avg_assist_points_allowed_post <- Nading_Defenders$`Post Assist` * mean(Nading_assist_post$ptsScored_team)
Nading_Defenders$avg_assist_points_allowed_offb <- Nading_Defenders$`Off Ball Assist` * mean(Nading_assist_offb$ptsScored_team)
Nading_Defenders$expected_points_off_iso_assists <- Nading_Defenders$avg_assist_points_allowed_iso * Nading_Defenders$`Iso Freq`
Nading_Defenders$expected_points_off_pick_assists <- Nading_Defenders$avg_assist_points_allowed_pick * Nading_Defenders$`Pick Freq`
Nading_Defenders$expected_points_off_post_assists <- Nading_Defenders$avg_assist_points_allowed_post * Nading_Defenders$`Post Freq`
Nading_Defenders$expected_points_off_offb_assists <- Nading_Defenders$avg_assist_points_allowed_offb * Nading_Defenders$`Off Ball Freq`


Nading_Defenders[is.na(Nading_Defenders)] <- 0
Nading_Defenders$TotalPoints <- Nading_Defenders$`Iso Pts` + Nading_Defenders$`Pick Pts` + Nading_Defenders$`Post Pts` + Nading_Defenders$`Off Ball Pts`
Nading_Defenders$TotalFrequency <- Nading_Defenders$`Iso Freq` + Nading_Defenders$`Pick Freq` + Nading_Defenders$`Post Freq` + Nading_Defenders$`Off Ball Freq`
Nading_Defenders$Avg_Points <- Nading_Defenders$TotalPoints / Nading_Defenders$TotalFrequency
Nading_Defenders$TotalAssistPoints <- Nading_Defenders$expected_points_off_iso_assists + Nading_Defenders$expected_points_off_pick_assists + Nading_Defenders$expected_points_off_post_assists + Nading_Defenders$expected_points_off_offb_assists
Nading_Defenders$Avg_Assist_Points <- Nading_Defenders$TotalAssistPoints / Nading_Defenders$TotalFrequency
Nading_Defenders$Total_Combined_Points_Average <- Nading_Defenders$Avg_Points + Nading_Defenders$Avg_Assist_Points


Nading_Defenders <- subset(Nading_Defenders, Nading_Defenders$Frequency>=10)
Nading_Defenders_Main_Factors <- Nading_Defenders[,c(1,2,3,4,5,28,30,31)]

names(Nading_Defenders_Main_Factors) <- c("Defender Name", "Frequency", "qSQ", "Turnovers Against", "Turnover Rate Against", "Average Points Allowed", "Average Points Allowed Through Assists", "Total Average Points Allowed Combined")
Nading_Defenders_Main_Factors <- Nading_Defenders_Main_Factors[order(Nading_Defenders_Main_Factors$`Total Average Points Allowed Combined`),]


#Analyzing All Defenders Statistics

all_defenders_iso <- aggregate(iso_pts ~ player_defMatchup_a, data = iso_plays, FUN = mean, na.rm = TRUE)
all_defenders_iso_freq <- aggregate(poss ~ player_defMatchup_a, data = iso_plays, FUN = sum, na.rm = TRUE)
all_defenders_iso <- merge(all_defenders_iso, all_defenders_iso_freq, by = "player_defMatchup_a")
all_defenders_iso_assist <- aggregate(assistOppCreated ~ player_defMatchup_a, data = iso_plays, FUN = mean, na.rm = TRUE)
all_defenders_iso <- merge(all_defenders_iso, all_defenders_iso_assist, by = "player_defMatchup_a")
all_defenders_iso$avg_assist_points_allowed <- all_defenders_iso$assistOppCreated * mean(assist_iso$ptsScored_team)


all_defenders_pick <- aggregate(pick_pts ~ player_defMatchup_a, data = pick_plays, FUN = mean, na.rm = TRUE)
all_defenders_pick_freq <- aggregate(poss ~ player_defMatchup_a, data = pick_plays, FUN = sum, na.rm = TRUE)
all_defenders_pick <- merge(all_defenders_pick, all_defenders_pick_freq, by = "player_defMatchup_a")
all_defenders_pick_assist <- aggregate(assistOppCreated ~ player_defMatchup_a, data = pick_plays, FUN = mean, na.rm = TRUE)
all_defenders_pick <- merge(all_defenders_pick, all_defenders_pick_assist, by = "player_defMatchup_a")
all_defenders_pick$avg_assist_points_allowed <- all_defenders_pick$assistOppCreated * mean(assist_pick$ptsScored_team)


all_defenders_post <- aggregate(post_pts ~ player_defMatchup_a, data = post_plays, FUN = mean, na.rm = TRUE)
all_defenders_post_freq <- aggregate(poss ~ player_defMatchup_a, data = post_plays, FUN = sum, na.rm = TRUE)
all_defenders_post <- merge(all_defenders_post, all_defenders_post_freq, by = "player_defMatchup_a")
all_defenders_post_assist <- aggregate(assistOppCreated ~ player_defMatchup_a, data = post_plays, FUN = mean, na.rm = TRUE)
all_defenders_post <- merge(all_defenders_post, all_defenders_post_assist, by = "player_defMatchup_a")
all_defenders_post$avg_assist_points_allowed <- all_defenders_post$assistOppCreated * mean(assist_post$ptsScored_team)


all_defenders_offb <- aggregate(offBall_pts ~ player_defMatchup_a, data = offb_plays, FUN = mean, na.rm = TRUE)
all_defenders_offb_freq <- aggregate(poss ~ player_defMatchup_a, data = offb_plays, FUN = sum, na.rm = TRUE)
all_defenders_offb <- merge(all_defenders_offb, all_defenders_offb_freq, by = "player_defMatchup_a")
all_defenders_offb_assist <- aggregate(assistOppCreated ~ player_defMatchup_a, data = offb_plays, FUN = mean, na.rm = TRUE)
all_defenders_offb <- merge(all_defenders_offb, all_defenders_offb_assist, by = "player_defMatchup_a")
all_defenders_offb$avg_assist_points_allowed <- all_defenders_offb$assistOppCreated * mean(assist_offb$ptsScored_team)


#Expected Points for Nading

all_defenders_iso$expected_points_iso <- all_defenders_iso$iso_pts * nrow(Nading_Iso)
all_defenders_pick$expected_points_pick <- all_defenders_pick$pick_pts * nrow(Nading_Pick)
all_defenders_post$expected_points_post <- all_defenders_post$post_pts * nrow(Nading_Post)
all_defenders_offb$expected_points_offb <- all_defenders_offb$offBall_pts * nrow(Nading_offBall)
all_defenders_iso$expected_points_off_iso_assists <- all_defenders_iso$avg_assist_points_allowed * nrow(Nading_Iso)
all_defenders_pick$expected_points_off_pick_assists <- all_defenders_pick$avg_assist_points_allowed * nrow(Nading_Pick)
all_defenders_post$expected_points_off_post_assists <- all_defenders_post$avg_assist_points_allowed * nrow(Nading_Post)
all_defenders_offb$expected_points_off_offb_assists <- all_defenders_offb$avg_assist_points_allowed * nrow(Nading_offBall)

all_defenders_all_types <- cbind(all_defenders_iso, all_defenders_pick, all_defenders_post, all_defenders_offb)
all_defenders_all_types$total_expected_points <- all_defenders_all_types$expected_points_iso + all_defenders_all_types$expected_points_pick + all_defenders_all_types$expected_points_post + all_defenders_all_types$expected_points_offb
all_defenders_all_types$total_expected_points_through_assists <- all_defenders_all_types$expected_points_off_iso_assists + all_defenders_all_types$expected_points_off_pick_assists + all_defenders_all_types$expected_points_off_post_assists + all_defenders_all_types$expected_points_off_offb_assists
all_defenders_all_types$total_expected_points_score_and_assist <- all_defenders_all_types$total_expected_points_through_assists + all_defenders_all_types$total_expected_points


all_defenders_all_types <- all_defenders_all_types[,-c(4,5,8,11,12,15,18,19,22,25,26)]
colnames(all_defenders_all_types) <- c("Defender Name", "Iso Play Average Points Allowed", "Iso Defended Frequency", "Expected Points for Nading on Iso Plays", "Expected Points through Assists Oppurtunities Created on Iso Plays", "Pick Play Average Points Allowed", "Pick Defended Frequency", "Expected Points for Nading on Pick Plays", "Expected Points through Assists Oppurtunities Created on Pick Plays", "Post Play Average Points Allowed", "Post Defended Frequency", "Expected Points for Nading on Post Play", "Expected Points through Assists Oppurtunities Created on Post Plays", "Off Ball Play Average Points Allowed", "Off Ball Defended Frequency", "Expected Points for Nading on Off Ball Play", "Expected Points through Assists Oppurtunities Created on Off Ball Plays", "Total Expected Points for Nading", "Total Expected Points for Nading Through Assists", "Total Expected Points Combining Score and Assists")
all_defenders_total_expected_points <- all_defenders_all_types[,c(1,18,19,20)]
all_defenders_total_expected_points <- all_defenders_total_expected_points[order(all_defenders_total_expected_points$`Total Expected Points Combining Score and Assists`),]


#Testing on the Players Using the Similarity Score
#Top 5 Players on Similarity Score: Joshua Sandoval, Jeremy Oscarson, Brandyn Hammond, Edward Thin-Elk, and Iaasic Kierstead


Similarity_Possessions <- subset(all_possessions, all_possessions$player_off_a == "Sandoval, Joshua" |
                                                all_possessions$player_off_a == "Oscarson, Jeremy" | 
                                                all_possessions$player_off_a == "Hammond, Brandyn" |
                                                all_possessions$player_off_a == "Thin-Elk, Edward" |
                                                all_possessions$player_off_a == "Kierstead, Iaasic")
Similarity_Iso <- subset(Similarity_Possessions, Similarity_Possessions$iso_actions>0)
Similarity_Pick <- subset(Similarity_Possessions, Similarity_Possessions$pick_actions>0)
Similarity_Post <- subset(Similarity_Possessions, Similarity_Possessions$post_actions>0)
Similarity_offBall <- subset(Similarity_Possessions, Similarity_Possessions$offBall_actions>0)


##Similar Defenders Defensive Analysis

Similarity_Defenders <- as.data.frame(sort(table(Similarity_Possessions$player_defMatchup_a), decreasing = TRUE))
Similarity_Defenders_qsq <- aggregate(qSQ ~ player_defMatchup_a, data = Similarity_Possessions, FUN = mean, na.rm = TRUE)

Similarity_Defenders <- merge(Similarity_Defenders, Similarity_Defenders_qsq, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)

Similarity_Defenders_tov <- aggregate(tov ~ player_defMatchup_a, data = Similarity_Possessions, FUN = sum, na.rm = TRUE)
Similarity_Defenders_tov_rate <- aggregate(tov ~ player_defMatchup_a, data = Similarity_Possessions, FUN = mean, na.rm = TRUE)

Similarity_Defenders <- merge(Similarity_Defenders, Similarity_Defenders_tov, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Similarity_Defenders <- merge(Similarity_Defenders, Similarity_Defenders_tov_rate, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)

Similarity_Defenders_iso_pts <- aggregate(iso_pts ~ player_defMatchup_a, data = Similarity_Possessions, FUN = sum, na.rm = TRUE)
Similarity_Defenders_iso_freq <- aggregate(iso_actions ~ player_defMatchup_a, data = Similarity_Possessions, FUN = sum, na.rm = TRUE)
Similarity_Defenders_iso_assist <- aggregate(assistOppCreated ~ player_defMatchup_a, data = Similarity_Iso, FUN = mean, na.rm = TRUE)
Similarity_Defenders_pick_pts <- aggregate(pick_pts ~ player_defMatchup_a, data = Similarity_Possessions, FUN = sum, na.rm = TRUE)
Similarity_Defenders_pick_freq <- aggregate(pick_actions ~ player_defMatchup_a, data = Similarity_Possessions, FUN = sum, na.rm = TRUE)
Similarity_Defenders_pick_assist <- aggregate(assistOppCreated ~ player_defMatchup_a, data = Similarity_Pick, FUN = mean, na.rm = TRUE)
Similarity_Defenders_post_pts <- aggregate(post_pts ~ player_defMatchup_a, data = Similarity_Possessions, FUN = sum, na.rm = TRUE)
Similarity_Defenders_post_freq <- aggregate(post_actions ~ player_defMatchup_a, data = Similarity_Possessions, FUN = sum, na.rm = TRUE)
Similarity_Defenders_post_assist <- aggregate(assistOppCreated ~ player_defMatchup_a, data = Similarity_Post, FUN = mean, na.rm = TRUE)
Similarity_Defenders_offb_pts <- aggregate(offBall_pts ~ player_defMatchup_a, data = Similarity_Possessions, FUN = sum, na.rm = TRUE)
Similarity_Defenders_offb_freq <- aggregate(offBall_actions ~ player_defMatchup_a, data = Similarity_Possessions, FUN = sum, na.rm = TRUE)
Similarity_Defenders_offb_assist <- aggregate(assistOppCreated ~ player_defMatchup_a, data = Similarity_offBall, FUN = mean, na.rm = TRUE)


Similarity_Defenders <- merge(Similarity_Defenders, Similarity_Defenders_iso_pts, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Similarity_Defenders <- merge(Similarity_Defenders, Similarity_Defenders_iso_freq, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Similarity_Defenders <- merge(Similarity_Defenders, Similarity_Defenders_iso_assist, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Similarity_Defenders <- merge(Similarity_Defenders, Similarity_Defenders_pick_pts, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Similarity_Defenders <- merge(Similarity_Defenders, Similarity_Defenders_pick_freq, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Similarity_Defenders <- merge(Similarity_Defenders, Similarity_Defenders_pick_assist, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Similarity_Defenders <- merge(Similarity_Defenders, Similarity_Defenders_post_pts, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Similarity_Defenders <- merge(Similarity_Defenders, Similarity_Defenders_post_freq, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Similarity_Defenders <- merge(Similarity_Defenders, Similarity_Defenders_post_assist, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Similarity_Defenders <- merge(Similarity_Defenders, Similarity_Defenders_offb_pts, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Similarity_Defenders <- merge(Similarity_Defenders, Similarity_Defenders_offb_freq, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Similarity_Defenders <- merge(Similarity_Defenders, Similarity_Defenders_offb_assist, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
names(Similarity_Defenders) <- c("Defender Name", "Frequency", "qSQ", "Turnovers Against", "Turnover Rate Against", "Iso Pts", "Iso Freq", "Iso Assist", "Pick Pts", "Pick Freq", "Pick Assist", "Post Pts", "Post Freq", "Post Assist", "Off Ball Pts", "Off Ball Freq", "Off Ball Assist")


Similarity_Defenders$avg_assist_points_allowed_iso <- Similarity_Defenders$`Iso Assist` * mean(assist_iso$ptsScored_team)
Similarity_Defenders$avg_assist_points_allowed_pick <- Similarity_Defenders$`Pick Assist` * mean(assist_pick$ptsScored_team)
Similarity_Defenders$avg_assist_points_allowed_post <- Similarity_Defenders$`Post Assist` * mean(assist_post$ptsScored_team)
Similarity_Defenders$avg_assist_points_allowed_offb <- Similarity_Defenders$`Off Ball Assist` * mean(assist_offb$ptsScored_team)
Similarity_Defenders$expected_points_off_iso_assists <- Similarity_Defenders$avg_assist_points_allowed_iso * Similarity_Defenders$`Iso Freq`
Similarity_Defenders$expected_points_off_pick_assists <- Similarity_Defenders$avg_assist_points_allowed_pick * Similarity_Defenders$`Pick Freq`
Similarity_Defenders$expected_points_off_post_assists <- Similarity_Defenders$avg_assist_points_allowed_post * Similarity_Defenders$`Post Freq`
Similarity_Defenders$expected_points_off_offb_assists <- Similarity_Defenders$avg_assist_points_allowed_offb * Similarity_Defenders$`Off Ball Freq`


Similarity_Defenders[is.na(Similarity_Defenders)] <- 0
Similarity_Defenders$TotalPoints <- Similarity_Defenders$`Iso Pts` + Similarity_Defenders$`Pick Pts` + Similarity_Defenders$`Post Pts` + Similarity_Defenders$`Off Ball Pts`
Similarity_Defenders$TotalFrequency <- Similarity_Defenders$`Iso Freq` + Similarity_Defenders$`Pick Freq` + Similarity_Defenders$`Post Freq` + Similarity_Defenders$`Off Ball Freq`
Similarity_Defenders$Avg_Points <- Similarity_Defenders$TotalPoints / Similarity_Defenders$TotalFrequency
Similarity_Defenders$TotalAssistPoints <- Similarity_Defenders$expected_points_off_iso_assists + Similarity_Defenders$expected_points_off_pick_assists + Similarity_Defenders$expected_points_off_post_assists + Similarity_Defenders$expected_points_off_offb_assists
Similarity_Defenders$Avg_Assist_Points <- Similarity_Defenders$TotalAssistPoints / Similarity_Defenders$TotalFrequency
Similarity_Defenders$Total_Combined_Points_Average <- Similarity_Defenders$Avg_Points + Similarity_Defenders$Avg_Assist_Points


Similarity_Defenders <- subset(Similarity_Defenders, Similarity_Defenders$Frequency>=30)
Similarity_Defenders_Main_Factors <- Similarity_Defenders[,c(1,2,3,4,5,28,30,31)]

names(Similarity_Defenders_Main_Factors) <- c("Defender Name", "Frequency", "qSQ", "Turnovers Against", "Turnover Rate Against", "Average Points Allowed", "Average Points Allowed Through Assists", "Total Average Points Allowed Combined")
Similarity_Defenders_Main_Factors <- Similarity_Defenders_Main_Factors[order(Similarity_Defenders_Main_Factors$`Total Average Points Allowed Combined`),]


