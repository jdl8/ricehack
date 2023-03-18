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


#Nading Defenders Analysis

Nading_Defenders <- as.data.frame(sort(table(Nading_Possessions$player_defMatchup_a), decreasing = TRUE))
Nading_Defenders_qsq <- aggregate(qSQ ~ player_defMatchup_a, data = Nading_Possessions, FUN = mean, na.rm = TRUE)

Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_qsq, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)

Nading_Defenders_tov <- aggregate(tov ~ player_defMatchup_a, data = Nading_Possessions, FUN = sum, na.rm = TRUE)
Nading_Defenders_tov_rate <- aggregate(tov ~ player_defMatchup_a, data = Nading_Possessions, FUN = mean, na.rm = TRUE)

Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_tov, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)
Nading_Defenders <- merge(Nading_Defenders, Nading_Defenders_tov_rate, by.x = "Var1", by.y = "player_defMatchup_a", all.x = TRUE)

names(Nading_Defenders) <- c("Defender Name", "Frequency", "qSQ", "Turnovers Against", "Turnover Rate Against")


all_defenders_iso <- aggregate(iso_pts ~ player_defMatchup_a, data = iso_plays, FUN = mean, na.rm = TRUE)
all_defenders_iso_freq <- aggregate(poss ~ player_defMatchup_a, data = iso_plays, FUN = sum, na.rm = TRUE)
all_defenders_iso <- merge(all_defenders_iso, all_defenders_iso_freq, by = "player_defMatchup_a")
all_defenders_iso_assist <- aggregate(assistOppCreated ~ player_defMatchup_a, data = iso_plays, FUN = mean, na.rm = TRUE)
all_defenders_iso <- merge(all_defenders_iso, all_defenders_iso_assist, by = "player_defMatchup_a")
all_defenders_iso$points_allowed_off_assist <- all_defenders_iso$assistOppCreated*2

all_defenders_pick <- aggregate(pick_pts ~ player_defMatchup_a, data = pick_plays, FUN = mean, na.rm = TRUE)
all_defenders_pick_freq <- aggregate(poss ~ player_defMatchup_a, data = pick_plays, FUN = sum, na.rm = TRUE)
all_defenders_pick <- merge(all_defenders_pick, all_defenders_pick_freq, by = "player_defMatchup_a")
all_defenders_pick_assist <- aggregate(assistOppCreated ~ player_defMatchup_a, data = pick_plays, FUN = mean, na.rm = TRUE)
all_defenders_pick <- merge(all_defenders_pick, all_defenders_pick_assist, by = "player_defMatchup_a")
all_defenders_pick$points_allowed_off_assist <- all_defenders_pick$assistOppCreated*2

all_defenders_post <- aggregate(post_pts ~ player_defMatchup_a, data = post_plays, FUN = mean, na.rm = TRUE)
all_defenders_post_freq <- aggregate(poss ~ player_defMatchup_a, data = post_plays, FUN = sum, na.rm = TRUE)
all_defenders_post <- merge(all_defenders_post, all_defenders_post_freq, by = "player_defMatchup_a")
all_defenders_post_assist <- aggregate(assistOppCreated ~ player_defMatchup_a, data = post_plays, FUN = mean, na.rm = TRUE)
all_defenders_post <- merge(all_defenders_post, all_defenders_post_assist, by = "player_defMatchup_a")
all_defenders_post$points_allowed_off_assist <- all_defenders_post$assistOppCreated*2

all_defenders_offb <- aggregate(offBall_pts ~ player_defMatchup_a, data = offb_plays, FUN = mean, na.rm = TRUE)
all_defenders_offb_freq <- aggregate(poss ~ player_defMatchup_a, data = offb_plays, FUN = sum, na.rm = TRUE)
all_defenders_offb <- merge(all_defenders_offb, all_defenders_offb_freq, by = "player_defMatchup_a")
all_defenders_offb_assist <- aggregate(assistOppCreated ~ player_defMatchup_a, data = offb_plays, FUN = mean, na.rm = TRUE)
all_defenders_offb <- merge(all_defenders_offb, all_defenders_offb_assist, by = "player_defMatchup_a")


#Expected Points for Nading

all_defenders_iso$expected_points1 <- all_defenders_iso$iso_pts * sum(Nading_Possessions$iso_actions, na.rm = TRUE)
all_defenders_pick$expected_points2 <- all_defenders_pick$pick_pts * sum(Nading_Possessions$pick_actions, na.rm = TRUE)
all_defenders_post$expected_points3 <- all_defenders_post$post_pts * sum(Nading_Possessions$post_actions, na.rm = TRUE)
all_defenders_offb$expected_points4 <- all_defenders_offb$offBall_pts * sum(Nading_Possessions$offBall_actions, na.rm = TRUE)


all_defenders_all_types <- cbind(all_defenders_iso, all_defenders_pick, all_defenders_post, all_defenders_offb)
all_defenders_all_types$total_expected_points <- all_defenders_all_types$expected_points1 + all_defenders_all_types$expected_points2 + all_defenders_all_types$expected_points3 + all_defenders_all_types$expected_points4
all_defenders_all_types <- all_defenders_all_types[,-c(5,9,13)]
colnames(all_defenders_all_types) <- c("Defender Name", "Iso Play Average Points Allowed", "Iso Defended Frequency", "Expected Points for Nading on Iso Plays", "Pick Play Average Points Allowed", "Pick Defended Frequency", "Expected Points for Nading on Pick Plays", "Post Play Average Points Allowed", "Post Defended Frequency", "Expected Points for Nading on Post Play", "Off Ball Play Average Points Allowed", "Off Ball Defended Frequency", "Expected Points for Nading on Off Ball Play", "Total Expected Points for Nading")
all_defenders_total_expected_points <- all_defenders_all_types[,c(1,14)]
all_defenders_total_expected_points <- all_defenders_total_expected_points[order(all_defenders_total_expected_points$`Total Expected Points for Nading`),]

# for (row in all_defenders_total_expected_points){
#   cat(paste0(all_defenders_total_expected_points$`Defender Name`, ": ", all_defenders_total_expected_points$`Total Expected Points for Nading`, "\n"))
# }


Nading_Total_Points_Plays <- sum(Nading_Possessions$iso_pts, na.rm=TRUE) + sum(Nading_Possessions$pick_pts, na.rm = TRUE) + sum(Nading_Possessions$post_pts, na.rm = TRUE) + sum(Nading_Possessions$offBall_pts, na.rm = TRUE)

