# LFC competition

rm(list = ls(all=TRUE))
require("ggplot2")
require("gganimate")
require("StatsBombR")
require("SBpitch")
require("gifski")
require("png")
require("stringr")
require("data.table")

# lfc data
data <- read.csv("C:\\Users\\johan\\Documents\\lfc competition\\liverpool_2019.csv", header = T)

# sb data
comp <- FreeCompetitions()
matches <- FreeMatches(comp)
matches <- subset (matches, competition.competition_id %in% c(11,37,49)) #only league games (WSL, NWSL, La Liga)
data_sb <- StatsBombFreeEvents(MatchesDF = matches, Parallel = T)

# create zones for analyses

x <- c(0,30,60,90,105)
y <- c(0,18,30,50,62)
x_end <- c(30,60,90,105,120)
x_end <- as.data.frame(cbind(x,x_end))
y_end <- c(18,30,50,62,80)
y_end <- as.data.frame(cbind(y,y_end))
zones <- merge(x,y)

zones <- merge(zones, x_end, by = c("x"))
zones <- merge(zones, y_end, by = c("y"))
zones$centre_x <- zones$x_end - (zones$x_end - zones$x)/2
zones$centre_y <- zones$y_end - (zones$y_end - zones$y)/2

zones <- zones[
  with(zones, order(x, y)),
  ]

number <- 1:25

zones <- cbind(zones, number)

# create pitch with zones
pitch_with_zones <- 
  create_Pitch(grass_colour = "darkgreen", line_colour = "black",
             background_colour = "darkgreen") +
  geom_segment (aes(x = 0, xend = 120, y = 17.5, yend = 17.5), linetype = 2, col = "blue") +
  geom_segment (aes(x = 0, xend = 120, y = 29.5, yend = 29.5), linetype = 2, col = "blue") +
  geom_segment (aes(x = 0, xend = 120, y = 50.5, yend = 50.5), linetype = 2, col = "blue") +
  geom_segment (aes(x = 0, xend = 120, y = 62.5, yend = 62.5), linetype = 2, col = "blue") +
  geom_segment (aes(x = 30, xend = 30, y = 0, yend = 80), linetype = 2, col = "blue") +
  geom_segment (aes(x = 60, xend = 60, y = 0, yend = 80), linetype = 2, col = "blue") +
  geom_segment (aes(x = 90, xend = 90, y = 0, yend = 80), linetype = 2, col = "blue") +
  geom_segment (aes(x = 105, xend = 105, y = 0, yend = 80), linetype = 2, col = "blue") +
  annotate ("segment", x = 40, xend = 60, y = -5, yend = -5, size = 2, arrow = arrow()) +
  annotate ("text", x = 75, y = -4, label = "Attacking Direction", family = "serif") +
  annotate ("text", x = 60, y = 85, label = "Zones used for analyses", family = "serif", fontface = "bold") 
  
  
# filter passes and shots
pass <- subset (data_sb, type.id == 30)
shots <- subset (data_sb, type.id == 16)
rm(data_sb) 

# Pass model
pass <- pass[,c("location", "pass.length", "pass.angle", 
                    "pass.end_location", "pass.outcome.id", "pass.outcome.name")]
pass$successful <- ifelse(is.na(pass$pass.outcome.id), 1, 0)

# extract x, y location 
pass$x <- sub("\\,.*", "", pass$location)
pass$x <- substr(pass$x, 3, nchar(pass$x))
pass$x <- as.numeric(pass$x)

pass$y <- sub("^[^,]*", "", pass$location)
pass$y <- substr(pass$y, 3, nchar(pass$y)-1)
pass$y <- as.numeric(pass$y)

pass$x_end <- sub("\\,.*", "", pass$pass.end_location)
pass$x_end <- substr(pass$x_end, 3, nchar(pass$x_end))
pass$x_end <- as.numeric(pass$x_end)

pass$y_end <- sub("^[^,]*", "", pass$pass.end_location)
pass$y_end <- substr(pass$y_end, 3, nchar(pass$y_end)-1)
pass$y_end <- as.numeric(pass$y_end)

# start and end zones for each pass
pass$start_zone_x <- 0
pass$start_zone_x <- ifelse(0 < pass$x & pass$x <= 30, 1, pass$start_zone_x)
pass$start_zone_x <- ifelse(30 < pass$x & pass$x <= 60, 2, pass$start_zone_x)
pass$start_zone_x <- ifelse(60 < pass$x & pass$x <= 90, 3, pass$start_zone_x)
pass$start_zone_x <- ifelse(90 < pass$x & pass$x <= 105, 4, pass$start_zone_x)
pass$start_zone_x <- ifelse(105 < pass$x & pass$x <= 122, 5, pass$start_zone_x)


pass$start_zone_y <- 0
pass$start_zone_y <- ifelse(0 < pass$y & pass$y <= 18, 1, pass$start_zone_y)
pass$start_zone_y <- ifelse(18 < pass$y & pass$y <= 30, 2, pass$start_zone_y)
pass$start_zone_y <- ifelse(30 < pass$y & pass$y < 50, 3, pass$start_zone_y)
pass$start_zone_y <- ifelse(50 <= pass$y & pass$y <= 62, 4, pass$start_zone_y)
pass$start_zone_y <- ifelse(62 < pass$y & pass$y <= 82, 5, pass$start_zone_y)

pass$start_zone <- pass$start_zone_x * pass$start_zone_y 
pass$start_zone <- ifelse(pass$start_zone_x == 2, pass$start_zone_y + 5, pass$start_zone)
pass$start_zone <- ifelse(pass$start_zone_x == 3, pass$start_zone_y + 10, pass$start_zone)
pass$start_zone <- ifelse(pass$start_zone_x == 4, pass$start_zone_y + 15, pass$start_zone)
pass$start_zone <- ifelse(pass$start_zone_x == 5, pass$start_zone_y + 20, pass$start_zone)


pass$end_zone_x <- 0
pass$end_zone_x <- ifelse(0 < pass$x_end & pass$x_end <= 30, 1, pass$end_zone_x)
pass$end_zone_x <- ifelse(30 < pass$x_end & pass$x_end <= 60, 2, pass$end_zone_x)
pass$end_zone_x <- ifelse(60 < pass$x_end & pass$x_end <= 90, 3, pass$end_zone_x)
pass$end_zone_x <- ifelse(90 < pass$x_end & pass$x_end <= 105, 4, pass$end_zone_x)
pass$end_zone_x <- ifelse(105 < pass$x_end & pass$x_end <= 122, 5, pass$end_zone_x)


pass$end_zone_y <- 0
pass$end_zone_y <- ifelse(0 < pass$y_end & pass$y_end <= 18, 1, pass$end_zone_y)
pass$end_zone_y <- ifelse(18 < pass$y_end & pass$y_end <= 30, 2, pass$end_zone_y)
pass$end_zone_y <- ifelse(30 < pass$y_end & pass$y_end < 50, 3, pass$end_zone_y)
pass$end_zone_y <- ifelse(50 <= pass$y_end & pass$y_end <= 62, 4, pass$end_zone_y)
pass$end_zone_y <- ifelse(62 < pass$y_end & pass$y_end <= 82, 5, pass$end_zone_y)

pass$end_zone <- pass$end_zone_x * pass$end_zone_y 
pass$end_zone <- ifelse(pass$end_zone_x == 2, pass$end_zone_y + 5, pass$end_zone)
pass$end_zone <- ifelse(pass$end_zone_x == 3, pass$end_zone_y + 10, pass$end_zone)
pass$end_zone <- ifelse(pass$end_zone_x == 4, pass$end_zone_y + 15, pass$end_zone)
pass$end_zone <- ifelse(pass$end_zone_x == 5, pass$end_zone_y + 20, pass$end_zone)

# distribution of start and end zones
start<-as.data.frame(table(pass[,c("start_zone")]))
end<-as.data.frame(table(pass[,c("end_zone")]))

# first step towards model: how many passes are played from which zone to which zone
pass$n <- 1
zones_sum <- aggregate(list(passes_played = pass$n, passes_successful = pass$successful),
                       by = list (start_zone = pass$start_zone, end_zone = pass$end_zone),
                       FUN = sum)

# missing combinations (88)
start_zone <- 1:25
end_zone <- 1:25
missing <- merge(start_zone, end_zone)
colnames (missing) [1] <- "start_zone"
colnames (missing) [2] <- "end_zone"
missing$passes_played <- 0
missing$passes_successful <- 0

zones_sum <- rbind(zones_sum, missing)

zones_sum <- aggregate(list(passes_played = zones_sum$passes_played, passes_successful = zones_sum$passes_successful),
                       by = list (start_zone = zones_sum$start_zone, end_zone = zones_sum$end_zone),
                       FUN = sum)

zones_sum$perc <- zones_sum$passes_successful / zones_sum$passes_played

pass <- merge(pass, zones_sum, by = c("start_zone", "end_zone"))

# pass length model 
pass_model <- glm(formula = successful ~ pass.length, 
                  family = "binomial", data = pass)

pass$length_model_pred <- predict(pass_model, newdata = pass, type = "response")

# predict pass success based on combination of zones and pass length
pass$weight_zone <- ifelse(pass$passes_played / max(pass$passes_played) > 0.9, 0.9, pass$passes_played / max(pass$passes_played))
pass$weight_length <- 1 - pass$weight_zone

pass$pred <- pass$perc * pass$weight_zone + 
  pass$length_model_pred * pass$weight_length

# evaluating the approaches
zones_eval <- as.data.frame(table(true = pass$successful, pred = as.numeric(pass$perc > 0.5)))
model_eval <- as.data.frame(table(true = pass$successful, pred = round(fitted(pass_model))))
mixed_eval <- as.data.frame(table(true = pass$successful, pred = as.numeric(pass$pred > 0.5)))


# visualize pass model for pass from midpoint
x <- 60
y <- 40

pred <- data.frame(x,y)

pred$start_zone_x <- 0
pred$start_zone_x <- ifelse(0 < pred$x & pred$x <= 30, 1, pred$start_zone_x)
pred$start_zone_x <- ifelse(30 < pred$x & pred$x <= 60, 2, pred$start_zone_x)
pred$start_zone_x <- ifelse(60 < pred$x & pred$x <= 90, 3, pred$start_zone_x)
pred$start_zone_x <- ifelse(90 < pred$x & pred$x <= 105, 4, pred$start_zone_x)
pred$start_zone_x <- ifelse(105 < pred$x & pred$x <= 122, 5, pred$start_zone_x)


pred$start_zone_y <- 0
pred$start_zone_y <- ifelse(0 < pred$y & pred$y <= 18, 1, pred$start_zone_y)
pred$start_zone_y <- ifelse(18 < pred$y & pred$y <= 30, 2, pred$start_zone_y)
pred$start_zone_y <- ifelse(30 < pred$y & pred$y < 50, 3, pred$start_zone_y)
pred$start_zone_y <- ifelse(50 <= pred$y & pred$y <= 62, 4, pred$start_zone_y)
pred$start_zone_y <- ifelse(62 < pred$y & pred$y <= 82, 5, pred$start_zone_y)

pred$start_zone <- pred$start_zone_x * pred$start_zone_y 
pred$start_zone <- ifelse(pred$start_zone_x == 2, pred$start_zone_y + 5, pred$start_zone)
pred$start_zone <- ifelse(pred$start_zone_x == 3, pred$start_zone_y + 10, pred$start_zone)
pred$start_zone <- ifelse(pred$start_zone_x == 4, pred$start_zone_y + 15, pred$start_zone)
pred$start_zone <- ifelse(pred$start_zone_x == 5, pred$start_zone_y + 20, pred$start_zone)

pred <- pred[,c(1,2,5)]
pred <- merge(pred, zones_sum, by = c("start_zone"))

colnames(zones) [1] <- "y_start"
colnames(zones) [2] <- "x_start"

pred <- merge(pred, zones, 
              by.x=c("end_zone"), by.y=c("number"))

pred$pass.length <- sqrt((pred$x - pred$centre_x) ^ 2 + (pred$y - pred$centre_y) ^ 2)
pred$length_model_pred <- predict(pass_model, newdata = pred, type = "response")

# predict pass success based on combination of zones and pass length
pred$weight_zone <- ifelse(pred$passes_played / max(zones_sum$passes_played) > 0.9, 0.9, pred$passes_played / max(zones_sum$passes_played))
pred$weight_length <- 1 - pred$weight_zone

pred$pred <- pred$perc * pred$weight_zone + 
  pred$length_model_pred * pred$weight_length

# visualize predictions
pitch_pass_model <- 
  create_Pitch(grass_colour = "darkgreen", line_colour = "black",
               background_colour = "darkgreen") +
  geom_rect (aes(xmin = pred$x_start, ymin = pred$y_start, 
                 xmax = pred$x_end, ymax = pred$y_end, fill = pred$pred),
             alpha = .5) +
  theme (legend.position = "none") +
  annotate ("text", x = 60, y = 85, label = "Pass probability from the centre mark", family = "serif", fontface = "bold") 

# Shots 
shots <- shots[,c("location", "shot.statsbomb_xg", "shot.outcome.name", "shot.outcome.id")]

# extract x, y location 
shots$x <- sub("\\,.*", "", shots$location)
shots$x <- substr(shots$x, 3, nchar(shots$x))
shots$x <- as.numeric(shots$x)

shots$y <- sub("^[^,]*", "", shots$location)
shots$y <- substr(shots$y, 3, nchar(shots$y)-1)
shots$y <- as.numeric(shots$y)

# determine shot zone 

shots$shot_zone_x <- 0
shots$shot_zone_x <- ifelse(0 < shots$x & shots$x <= 30, 1, shots$shot_zone_x)
shots$shot_zone_x <- ifelse(30 < shots$x & shots$x <= 60, 2, shots$shot_zone_x)
shots$shot_zone_x <- ifelse(60 < shots$x & shots$x <= 90, 3, shots$shot_zone_x)
shots$shot_zone_x <- ifelse(90 < shots$x & shots$x <= 105, 4, shots$shot_zone_x)
shots$shot_zone_x <- ifelse(105 < shots$x & shots$x <= 122, 5, shots$shot_zone_x)


shots$shot_zone_y <- 0
shots$shot_zone_y <- ifelse(0 < shots$y & shots$y <= 18, 1, shots$shot_zone_y)
shots$shot_zone_y <- ifelse(18 < shots$y & shots$y <= 30, 2, shots$shot_zone_y)
shots$shot_zone_y <- ifelse(30 < shots$y & shots$y < 50, 3, shots$shot_zone_y)
shots$shot_zone_y <- ifelse(50 <= shots$y & shots$y <= 62, 4, shots$shot_zone_y)
shots$shot_zone_y <- ifelse(62 < shots$y & shots$y <= 82, 5, shots$shot_zone_y)

shots$shot_zone <- shots$shot_zone_x * shots$shot_zone_y 
shots$shot_zone <- ifelse(shots$shot_zone_x == 2, shots$shot_zone_y + 5, shots$shot_zone)
shots$shot_zone <- ifelse(shots$shot_zone_x == 3, shots$shot_zone_y + 10, shots$shot_zone)
shots$shot_zone <- ifelse(shots$shot_zone_x == 4, shots$shot_zone_y + 15, shots$shot_zone)
shots$shot_zone <- ifelse(shots$shot_zone_x == 5, shots$shot_zone_y + 20, shots$shot_zone)

# summarize data and evaluate danger of shots from zones
shots$shot <- 1
shots$goal <- ifelse(shots$shot.outcome.id == 97, 1, 0)

shot_sum <- aggregate(list(shots = shots$shot, goals = shots$goal, xg = shots$shot.statsbomb_xg),
                      by = list(shot_zone = shots$shot_zone), FUN = sum)

shot_sum$perc <- shot_sum$goals / shot_sum$shots
shot_sum$ave_xg <- shot_sum$xg / shot_sum$shots

shot_sum$weight_zone <- ifelse(shot_sum$shots / max(shot_sum$shots) > 0.9, 0.9, shot_sum$shots / max(shot_sum$shots))
shot_sum$weight_xg <- 1 - shot_sum$weight_zone

shot_sum$pred <- shot_sum$perc * shot_sum$weight_zone + 
  shot_sum$ave_xg * shot_sum$weight_xg

shot_sum <- subset(shot_sum, shot_zone >= 11)

shot_sum <- merge(shot_sum, zones[,c(1:4,7)], 
                  by.x = c("shot_zone"), by.y = c("number"))

# visualize basic xg-model

pitch_shot_model <- 
  create_Pitch(grass_colour = "darkgreen", line_colour = "black",
               background_colour = "darkgreen") +
  geom_rect (aes(xmin = shot_sum$x_start, ymin = shot_sum$y_start, 
                 xmax = shot_sum$x_end, ymax = shot_sum$y_end, fill = shot_sum$pred),
             alpha = .5) +
  theme (legend.position = "none") +
  annotate ("text", x = 60, y = 85, label = "Scoring probability", family = "serif", fontface = "bold") 

# calculate distance from goal for each zone
distance <- merge(start_zone, end_zone)
colnames (distance) [1] <- "start_zone"
colnames (distance) [2] <- "end_zone"

distance <- merge(distance, zones[,c(5:7)],
                  by.x = c("start_zone"), by.y = c("number"))
colnames (distance) [3] <- "start_zone_centre_x"
colnames (distance) [4] <- "start_zone_centre_y"

distance <- merge(distance, zones[,c(5:7)],
                  by.x = c("end_zone"), by.y = c("number"))
colnames (distance) [5] <- "end_zone_centre_x"
colnames (distance) [6] <- "end_zone_centre_y"

distance$start_zone_distance <- sqrt((distance$start_zone_centre_x - 120) ^ 2 + (distance$start_zone_centre_y - 40) ^ 2)
distance$end_zone_distance <- sqrt((distance$end_zone_centre_x - 120) ^ 2 + (distance$end_zone_centre_y - 40) ^ 2)
distance$difference <- distance$start_zone_distance - distance$end_zone_distance

# need to make coordinates of tracking data comparable with StatsBomb and SBPitch
pres <- data
pres$x <- pres$x / 100 * 120
pres$y <- pres$y / 100 * 80

# also change x,y coordinates to always attack from left to right
ball <- subset(pres, player == 0)
last_frame <- aggregate (list(last_frame = ball$frame), by = list(play = ball$play),
                         FUN = max)

ball <- merge (ball, last_frame, by = c("play"))
ball <- subset (ball, frame == last_frame)
ball$dir <- ifelse(ball$x > 60, 1, 0)

pres <- merge(pres, ball[,c(1,15)], by = c("play"))
pres$x <- ifelse(pres$dir == 0, (120 - pres$x), pres$x)
pres$y <- ifelse(pres$dir == 0, (80 - pres$y), pres$y)
pres <- pres[,c(1:13)]

# set colours of ball and size 
ball <- subset(pres, player == 0)
ball$size <- 0.5
ball$bgcolor <- "black"
ball$size <- 0.5

player <- subset(pres, player != 0)
player$size <- 1

pres <- rbind(player, ball)

games <- as.data.frame(unique(pres$play))

for (k in games$`unique(pres$play)`)
{
game <- subset (pres, play == k ) 
title <- unique(game$play)

ball <- subset(game, player == 0)

ball$zone_x <- 0
ball$zone_x <- ifelse(0 < ball$x & ball$x <= 30, 1, ball$zone_x)
ball$zone_x <- ifelse(30 < ball$x & ball$x <= 60, 2, ball$zone_x)
ball$zone_x <- ifelse(60 < ball$x & ball$x <= 90, 3, ball$zone_x)
ball$zone_x <- ifelse(90 < ball$x & ball$x <= 105, 4, ball$zone_x)
ball$zone_x <- ifelse(105 < ball$x & ball$x <= 122, 5, ball$zone_x)

ball$zone_y <- 0
ball$zone_y <- ifelse(0 < ball$y & ball$y <= 18, 1, ball$zone_y)
ball$zone_y <- ifelse(18 < ball$y & ball$y <= 30, 2, ball$zone_y)
ball$zone_y <- ifelse(30 < ball$y & ball$y < 50, 3, ball$zone_y)
ball$zone_y <- ifelse(50 <= ball$y & ball$y <= 62, 4, ball$zone_y)
ball$zone_y <- ifelse(62 < ball$y & ball$y <= 82, 5, ball$zone_y)

ball$zone <- ball$zone_x * ball$zone_y 
ball$zone <- ifelse(ball$zone_x == 2, ball$zone_y + 5, ball$zone)
ball$zone <- ifelse(ball$zone_x == 3, ball$zone_y + 10, ball$zone)
ball$zone <- ifelse(ball$zone_x == 4, ball$zone_y + 15, ball$zone)
ball$zone <- ifelse(ball$zone_x == 5, ball$zone_y + 20, ball$zone)

ball$distance <- sqrt((ball$x - 120) ^ 2 + (ball$y - 40) ^ 2)

# for each possible zone add pass probability, xg_value and distance to goal
for (i in 1:25) {
  ball$end_zone <- i
  
  # pass into zone
  
  ball <- merge (ball, zones_sum [,c(1,2,3,5)], 
                 by.x = c("zone", "end_zone"), by.y = c("start_zone", "end_zone"))
  ball <- merge (ball, zones [,c(5:7)],
                 by.x = c("end_zone"), by.y = c("number"))
  ball$pass.length <- sqrt((ball$x - ball$centre_x) ^ 2 + (ball$y - ball$centre_y) ^ 2)
  ball$length_model_pred <- predict(pass_model, newdata = ball, type = "response")
  ball$weight_zone <- ifelse(ball$passes_played / max(zones_sum$passes_played) > 0.9, 0.9, ball$passes_played / max(zones_sum$passes_played))
  ball$weight_length <- 1 - ball$weight_zone
  ball$pass_pred <- ball$perc * ball$weight_zone + 
    ball$length_model_pred * ball$weight_length 
  ball$pass_pred <- ifelse(is.na(ball$perc), ball$length_model_pred, ball$pass_pred)
  
  # xg for zones
  ball <- merge(ball, shot_sum[,c(1,9)], 
                by.x = c("zone"), by.y = c("shot_zone"), all.x = TRUE)
  ball$pred <- ifelse(is.na(ball$pred), 0, ball$pred)
  colnames (ball) [ncol(ball)] <- "ball_xg"

  ball <- merge(ball, shot_sum[,c(1,9)], 
                by.x = c("end_zone"), by.y = c("shot_zone"), all.x = T)
  ball$pred <- ifelse(is.na(ball$pred), 0, ball$pred)
  colnames (ball) [ncol(ball)] <- "end_zone_xg" 
  
  ball$xg_diff <- ifelse(ball$ball_xg == 0 & ball$end_zone_xg == 0, 0, ball$end_zone_xg / (ball$ball_xg + ball$end_zone_xg))
  
  # distance from goal
  ball$end_zone_distance <- sqrt((ball$centre_x - 120) ^ 2 + (ball$centre_y - 40) ^ 2)
  ball$distance_diff <- ball$distance / (ball$distance + ball$end_zone_distance)

  # calculate danger for each frame and write it into separate data frame
  ball$danger <- ball$pass_pred + ball$xg_diff + ball$distance_diff
  frame <- ball$frame
  danger <- ball$danger
  end_zone <- data.frame(frame,danger) 
  end_zone$number <- i
  end_zone <- merge (end_zone, zones [,c(1:4,7)], by = c("number"))
  colnames (end_zone) [3] <- paste("danger", "zone", i, sep = "_")
  colnames (end_zone) [4] <- paste("y_start", "zone", i, sep = "_")
  colnames (end_zone) [5] <- paste("x_start", "zone", i, sep = "_")
  colnames (end_zone) [6] <- paste("x_end", "zone", i, sep = "_")
  colnames (end_zone) [7] <- paste("y_end", "zone", i, sep = "_")
  
  game <- merge (game, end_zone [,c(2:7)], by = c("frame"))
  
  ball <- ball[,c(2:19)]
}

pitch_create <- ggplot(game, aes(x,y)) +
geom_rect (aes(xmin = game$x_start_zone_1, ymin = game$y_start_zone_1, 
    xmax = game$x_end_zone_1, ymax = game$y_end_zone_1, 
    fill = game$danger_zone_1), alpha = .1) + 
geom_rect (aes(xmin = game$x_start_zone_2, ymin = game$y_start_zone_2, 
    xmax = game$x_end_zone_2, ymax = game$y_end_zone_2, 
    fill = game$danger_zone_2), alpha = .1) + 
geom_rect (aes(xmin = game$x_start_zone_3, ymin = game$y_start_zone_3, 
    xmax = game$x_end_zone_3, ymax = game$y_end_zone_3, 
    fill = game$danger_zone_3), alpha = .1) + 
geom_rect (aes(xmin = game$x_start_zone_4, ymin = game$y_start_zone_4, 
    xmax = game$x_end_zone_4, ymax = game$y_end_zone_4, 
    fill = game$danger_zone_4), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_5, ymin = game$y_start_zone_5, 
    xmax = game$x_end_zone_5, ymax = game$y_end_zone_5, 
    fill = game$danger_zone_5), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_6, ymin = game$y_start_zone_6, 
    xmax = game$x_end_zone_6, ymax = game$y_end_zone_6, 
    fill = game$danger_zone_6), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_7, ymin = game$y_start_zone_7, 
    xmax = game$x_end_zone_7, ymax = game$y_end_zone_7, 
    fill = game$danger_zone_7), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_8, ymin = game$y_start_zone_8, 
    xmax = game$x_end_zone_8, ymax = game$y_end_zone_8, 
    fill = game$danger_zone_8), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_9, ymin = game$y_start_zone_9, 
    xmax = game$x_end_zone_9, ymax = game$y_end_zone_9, 
    fill = game$danger_zone_9), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_10, ymin = game$y_start_zone_10, 
    xmax = game$x_end_zone_10, ymax = game$y_end_zone_10, 
    fill = game$danger_zone_10), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_11, ymin = game$y_start_zone_11, 
    xmax = game$x_end_zone_11, ymax = game$y_end_zone_11, 
    fill = game$danger_zone_11), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_12, ymin = game$y_start_zone_12, 
    xmax = game$x_end_zone_12, ymax = game$y_end_zone_12, 
    fill = game$danger_zone_12), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_13, ymin = game$y_start_zone_13, 
    xmax = game$x_end_zone_13, ymax = game$y_end_zone_13, 
    fill = game$danger_zone_13), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_14, ymin = game$y_start_zone_14, 
    xmax = game$x_end_zone_14, ymax = game$y_end_zone_14, 
    fill = game$danger_zone_14), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_15, ymin = game$y_start_zone_15, 
    xmax = game$x_end_zone_15, ymax = game$y_end_zone_15, 
    fill = game$danger_zone_15), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_16, ymin = game$y_start_zone_16, 
    xmax = game$x_end_zone_16, ymax = game$y_end_zone_16, 
    fill = game$danger_zone_16), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_17, ymin = game$y_start_zone_17, 
    xmax = game$x_end_zone_17, ymax = game$y_end_zone_17, 
    fill = game$danger_zone_17), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_18, ymin = game$y_start_zone_18, 
    xmax = game$x_end_zone_18, ymax = game$y_end_zone_18, 
    fill = game$danger_zone_18), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_19, ymin = game$y_start_zone_19, 
    xmax = game$x_end_zone_19, ymax = game$y_end_zone_19, 
    fill = game$danger_zone_19), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_20, ymin = game$y_start_zone_20, 
    xmax = game$x_end_zone_20, ymax = game$y_end_zone_20, 
    fill = game$danger_zone_20), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_21, ymin = game$y_start_zone_21, 
    xmax = game$x_end_zone_21, ymax = game$y_end_zone_21, 
    fill = game$danger_zone_21), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_22, ymin = game$y_start_zone_22, 
    xmax = game$x_end_zone_22, ymax = game$y_end_zone_22, 
    fill = game$danger_zone_22), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_23, ymin = game$y_start_zone_23, 
    xmax = game$x_end_zone_23, ymax = game$y_end_zone_23, 
    fill = game$danger_zone_23), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_24, ymin = game$y_start_zone_24, 
    xmax = game$x_end_zone_24, ymax = game$y_end_zone_24, 
    fill = game$danger_zone_24), alpha = 1) + 
geom_rect (aes(xmin = game$x_start_zone_25, ymin = game$y_start_zone_25, 
  xmax = game$x_end_zone_25, ymax = game$y_end_zone_25, 
  fill = game$danger_zone_25), alpha = 1) + 
geom_point(col=game$bgcolor, size = game$size) +
xlim (-5,125) + ylim (-5,85) +
annotate("text", x = 60, y = 85, label = title, family = "serif", fontface = "bold") +
theme (axis.text = element_blank(), # setting the theme, removing all chunk
  axis.title = element_blank(),
  axis.ticks = element_blank(), 
  panel.grid.major=element_blank(),
  panel.grid.minor=element_blank(),
  panel.background=element_rect(fill="darkgreen",colour="darkgreen")) + 
annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80, col = "black", alpha = 0) + # pitch
annotate("rect", xmin = 0, xmax = 18, ymin = 18, ymax = 62, col = "black", alpha = 0) + # box left
annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62, col = "black", alpha = 0) + # box right
annotate("rect", xmin = 0, xmax = 6, ymin = 30, ymax = 50, col = "black", alpha = 0) + # six yard box left
annotate("rect", xmin = 114, xmax = 120, ymin = 30, ymax = 50, col = "black", alpha = 0) + # six yard box right
annotate("rect", xmin = -1, xmax = 0, ymin = 36, ymax = 44, col = "black", alpha = 0) + # goal left
annotate("rect", xmin = 120, xmax = 121, ymin = 36, ymax = 44, col = "black", alpha = 0) + # goal right
geom_segment(x = 60, xend = 60, y = 0, yend = 80, col = "black") + # middle line
geom_point(aes(x = 12, y = 40), col = "black") + # penalty spot left
geom_point(aes(x = 108, y = 40), col = "black") + # penalty spot right
geom_point(aes(x = 60, y = 40), shape = 1) + # kick-off point
annotate("path", # semicircle left
  x = 12 + 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
  y = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
  col = "black") +
annotate("path", # semicircle right
  x = 108 - 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
  y = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
  col = "black") +
annotate("path", # middle circle
  x = 60 + 10 * cos(seq(-pi, pi, length.out = 30)),
  y = 40 + 10 * sin(seq(-pi, pi, length.out = 30)),
  col = "black")  +
theme (legend.position = "none")

pitch_create_animate <- pitch_create + transition_time(frame) + enter_fade()

anim <- animate(pitch_create_animate, height = 461, width = 700)

filename <- paste0("C:\\Users\\johan\\Documents\\lfc competition\\", title, ".gif")
anim_save(filename, anim)

}

# set pieces not very useful (just one): 
# consider different variables