# LFC competition

rm(list = ls())
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

# filter passes and shots
pass <- subset (data_sb, type.id == 30)
shots <- subset (data_sb, type.id == 16)
rm(data_sb) 

# Pass model
pass <- passes[,c("location", "pass.length", "pass.angle", 
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

# define end zones of passes and calculate average pass success rate
pass$third <- 0
pass$third <- ifelse(pass$x_end < 40, 1, pass$third)
pass$third <- ifelse(pass$x_end >= 40 & pass$x_end <= 80, 2, pass$third)
pass$third <- ifelse(pass$x_end > 80, 3, pass$third)

pass$strip <- 0
pass$strip <- ifelse(pass$y_end < 18, 1, pass$strip)
pass$strip <- ifelse(pass$y_end >= 18 & pass$y_end < 30, 2, pass$strip)
pass$strip <- ifelse(pass$y_end >= 30 & pass$y_end <= 50, 3, pass$strip)
pass$strip <- ifelse(pass$y_end > 50 & pass$y_end <= 62, 4, pass$strip)
pass$strip <- ifelse(pass$y_end > 62, 5, pass$strip)

pass$end_zone <- pass$strip
pass$end_zone <- ifelse(pass$third == 2, pass$strip + 5, pass$end_zone)
pass$end_zone <- ifelse(pass$third == 3, pass$strip + 10, pass$end_zone)

pass$passes <- 1

zones <- aggregate(list(passes = pass$passes, successful = pass$successful),
                   by = list(end_zone = pass$end_zone), FUN = sum)
zones$zone_percentage <- zones$successful / zones$passes

pass <- merge(pass, zones[,c(1,4)], by = c("end_zone"))

# standardize angle to forward passes
pass$forward <- ifelse(pass$x < pass$x_end, 1, 0)

# pass model
pass_model <- glm(successful ~ x + y + pass.length + forward + zone_percentage,
                  family = "binomial", data=pass)

# create new data set to predict pass success from each point of tracking data
pred <- data.frame (x_end = numeric(), y_end = numeric())
for (i in 0:120) {
  x_end <- i
  y_end <- seq(0, 80, 1)
  temp <- data.frame (x_end, y_end)
  pred <- rbind(pred,temp)
    
}

pred$third <- 0
pred$third <- ifelse(pred$x_end < 40, 1, pred$third)
pred$third <- ifelse(pred$x_end >= 40 & pred$x_end <= 80, 2, pred$third)
pred$third <- ifelse(pred$x_end > 80, 3, pred$third)

pred$strip <- 0
pred$strip <- ifelse(pred$y_end < 18, 1, pred$strip)
pred$strip <- ifelse(pred$y_end >= 18 & pred$y_end < 30, 2, pred$strip)
pred$strip <- ifelse(pred$y_end >= 30 & pred$y_end <= 50, 3, pred$strip)
pred$strip <- ifelse(pred$y_end > 50 & pred$y_end <= 62, 4, pred$strip)
pred$strip <- ifelse(pred$y_end > 62, 5, pred$strip)

pred$end_zone <- pred$strip
pred$end_zone <- ifelse(pred$third == 2, pred$strip + 5, pred$end_zone)
pred$end_zone <- ifelse(pred$third == 3, pred$strip + 10, pred$end_zone)

pred <- merge(pred, zones[,c(1,4)], by = c("end_zone"))

pred$third <- NULL
pred$strip <- NULL
pred$end_zone <- NULL

# Shots 
shots <- shots[,c("location", "shot.statsbomb_xg")]

# extract x, y location 
shots$x <- sub("\\,.*", "", shots$location)
shots$x <- substr(shots$x, 3, nchar(shots$x))
shots$x <- as.numeric(shots$x)

shots$y <- sub("^[^,]*", "", shots$location)
shots$y <- substr(shots$y, 3, nchar(shots$y)-1)
shots$y <- as.numeric(shots$y)

# y needs to be normalised for linear regression
shots$central <- sqrt((shots$y - 40)**2)

# categorize shots

zone <- data.frame (x_cat = numeric(), y_cat = numeric())
for (i in 45:120) {
  x_cat <- i
  y_cat <- seq(0, 80, 1)
  temp <- data.frame (x_cat, y_cat)
  zone <- rbind(zone, temp)
  
}

# modelling xg according to x,y location of shot
xg_model <- lm(shot.statsbomb_xg ~ x + central, data = shots)

# include into prediction data frame
pred$x <- pred$x_end
pred$central <- sqrt((pred$y_end - 40)**2)
pred$xg <- predict(xg_model, newdata = pred)
pred$xg <- ifelse(pred$x_end < 60 | pred$xg < 0, 0, pred$xg)
pred$x <- NULL
pred$central <- NULL

# need to make coordinates of tracking data comparable with StatsBomb and SBPitch
pres <- data
pres$x <- pres$x / 100 * 120
pres$y <- pres$y / 100 * 80

temp <- subset (pres, play == "Liverpool [3] - 0 Bournemouth" ) #& player == 0)
players <- subset (temp, player != 0)
ball <- subset (temp, player == 0)
# calculate pass probability and xg from pass end location for each frame
for (i in min(ball$X):max(ball$X)) {

  temp <- subset (ball, X == i)
  pred$x <- temp$x
  pred$y <- temp$y
  pred$forward <- ifelse(pred$x < pred$x_end, 1, 0)
  pred$pass.length <- sqrt((pred$x_end - pred$x) ^ 2 + (pred$x_end - pred$y) ^ 2)
  pred$pass_success <- predict(pass_model, pred, type = "response")
  pred$danger <- 0
  pred$danger <- ifelse (pred$x_end < 40, pred$pass_success * pred$forward / 2, pred$danger)
  pred$danger <- ifelse (pred$x_end >= 40 & pred$x_end <= 80, pred$pass_success * pred$forward / 2, pred$danger)
  pred$danger <- ifelse (pred$x_end > 80, pred$xg + pred$pass_success, pred$danger)
}

create_Pitch(grass_colour = "darkgreen", line_colour = "black",
             background_colour = "darkgreen") + 
  geom_point(aes(x=pred$x, y=pred$y), col="black") + 
  geom_point(aes(x=pred$x_end, y=pred$y_end, col = pred$danger), size = 2)


















create_Pitch(grass_colour = "grey", line_colour = "black",
             background_colour = "grey") +
  geom_point(aes(x=players$x, y=players$y), col=players$bgcolor) + 
  #ggplot() + 
  geom_point(aes(x=ball$x, y=ball$y), col = "black") +
  transition_states(states = frame) +
  labs(title = "Year: {frame_time}")
