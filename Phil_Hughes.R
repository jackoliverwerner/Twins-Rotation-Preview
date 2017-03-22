############
# Preamble #
############

library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

#setwd("C:/Users/jack.werner1/Documents/BB")
setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Scraping Files")

# Read data
pitch <- read.csv(file = "phil.csv")

#p2 <- read.csv(file = "pitch_data_2016.csv")

####################
# Reference tables #
####################

# At-bat results
simpleResults <- data.frame(event = as.character(sort(unique(pitch$event))),
                            simple_event = c("Out", "Out", "Hit", "Out", "Hit", "Out", 
                                             "Out", "Out", "Out", "Out", "HBP", "Hit", 
                                             "Out", "Out", "Out", "Out", "Out", "Hit", 
                                             "K", "Hit", "BB"),
                            stringsAsFactors = F)

# Pitch classifications
simplePitches <- data.frame(pitch_type = sort(as.character(unique(pitch$pitch_type))),
                            simple_pitch_type = c("UN", "CH", "FC", "FF", "FT", "CU"),
                            fastball = c("UN", "O", "F", "F", "F", "O")
)

# Player names/IDs
pitcher_names <- read.csv("playerid_list.csv") %>%
  mutate(name = paste0(FIRSTNAME, " ", LASTNAME), id = MLBCODE) %>%
  select(name, id)


######################
# Manipulate dataset #
######################

# Add Simple Event, Simple Pitch Type, Fastball, Player Names
phil.pre <- pitch %>% filter(pitcher == 461833) %>%
  left_join(simpleResults, by = "event") %>%
  left_join(simplePitches, by = "pitch_type") %>%
  left_join(pitcher_names, by = c("batter" = "id")) %>%
  rename(batter_name = name) %>%
  left_join(pitcher_names, by = c("pitcher" = "id")) %>%
  rename(pitcher_name = name)

# A
phil <- phil.pre %>% 
  mutate(hand_match = as.character(b_hand) == as.character(p_throws)) %>%     # Handedness match
  group_by(gid, ab_num) %>%
  mutate(finalCount = paste0(b, "-", s),     # Count on last pitch
         last = row_number() == n(),
         next_balls = pmin(cumsum(type == "B"), 3), next_strikes = pmin(cumsum(type == "S"), 2),
         next_count = ifelse(last, simple_event, paste0(next_balls, "-", next_strikes)),
         count = lag(as.character(next_count), default = "0-0"),
         balls = lag(as.character(next_balls), default = "0"),
         strikes = lag(as.character(next_strikes), default = "0")) %>% 
  ungroup()


#########################
# Check out pitch types #
#########################

table(phil$simple_pitch_type)

# Get rid of unknowns
phil <- phil %>% filter(simple_pitch_type != "UN" & simple_pitch_type != "PO") %>%
  mutate(simple_pitch_type = as.character(simple_pitch_type))

# Break
ggplot(data = phil, aes(pfx_x, pfx_z, color = simple_pitch_type)) + geom_point()

# Velocity
ggplot(data = phil, aes(start_speed)) + facet_grid(simple_pitch_type~.) + geom_histogram()

ggplot(data = phil, aes(start_speed, fill = simple_pitch_type)) + 
  geom_density(alpha = .5, color = "grey50", size = 1)

phil$simple_pitch_type <- ifelse(phil$simple_pitch_type == "FT", "FF",
                                 phil$simple_pitch_type)

########
# PCPS #
########

leaguewide_counts <- read.csv("leaguewide_counts.csv")

phil_counts <- phil %>% group_by(count) %>%
  summarize(ff_p = sum(fastball == "F")/n()) %>%
  ungroup() %>%
  join(leaguewide_counts, by = "count") %>%
  mutate(adj_ff_p = sum(ff_p*freq),
         diff = ff_p - adj_ff_p,
         abs_diff = abs(diff)*freq)

# 0.060826
(pcps <- sum(phil_counts$abs_diff))

###############################
# Pitches by count/handedness #
###############################



table(phil$simple_pitch_type, phil$b_hand) %>% prop.table(2)

phil$fastball <- factor(phil$fastball, levels = c("O", "F"))

ggplot(data = phil, aes(pitcher, fill = fastball)) + facet_grid(strikes~balls) + 
  geom_bar(position = "fill") +
  labs(x = "Balls", y = "Strikes", title = "Fastball Percentage by Count",
       fill = "Pitch") +
  scale_fill_manual(values = c("#E31A1C", "#12496D"),
                    labels = c("Fastball", "Offspeed")) +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Trebuchet MS", color="#666666", size=15),
        strip.text = element_text(family = "Trebuchet MS", color="#666666", size=20),
        panel.margin = unit(0, "lines"))


ggplot(data = phil, aes(pitcher, fill = simple_pitch_type)) + facet_grid(strikes~balls) + 
  geom_bar(position = "fill") +
  labs(x = "Balls", y = "Strikes", title = "Pitch Types by Count",
       fill = "Pitch") +
  scale_fill_manual(values = c("#E31A1C", "#FB9A99", "#1F78B4", "#12496D"),
                    labels = c("Changeup", "Curveball", "Cutter", "Four-seam")) +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Trebuchet MS", color="#666666", size=15),
        strip.text = element_text(family = "Trebuchet MS", color="#666666", size=20),
        panel.margin = unit(0, "lines"))


ggplot(data = phil, aes(b_hand, fill = simple_pitch_type)) + facet_grid(strikes~balls) + 
  geom_bar(position = "fill") +
  labs(x = "Balls", y = "Strikes", title = "Pitch Types by Count and Handedness",
       fill = "Pitch") +
  scale_fill_manual(values = c("#E31A1C", "#FB9A99", "#1F78B4", "#12496D"),
                    labels = c("Changeup", "Curveball", "Cutter", "Four-seam")) +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Trebuchet MS", color="#666666", size=15),
        strip.text = element_text(family = "Trebuchet MS", color="#666666", size=20),
        panel.margin = unit(0, "lines"))









#############
# Locations #
#############

strike.zone <- data.frame(x = c(17/24, 17/24, -17/24, -17/24, 17/24), y = c(1.5812, 3.4499, 3.4499, 1.5812, 1.5812))

ggplot(data = filter(phil, pitch_result %in% c("Ball", "Ball In Dirt", "Called Strike")), 
       aes(px, pz, color = type)) + 
  geom_point() +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()


ggplot(data = phil, aes(px, pz, color = simple_pitch_type)) + geom_point() +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

ggplot(data = phil, aes(px, pz)) + geom_point() + facet_wrap(~simple_pitch_type) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()


ggplot(data = phil, aes(px, pz, color = simple_pitch_type)) + geom_point() + facet_grid(strikes~balls) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()


# Cutter ARTICLE
ggplot(data = filter(phil, simple_pitch_type == "FC"), 
       aes(px, pz)) + facet_wrap(~b_hand) +
  geom_point() +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

ggplot(data = filter(phil, simple_pitch_type == "FC", b_hand == "L"), 
       aes(px, pz)) +
  geom_point(size = 4, color = "#BA0C2E") +
  geom_polygon(data = strike.zone, 
               aes(x = x, y = y, color = NA), 
               fill = NA, color = "black", size = 1) +
  coord_fixed() +
  labs(x = "", y = "", title = "Cutters to Lefties") +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(family = "Trebuchet MS", color="#666666", size=20))


# ARTICLE
phil$twostrikes <- ifelse(phil$strikes == 2, "Two Strikes", "Less Than Two Strikes")
ggplot(data = filter(phil, simple_pitch_type == "FC", b_hand == "R"), 
       aes(px, pz)) + facet_wrap(~twostrikes) +
  geom_point(size = 2, color = "#BA0C2E") +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed() +
  labs(x = "", y = "", title = "Cutters to Righties") +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(family = "Trebuchet MS", color="#666666", size=20))






phil <- phil %>% group_by(gid, ab_num) %>%
  mutate(prev_count = lag(as.character(count), default = "X"),
         prev_pitch = lag(as.character(simple_pitch_type), default = "X"))

#######
# 0-2 #
#######

ggplot(data = filter(phil, count == "0-2"), aes(px, pz, color = simple_pitch_type)) + 
  geom_point(size = 3) + facet_wrap(~b_hand) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

ggplot(data = filter(phil, count == "0-2"), aes(px, pz, color = simple_pitch_type)) + 
  geom_point(size = 3) + facet_grid(prev_count~b_hand) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

ggplot(data = filter(phil, count == "0-2", b_hand == "R"), aes(px, pz)) + 
  geom_point(size = 3) + facet_wrap(~simple_pitch_type) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()


#######
# 1-2 #
#######

ggplot(data = filter(phil, count == "1-2"), aes(px, pz, color = simple_pitch_type)) + 
  geom_point(size = 3) + facet_wrap(~b_hand) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

#######
# 2-2 #
#######

ggplot(data = filter(phil, count == "2-2"), aes(px, pz, color = simple_pitch_type)) + 
  geom_point(size = 3) + facet_wrap(~b_hand) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()


table(phil$px[phil$b_hand == "L" & phil$count == "0-2"] < 0)
table(phil$px[phil$b_hand == "L" & phil$strikes == 2] < 0)
table(phil$px[phil$b_hand == "L"] < 0)


#############
# 1-2 and 0-2 #
#############

ggplot(data = filter(phil, strikes == 2, balls < 2, b_hand == "L"), aes(px, pz, color = simple_pitch_type)) + 
  geom_point(size = 3) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed() +
  scale_color_manual(values = c("#E31A1C", "#FB9A99", "#1F78B4", "#12496D"),
                    labels = c("Changeup", "Curveball", "Cutter", "Four-seam")) +
  labs(x = "", y = "", title = "0-2 and 1-2 pitches to LH", fill = "Pitch") +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(family = "Trebuchet MS", color="#666666", size=20))

phil$h_labs <- ifelse(phil$b_hand == "L", "vs. LH", "vs. RH")
ggplot(data = filter(phil, strikes == 2, balls < 2), aes(px, pz, color = simple_pitch_type)) + 
  geom_point(size = 2) + facet_grid(h_labs~simple_pitch_type) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed() +
  scale_color_manual(values = c("#E31A1C", "#FB9A99", "#1F78B4", "#12496D"),
                     labels = c("Changeup", "Curveball", "Cutter", "Four-seam")) +
  labs(x = "", y = "", title = "0-2 and 1-2 Pitches", color = "Pitch") +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(family = "Trebuchet MS", color="#666666", size=20))



#################
# Pitch strings #
#################

# Actual

abid <- paste0(phil$gid, phil$ab_num)

k.pitch.adj <- ifelse(phil$simple_pitch_type == "CU", "KC",
                      ifelse(phil$simple_pitch_type == "FC", "TS", 
                             as.character(phil$simple_pitch_type)))
pitches.1 <- k.pitch.adj %>% as.character() %>% substr(1, 1)

seqs <- tapply(pitches.1, abid, paste0, collapse = "", simplify = T) %>%
  as.vector()

ps <- c("C", "F", "K", "T")
all.duos <- paste0(rep(ps, each = 5), ps)
all.trios <- paste0(rep(all.duos, each = 5), ps)


duos.to.match <- paste0("(?=", all.duos, ")")
duos.freq <- rep(0, length(all.duos))

for (i in 1:length(all.duos)) {
  duos.freq[i] <- gregexpr(duos.to.match[i], seqs, perl = T) %>% sapply(function(x){sum(x>0)}) %>% sum()
}


trios.to.match <- paste0("(?=", all.trios, ")")
trios.freq <- rep(0, length(all.trios))

for (i in 1:length(all.trios)) {
  trios.freq[i] <- gregexpr(trios.to.match[i], seqs, perl = T) %>% sapply(function(x){sum(x>0)}) %>% sum()
}

seqs.df <- data.frame(pattern = c(all.duos, all.trios), freq = c(duos.freq, trios.freq))


# Random

rand.freqs <- matrix(0, nrow = length(all.duos) + length(all.trios), ncol = 100)

for (j in 1:100) {
  pitches.1.r <- k.pitch.adj %>% as.character() %>% substr(1, 1) %>% sample()
  
  seqs.r <- tapply(pitches.1.r, abid, paste0, collapse = "", simplify = T) %>%
    as.vector()
  
  duos.freq.r <- rep(0, length(all.duos))
  
  for (i in 1:length(all.duos)) {
    duos.freq.r[i] <- gregexpr(duos.to.match[i], seqs.r, perl = T) %>% sapply(function(x){sum(x>0)}) %>% sum()
  }
  
  
  trios.freq.r <- rep(0, length(all.trios))
  
  for (i in 1:length(all.trios)) {
    trios.freq.r[i] <- gregexpr(trios.to.match[i], seqs.r, perl = T) %>% sapply(function(x){sum(x>0)}) %>% sum()
  }
  
  rand.freqs[,j] <- c(duos.freq.r, trios.freq.r)
  
  print(j)
}

seqs.df <- data.frame(pattern = c(all.duos, all.trios), 
                      freq = c(duos.freq, trios.freq), 
                      exp = apply(rand.freqs, 1, mean)) %>%
  mutate(p_diff = (freq - exp)/exp)

duos.df <- seqs.df[1:20,]
trios.df <- seqs.df[26:nrow(seqs.df),]

ggplot(data = duos.df, aes(x = exp, y = freq, label = pattern)) + 
  geom_text() + 
  geom_abline(slope = 1, intercept = 0, color = "red")

ggplot(data = trios.df, aes(x = exp, y = freq, label = pattern)) + 
  geom_text() + 
  geom_abline(slope = 1, intercept = 0, color = "red")

twins_blue <- "#0C2341"
twins_red <- "#BA0C2E"
twins_gold <- "#CFAB7A"

# ARTICLE
ggplot(data = filter(duos.df, exp > 75), aes(x = reorder(pattern, -p_diff), y = p_diff)) + 
  geom_bar(stat = "identity", fill = twins_blue, color = twins_gold, size = 1) + theme_minimal()
ggplot(data = filter(trios.df, exp > 20), aes(x = reorder(pattern, -p_diff), y = p_diff)) + geom_bar(stat = "identity")



########################################
# Pitch distribution as season went on #
########################################

gidToDate <- function(g) {
  gc <- as.character(g) %>%
    strsplit("_")
  
  date.char <- gc[[1]][2:4] %>% paste0(collapse = "")
  
  return(date.char)
}

phil$date <- sapply(phil$gid, gidToDate)
phil$date <- ymd(phil$date)

ggplot(data = phil, aes(date, fill = simple_pitch_type)) + 
  geom_bar(position = "fill") +
  labs(x = "Date", y = "Pitch Types", 
       title = "2015 Pitch Types by Start", fill = "Pitch") +
  scale_fill_manual(values = c("#E31A1C", "#FB9A99", "#1F78B4", "#12496D"),
                    labels = c("Changeup", "Curveball", "Cutter", "Four-seam")) +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
        axis.text.x = element_text(family = "Trebuchet MS", color="#666666", size=15),
        axis.text.y = element_blank(),
        strip.text = element_text(family = "Trebuchet MS", color="#666666", size=20))


a <- phil %>% group_by(date) %>% summarize(FF = sum(simple_pitch_type == "FF")/n(),
                                           FC = sum(simple_pitch_type == "FC")/n(),
                                           CH = sum(simple_pitch_type == "CH")/n(),
                                           CU = sum(simple_pitch_type == "CU")/n(),
                                           tot = n()) %>%
  gather("Pitch", "Freq", 2:5) %>% ungroup()

ggplot(a, aes(x = date, y = Freq, color = Pitch)) + geom_line(size = 1) + geom_point(size = 3)


###################
# 2016 #
###################

# Read data
pitch <- read.csv(file = "pitch_data_2016.csv")

####################
# Reference tables #
####################

# At-bat results
simpleResults <- data.frame(event = as.character(sort(unique(pitch$event))),
                            simple_event = c("Out", "Out", "Out", "Out", "HBP",
                                             "Hit", "Out", "Hit", "Out", "Out",
                                             "Out", "Out", "Out", "Out", "Out",
                                             "HBP", "Hit", "BB", "Out", "Out",
                                             "Out", "Out", "Out", "Out", "Out",
                                             "Hit", "K", "K", "Hit", "Out", "BB"),
                            stringsAsFactors = F)

# Pitch classifications
simplePitches <- data.frame(pitch_type = sort(as.character(unique(pitch$pitch_type))),
                            simple_pitch_type = c("UN", "UN",  "CH", "CU", "CH", "FC", "FF", 
                                                  "PO", "SI", "FT", "UN", "CU", "KN", "PO",
                                                  "UN", "SI", "SL", "UN"),
                            fastball = c("UN", "UN", "O", "O", "O", "F", "F", "O", "F",
                                         "F", "UN", "O", "O", "O", "UN", "F", "O", "UN")
)

# Player names/IDs
pitcher_names <- read.csv("playerid_list.csv") %>%
  mutate(name = paste0(FIRSTNAME, " ", LASTNAME), id = MLBCODE) %>%
  select(name, id)


######################
# Manipulate dataset #
######################

# Add Simple Event, Simple Pitch Type, Fastball, Player Names
phil_16.pre <- pitch %>% filter(pitcher == 461833) %>%
  left_join(simpleResults, by = "event") %>%
  left_join(simplePitches, by = "pitch_type") %>%
  left_join(pitcher_names, by = c("batter" = "id")) %>%
  rename(batter_name = name) %>%
  left_join(pitcher_names, by = c("pitcher" = "id")) %>%
  rename(pitcher_name = name)

# A
phil_16 <- phil_16.pre %>% 
  mutate(hand_match = b_hand == p_throws) %>%     # Handedness match
  group_by(gid, ab_num) %>%
  mutate(finalCount = paste0(b, "-", s),     # Count on last pitch
         last = row_number() == n(),
         next_balls = pmin(cumsum(type == "B"), 3), next_strikes = pmin(cumsum(type == "S"), 2),
         next_count = ifelse(last, simple_event, paste0(next_balls, "-", next_strikes)),
         count = lag(as.character(next_count), default = "0-0"),
         balls = lag(as.character(next_balls), default = "0"),
         strikes = lag(as.character(next_strikes), default = "0")) %>% 
  ungroup()


#########################
# Check out pitch types #
#########################

table(phil_16$simple_pitch_type)

# Get rid of unknowns
phil_16 <- phil_16 %>% filter(simple_pitch_type != "UN" & simple_pitch_type != "PO") %>%
  mutate(simple_pitch_type = as.character(simple_pitch_type))

phil_16$simple_pitch_type <- ifelse(phil_16$simple_pitch_type == "FT", "FF", phil_16$simple_pitch_type)




###################
# Both #
###################

gidToDate <- function(g) {
  gc <- as.character(g) %>%
    strsplit("_")
  
  date.char <- gc[[1]][2:4] %>% paste0(collapse = "")
  
  return(date.char)
}

phil_16$date <- sapply(phil_16$gid, gidToDate)
phil_16$date <- ymd(phil_16$date)


both_by_date <- rbind(phil %>% ungroup() %>% select(date, simple_pitch_type), 
                      phil_16 %>% ungroup() %>% select(date, simple_pitch_type))


ggplot(data = phil_16, aes(date, fill = simple_pitch_type)) + 
  geom_bar(position = "fill") +
  labs(x = "Date", y = "Pitch Types", 
       title = "2016 Pitch Types by Start", fill = "Pitch") +
  scale_fill_manual(values = c("#E31A1C", "#FB9A99", "#1F78B4", "#12496D"),
                    labels = c("Changeup", "Curveball", "Cutter", "Four-seam")) +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
        axis.text.x = element_text(family = "Trebuchet MS", color="#666666", size=15),
        axis.text.y = element_blank(),
        strip.text = element_text(family = "Trebuchet MS", color="#666666", size=20))




a <- phil %>% group_by(date) %>% summarize(FF = sum(simple_pitch_type == "FF")/n(),
                                           FC = sum(simple_pitch_type == "FC")/n(),
                                           CH = sum(simple_pitch_type == "CH")/n(),
                                           CU = sum(simple_pitch_type == "CU")/n(),
                                           tot = n()) %>%
  gather("Pitch", "Freq", 2:5) %>% ungroup()

ggplot(a, aes(x = date, y = Freq, color = Pitch)) + geom_line(size = 1) + geom_point(size = 3)