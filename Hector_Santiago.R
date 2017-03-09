############
# Preamble #
############

library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

setwd("C:/Users/jack.werner1/Documents/BB")
#setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Scraping Files")

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
hector.pre <- pitch %>% filter(pitcher == 502327) %>%
  left_join(simpleResults, by = "event") %>%
  left_join(simplePitches, by = "pitch_type") %>%
  left_join(pitcher_names, by = c("batter" = "id")) %>%
  rename(batter_name = name) %>%
  left_join(pitcher_names, by = c("pitcher" = "id")) %>%
  rename(pitcher_name = name)

# A
hector <- hector.pre %>% 
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

table(hector$simple_pitch_type)

# Get rid of unknowns
hector <- hector %>% filter(simple_pitch_type != "UN" & simple_pitch_type != "PO") %>%
  mutate(simple_pitch_type = as.character(simple_pitch_type))

# Break
ggplot(data = hector, aes(pfx_x, pfx_z, color = simple_pitch_type)) + geom_point()

# Velocity
ggplot(data = hector, aes(start_speed)) + facet_grid(simple_pitch_type~.) + geom_histogram()

ggplot(data = hector, aes(start_speed, fill = simple_pitch_type)) + 
  geom_density(alpha = .5, color = "grey50", size = 1)


###############################
# Pitches by count/handedness #
###############################

tables <- hector %>% group_by(count, balls, strikes, b_hand) %>%
  summarize(FC = sum(simple_pitch_type == "FC"),
            SI = sum(simple_pitch_type == "SI"),
            CH = sum(simple_pitch_type == "CH"),
            CU = sum(simple_pitch_type == "CU"),
            SL = sum(simple_pitch_type == "SL"),
            FC_p = FC/n(), SI_p = SI/n(), CH_p = CH/n(),
            CU_p = CU/n(), SL_p = SL/n(), total = n()) %>%
  ungroup()

tables %>% filter(b_hand == "R") %>% as.data.frame()

hector$simple_pitch_type <- factor(hector$simple_pitch_type, 
                                   levels = c("CU", "SL", "CH", "FC", "SI"))

ggplot(data = hector, aes(b_hand, fill = simple_pitch_type)) + facet_grid(strikes~balls) + 
  geom_bar(position = "fill")

ggplot(data = filter(hector, simple_pitch_type != "SI"), aes(b_hand, fill = simple_pitch_type)) + 
  facet_grid(strikes~balls) + 
  geom_bar(position = "fill")

table(hector$simple_pitch_type, hector$b_hand) %>% prop.table(2)


########################################
# Pitch distribution as season went on #
########################################

gidToDate <- function(g) {
  gc <- as.character(g) %>%
    strsplit("_")
  
  date.char <- gc[[1]][2:4] %>% paste0(collapse = "")
  
  return(date.char)
}

hector$date <- sapply(hector$gid, gidToDate)
hector$date <- ymd(hector$date)

ggplot(data = hector, aes(date, fill = simple_pitch_type)) + geom_bar(position = "fill")

a <- hector %>% group_by(date) %>% summarize(SI = sum(simple_pitch_type == "SI")/n(),
                                             FC = sum(simple_pitch_type == "FC")/n(),
                                             CH = sum(simple_pitch_type == "CH")/n(),
                                             SL = sum(simple_pitch_type == "SL")/n(),
                                             CU = sum(simple_pitch_type == "CU")/n()) %>%
  gather("Pitch", "Freq", 2:6) %>% ungroup()

ggplot(a, aes(x = date, y = Freq, color = Pitch)) + geom_line(size = 1) + geom_point(size = 3)


#############################
# Check out pitch locations #
#############################

strike.zone <- data.frame(x = c(17/24, 17/24, -17/24, -17/24, 17/24), y = c(1.5812, 3.4499, 3.4499, 1.5812, 1.5812))

ggplot(data = filter(hector, pitch_result %in% c("Ball", "Ball In Dirt", "Called Strike")), 
       aes(px, pz, color = type)) + 
  geom_point() +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

ggplot(data = hector, aes(px, pz, color = simple_pitch_type)) + geom_point() +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

ggplot(data = filter(hector, simple_pitch_type != "SI"), aes(px, pz, color = simple_pitch_type)) + 
  geom_point() +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

ggplot(data = hector, aes(px, pz)) + geom_point() + facet_wrap(~simple_pitch_type) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()


####################
# Pitches by count #
####################

ggplot(data = hector, aes(px, pz, color = simple_pitch_type)) + geom_point() + facet_grid(strikes~balls) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

# Sinkers
ggplot(data = filter(hector, simple_pitch_type == "SI"), aes(px, pz, color = simple_pitch_type)) + 
  geom_point() + facet_grid(strikes~balls) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

# Cutters
ggplot(data = filter(hector, simple_pitch_type == "FC"), aes(px, pz, color = simple_pitch_type)) + 
  geom_point() + facet_grid(strikes~balls) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

# Sliders
ggplot(data = filter(hector, simple_pitch_type == "SL"), aes(px, pz, color = simple_pitch_type)) + 
  geom_point() + facet_grid(strikes~balls) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

# Changeups
ggplot(data = filter(hector, simple_pitch_type == "CH"), aes(px, pz, color = simple_pitch_type)) + 
  geom_point() + facet_grid(strikes~balls) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

# Curveballs
ggplot(data = filter(hector, simple_pitch_type == "CU"), aes(px, pz, color = simple_pitch_type)) + 
  geom_point() + facet_grid(strikes~balls) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()




#################
# Pitch strings #
#################

# Actual

abid <- paste0(hector$gid, hector$ab_num)

k.pitch.adj <- ifelse(hector$simple_pitch_type == "SL", "LL",
                      ifelse(hector$simple_pitch_type == "CU", "UU", 
                             as.character(hector$simple_pitch_type)))
pitches.1 <- k.pitch.adj %>% as.character() %>% substr(1, 1)

seqs <- tapply(pitches.1, abid, paste0, collapse = "", simplify = T) %>%
  as.vector()

ps <- c("F", "S", "C", "U", "L")
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

duos.df <- seqs.df[1:25,]
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


# Consecutive changeups?

hector <- hector %>% group_by(gid, ab_num) %>%
  mutate(prev_pitch = lag(as.character(simple_pitch_type), default = "X")) %>%
  ungroup()


View(hector %>% select(ab_num, simple_pitch_type, prev_pitch))
table(ifelse(hector$prev_pitch == "CH", "Prev Change", "Not"),
      ifelse(hector$simple_pitch_type == "CH", "Changeup", "Not")) %>%
  prop.table(1)

table(ifelse(hector$prev_pitch == "CH", "Prev Change", "Not"),
      ifelse(hector$simple_pitch_type == "CH", "Changeup", "Not"),
      hector$count) %>%
  prop.table(c(1, 3))

hector$prev_change = hector$prev_pitch == "CH"
hector$change = hector$simple_pitch_type == "CH"

ggplot(data = hector, aes(prev_change, fill = change)) +
  geom_bar(position = "fill") +
  facet_grid(strikes~balls)


change.df <- hector %>% group_by(count, prev_change) %>%
  summarize(change_perc = sum(simple_pitch_type == "CH")/n()*100) %>%
  ungroup() %>% mutate(h_adj_all = 1.3 - 1.8*prev_change, h_adj_rh = h_adj_all,
                       v_adj_all = 0, v_adj_rh = 0)


# ARTICLE

change.df$v_adj_all[change.df$count == "1-0" & !change.df$prev_change] <- -.6
change.df$v_adj_all[change.df$count == "0-0" & !change.df$prev_change] <- -.4
change.df$v_adj_all[change.df$count == "2-2" & !change.df$prev_change] <- .8
change.df$v_adj_all[change.df$count == "2-0" & !change.df$prev_change] <- .4
change.df$v_adj_all[change.df$count == "3-2" & !change.df$prev_change] <- .8
change.df$v_adj_all[change.df$count == "0-2" & !change.df$prev_change] <- .4
change.df$v_adj_all[change.df$count == "3-1" & !change.df$prev_change] <- .6

ggplot(data = change.df, aes(x = prev_change, y = change_perc, group = count)) + 
  geom_text(aes(label = count, hjust = h_adj_all, vjust = v_adj_all), size = 5) + 
  geom_line() + geom_point() +
  coord_cartesian(ylim = c(0, 50), xlim = c(1.25, 1.75)) #+
#   labs(x = "Changeup on Previous Pitch?", y = "Changeup percentage", title = "Changeup Percentage by Previous Pitch") +
#   theme(legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
#         legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
#         plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
#         axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
#         axis.text.x = element_text(family = "Trebuchet MS", color="#666666", size=15),
#         axis.text.y = element_text(family = "Trebuchet MS", color="#666666", size=20))


# Just look at to righties
table(ifelse(hector$prev_pitch == "CH", "Prev Change", "Not")[hector$b_hand == "R"],
      ifelse(hector$simple_pitch_type == "CH", "Changeup", "Not")[hector$b_hand == "R"],
      hector$count[hector$b_hand == "R"]) %>%
  prop.table(c(1, 3))

ggplot(data = filter(hector, b_hand == "R"), aes(prev_change, fill = change)) +
  geom_bar(position = "fill") +
  facet_grid(strikes~balls)

changes <- hector %>% group_by(count, balls, strikes, prev_change) %>%
  summarize(change_perc = sum(change)/n()) %>%
  ungroup()



