############
# Preamble #
############

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

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
kyle.pre <- pitch %>% filter(pitcher == 502043) %>%
  left_join(simpleResults, by = "event") %>%
  left_join(simplePitches, by = "pitch_type") %>%
  left_join(pitcher_names, by = c("batter" = "id")) %>%
  rename(batter_name = name) %>%
  left_join(pitcher_names, by = c("pitcher" = "id")) %>%
  rename(pitcher_name = name)

# A
kyle <- kyle.pre %>% 
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

table(kyle$simple_pitch_type)

# Get rid of unknowns
kyle <- kyle %>% filter(simple_pitch_type != "UN") %>%
  mutate(simple_pitch_type = as.character(simple_pitch_type))

# Break
ggplot(data = kyle, aes(pfx_x, pfx_z, color = simple_pitch_type)) + geom_point()

# Velocity
ggplot(data = kyle, aes(start_speed)) + facet_grid(simple_pitch_type~.) + geom_histogram()

ggplot(data = kyle, aes(start_speed, fill = simple_pitch_type)) + 
  geom_density(alpha = .5, color = "grey50", size = 1)

kyle$simple_pitch_type <- factor(kyle$simple_pitch_type, levels = c("CU", "CH", "SL", "FT", "FF"))


###############################
# Pitches by count/handedness #
###############################

table(kyle$b_hand, kyle$simple_pitch_type) %>% prop.table(1)

ggplot(data = kyle, aes(b_hand, fill = simple_pitch_type)) + facet_grid(strikes~balls) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values = c("#12496D", "#1F78B4", "#A6CEE3", "#FB9A99", "#E31A1C"))

strike.zone <- data.frame(x = c(17/24, 17/24, -17/24, -17/24, 17/24), y = c(1.5812, 3.4499, 3.4499, 1.5812, 1.5812))

kyle$b_hand_lab = ifelse(kyle$b_hand == "L", "vs. LH", "vs. RH")

############
# Offspeed #
############

offspeed <- kyle %>% filter(simple_pitch_type %in% c("CH", "CU", "SL"))

offspeed$count_cat <- factor(ifelse(offspeed$count %in% c("0-2", "1-2", "2-2"), "Ahead", "Not Ahead"),
                             levels = c("Not Ahead", "Ahead"))

offspeed$simple_pitch_type <- factor(offspeed$simple_pitch_type, levels = c("CH", "SL", "CU"))

# Offspeed by count
ggplot(data = offspeed, aes(b_hand, fill = simple_pitch_type)) + 
  facet_grid(strikes~balls) + 
  geom_bar(position = "fill")

table(offspeed$simple_pitch_type[offspeed$balls < 3], 
      ifelse(offspeed$strikes[offspeed$balls < 3] < 2, "Early", "Late"),
      offspeed$b_hand[offspeed$balls < 3]) %>% 
  prop.table(2)


ggplot(data = filter(offspeed, count_cat != "Behind"), aes(px, pz, color = simple_pitch_type)) + 
  facet_grid(count_cat~b_hand_lab) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed() +
  labs(x = "Horizontal Position", y = "Vertical Position", 
       title = "Fastballs - Hitter's Count", color = "Pitch") +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
        strip.text = element_text(family = "Trebuchet MS", color="#666666", size=20))

# ARTICLE
ggplot(data = filter(offspeed),
       aes(count_cat, fill = simple_pitch_type)) + 
  facet_grid(.~b_hand_lab) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values = c("#12496D", "#1F78B4", "#A6CEE3"),
                    labels = c("Changeup", "Slider", "Curveball")) +
  coord_flip() +
  labs(x = "", y = "Frequency",
       title = "Offspeed Pitches", fill = "Pitch") +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
        axis.text.x = element_text(family = "Trebuchet MS", color="#666666", size=15),
        axis.text.y = element_text(family = "Trebuchet MS", color="#666666", size=20),
        strip.text = element_text(family = "Trebuchet MS", color="#666666", size=20))

table(offspeed$count_cat, offspeed$simple_pitch_type, offspeed$b_hand) %>% prop.table(c(1, 3))

table(kyle$simple_pitch_type == "SL", kyle$strikes == 2, kyle$b_hand) %>% prop.table(c(2, 3))

###############
# All Pitches #
###############

kyle$ahead <- ifelse(kyle$strikes >= kyle$balls, "Ahead", "Behind")
kyle$count_cat <- factor(ifelse(kyle$count %in% c("0-2", "1-2", "2-2"), "Ahead", "Not Ahead"),
                             levels = c("Not Ahead", "Ahead"))

kyle$simple_pitch_type <- factor(kyle$simple_pitch_type, levels = c("CH", "CU", "SL", "FT", "FF"))

# ARTICLE
ggplot(data = kyle, aes(ahead, fill = simple_pitch_type)) + 
  geom_bar(position = "fill") + 
  #scale_fill_brewer(palette = "Paired")
  scale_fill_manual(values = c("#12496D", "#1F78B4", "#A6CEE3", "#FB9A99", "#E31A1C"))

# ARTICLE
ggplot(data = filter(kyle, b_hand == "L"), aes(ahead, fill = simple_pitch_type)) + 
  geom_bar(position = "fill") + 
  #scale_fill_brewer(palette = "Paired")
  scale_fill_manual(values = c("#12496D", "#1F78B4", "#A6CEE3", "#FB9A99", "#E31A1C"))

table(kyle$simple_pitch_type,
      ifelse(kyle$strikes >= kyle$balls, "Ahead", "Behind"), 
      kyle$b_hand) %>%
  prop.table(c(2, 3))

table(kyle$simple_pitch_type,
      ifelse(kyle$strikes >= kyle$balls, "Ahead", "Behind")) %>%
  prop.table(2)


ggplot(data = filter(offspeed, count_cat == "Ahead"), aes(px, pz, color = simple_pitch_type)) + 
  #facet_grid(~b_hand) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed(ylim = c(0, 4)) +
  scale_color_manual(values = c("#12496D", "#1F78B4", "#A6CEE3"),
                     labels = c("Changeup", "Slider", "Curveball")) +
  labs(x = "Horizontal Position", y = "Vertical Position", 
       title = "Offspeed - Ahead In Count", color = "Pitch") +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
        strip.text = element_text(family = "Trebuchet MS", color="#666666", size=20))



#############
# Fastballs #
#############

# ARTICLE Fastballs ahead/behind
ggplot(data = filter(kyle, simple_pitch_type %in% c("FF", "FT")), aes(ahead, fill = simple_pitch_type)) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values = c("#FB9A99", "#E31A1C"),
                    labels = c("Two-Seam", "Four-Seam")) +
  labs(x = "", y = "Frequency",
       title = "Fastball Types", fill = "Pitch") +
  scale_x_discrete(labels = c("Ahead in Count", "Behind in Count")) +
  coord_flip() +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
        axis.text.x = element_text(family = "Trebuchet MS", color="#666666", size=15),
        axis.text.y = element_text(family = "Trebuchet MS", color="#666666", size=20))

# ARTICLE Fastballs ahead/behind left
ggplot(data = filter(kyle, simple_pitch_type %in% c("FF", "FT"), b_hand == "L"), aes(ahead, fill = simple_pitch_type)) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values = c("#FB9A99", "#E31A1C"),
                    labels = c("Two-Seam", "Four-Seam")) +
  labs(x = "", y = "Frequency",
       title = "Fastball Types - vs. Lefties", fill = "Pitch") +
  coord_flip() +
  scale_x_discrete(labels = c("Ahead in Count", "Behind in Count")) +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
        axis.text.x = element_text(family = "Trebuchet MS", color="#666666", size=15),
        axis.text.y = element_text(family = "Trebuchet MS", color="#666666", size=20))

# ARTICLE - Two-strike fastballs* location
ggplot(data = filter(kyle, strikes == 2, balls < 3, simple_pitch_type %in% c("FF", "FT")),
       aes(px, pz, color = simple_pitch_type)) +
  facet_grid(.~b_hand_lab) +
  geom_point(size = 3) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed(ylim = c(0, 6), xlim = c(-2.2, 2.2)) +
  scale_color_manual(values = c("#FB9A99", "#E31A1C"), 
                     labels = c("Two-Seam", "Four-Seam")) +
  labs(x = "Horizontal Position", y = "Vertical Position", 
       title = "Fastballs - Pitcher's Count", color = "Pitch") +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
        strip.text = element_text(family = "Trebuchet MS", color="#666666", size=20)
        )

# ARTICLE - Behind fastballs* location
ggplot(data = filter(kyle, balls > strikes, simple_pitch_type %in% c("FF", "FT")),
       aes(px, pz, color = simple_pitch_type)) +
  facet_grid(.~b_hand_lab) +
  geom_point(size = 3) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed(ylim = c(0, 6), xlim = c(-2.2, 2.2)) +
  scale_color_manual(values = c("#FB9A99", "#E31A1C"), 
                     labels = c("Two-Seam", "Four-Seam")) +
  labs(x = "Horizontal Position", y = "Vertical Position", 
       title = "Fastballs - Hitter's Count", color = "Pitch") +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=30, hjust=0),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20),
        strip.text = element_text(family = "Trebuchet MS", color="#666666", size=20))

table(kyle$simple_pitch_type[kyle$simple_pitch_type %in% c("FF", "FT")],
      kyle$strikes[kyle$simple_pitch_type %in% c("FF", "FT")] >= 
        kyle$balls[kyle$simple_pitch_type %in% c("FF", "FT")]) %>%
  prop.table(2)


#############
# Heat maps #
#############
kg <- kyle %>% filter(simple_pitch_type == "FF", b_hand == "L", strikes != 2) %>% select(px, pz, b_hand)

res <- 100
heat.df <- data.frame(x = seq(-3, 3, length.out = res), 
                      y = rep(seq(0, 6, length.out = res), each = res))
# Normal kernel
heat.func <- function(x, y, df.x = kg$px, df.y = kg$pz, st.dev = 1) {
  xsq <- (x - df.x)^2
  ysq <- (y - df.y)^2
  
  dist <- sqrt(xsq + ysq)
  
  weighted <- dnorm(dist, sd = st.dev)
  
  return(sum(weighted))
}

heat.df$heat <- mapply(heat.func, x = heat.df$x, y = heat.df$y, MoreArgs = list(st.dev = .25))

ggplot(data = heat.df, aes(x, y, fill = heat)) + geom_raster(interpolate = T) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "grey50") +
  coord_fixed() +
  scale_fill_gradient(low = "midnightblue", high = "yellow")


#############
# By inning #
#############

table(kyle$simple_pitch_type, kyle$inning, kyle$b_hand) %>% prop.table(c(2, 3))

inning.df <- kyle %>% group_by(inning) %>% 
  summarize(Fastball = sum(simple_pitch_type == "FF")/n(),
            Slider = sum(simple_pitch_type == "SL")/n(),
            Changeup = sum(simple_pitch_type == "CH")/n(),
            TwoSeam = sum(simple_pitch_type == "FT")/n(),
            Curveball = sum(simple_pitch_type == "CU")/n(),
            Total = n()) %>%
  ungroup() %>%
  gather(key = Pitch, value = Frequency, Fastball, Slider, Changeup, TwoSeam, Curveball)

ggplot(data = filter(inning.df, inning <= 7), aes(x = inning, y = Frequency, color = Pitch)) + geom_line() + geom_point()

inning.df <- kyle %>% group_by(gid) %>% 
  mutate(into_seventh = any(inning >= 6)) %>% filter(into_seventh) %>%
  ungroup() %>% group_by(inning) %>% 
  summarize(Fastball = sum(simple_pitch_type == "FF")/n(),
            Slider = sum(simple_pitch_type == "SL")/n(),
            Changeup = sum(simple_pitch_type == "CH")/n(),
            TwoSeam = sum(simple_pitch_type == "FT")/n(),
            Curveball = sum(simple_pitch_type == "CU")/n(),
            Total = n()) %>%
  ungroup() %>%
  gather(key = Pitch, value = Frequency, Fastball, Slider, Changeup, TwoSeam, Curveball)

ggplot(data = filter(inning.df, inning <= 7), aes(x = inning, y = Frequency, color = Pitch)) + 
  geom_line() + geom_point()


#################
# Pitch strings #
#################

# Actual

abid <- paste0(kyle$gid, kyle$ab_num)

k.pitch.adj <- ifelse(kyle$simple_pitch_type == "FT", "TT",
                      ifelse(kyle$simple_pitch_type == "CU", "DD", 
                             as.character(kyle$simple_pitch_type)))
pitches.1 <- k.pitch.adj %>% as.character() %>% substr(1, 1)

seqs <- tapply(pitches.1, abid, paste0, collapse = "", simplify = T) %>%
  as.vector()

ps <- c("F", "S", "C", "D", "T")
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







