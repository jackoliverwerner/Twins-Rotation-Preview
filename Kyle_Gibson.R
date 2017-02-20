############
# Preamble #
############

library(plyr)
library(dplyr)
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


# Try getting pitch types through clustering

kyle.mat <- kyle %>% select(pfx_x, pfx_z, start_speed) %>% as.matrix() %>% scale()

kyle$cluster <- kmeans(kyle.mat, centers = 5)$cluster

(clust.tab <- table(kyle$cluster, kyle$simple_pitch_type))

conv.df <- data.frame(cluster = as.numeric(as.character(rownames(clust.tab))), 
                      cluster_type = colnames(clust.tab)[apply(clust.tab, 1, which.max)])

kyle <- kyle %>% left_join(conv.df, by = "cluster")

kyle <- kyle %>% mutate(mismatch = simple_pitch_type == cluster_type)

# Look at groups by break
ggplot(data = kyle, aes(pfx_x, pfx_z, color = simple_pitch_type, size = mismatch)) + geom_point() +
  scale_size_manual(values = c(2, 1)) +
  ggtitle("Colored by Pitchf/x")

ggplot(data = kyle, aes(pfx_x, pfx_z, color = cluster_type, size = mismatch)) + geom_point() +
  scale_size_manual(values = c(2, 1)) +
  ggtitle("Colored by Cluster")

ggplot(data = kyle, aes(pfx_x, pfx_z, color = mismatch)) + geom_point() +
  scale_color_manual(values = c("red", "grey70"))


# Look at groups by velocity
ggplot(data = kyle, aes(start_speed, pfx_z, color = simple_pitch_type, size = mismatch)) + geom_point() +
  scale_size_manual(values = c(2, 1)) +
  ggtitle("Colored by Pitchf/x")

ggplot(data = kyle, aes(start_speed, pfx_z, color = cluster_type, size = mismatch)) + geom_point() +
  scale_size_manual(values = c(2, 1)) +
  ggtitle("Colored by Cluster")

ggplot(data = kyle, aes(start_speed, pfx_z, color = mismatch)) + geom_point() +
  scale_color_manual(values = c("red", "grey70"))

# 3d Plot
colors <- ifelse(kyle$simple_pitch_type == "FF", "red",
                 ifelse(kyle$simple_pitch_type == "SL", "green", "blue"))

plot3d(kyle$px, kyle$pz, kyle$start_speed, col = colors,
       xlab = "x", ylab = "z", zlab = "Velocity")

###############################
# Pitches by count/handedness #
###############################

table(kyle$b_hand, kyle$simple_pitch_type) %>% prop.table(1)

tables <- kyle %>% group_by(count, balls, strikes, b_hand) %>%
  summarize(FF = sum(simple_pitch_type == "FF"),
            FT = sum(simple_pitch_type == "FT"),
            SL = sum(simple_pitch_type == "SL"),
            CH = sum(simple_pitch_type == "CH"),
            CU = sum(simple_pitch_type == "CU"),
            FF_p = FF/n(), FT_p = FT/n(), SL_p = SL/n(), 
            CH_p = CH/n(), CU_p = CU/n(),  total = n()) %>%
  ungroup()

tables %>% filter(b_hand == "R") %>% as.data.frame()
tables %>% filter(b_hand == "L") %>% as.data.frame()

kyle$simple_pitch_type <- factor(kyle$simple_pitch_type, levels = c("CU", "CH", "SL", "FT", "FF"))

ggplot(data = kyle, aes(b_hand, fill = simple_pitch_type)) + facet_grid(strikes~balls) + 
  geom_bar(position = "fill") + scale_fill_manual(values = c("yellow", "orange", "red", "lightblue", "darkblue"))


ggplot(data = filter(kyle, b_hand == "R"), aes(b_hand, fill = simple_pitch_type)) + facet_grid(strikes~balls) + 
  geom_bar(position = "fill") + scale_fill_manual(values = c("yellow", "orange", "red", "lightblue", "darkblue"))


########################
# Pitch Location Plots #
########################

strike.zone <- data.frame(x = c(17/24, 17/24, -17/24, -17/24, 17/24), y = c(1.5812, 3.4499, 3.4499, 1.5812, 1.5812))

# Strike zone
ggplot(data = filter(kyle, pitch_result %in% c("Ball", "Ball In Dirt", "Called Strike")), 
       aes(px, pz, color = type)) + 
  geom_point() +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()


# By pitch type MAYBE ARTICLE
ggplot(data = kyle, aes(px, pz)) + geom_point(color = "red", alpha = .4) +
  facet_grid(b_hand~simple_pitch_type) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

kyle <- kyle %>% mutate(k = simple_event == "K" & last,
                          pitch_ab_res = ifelse(last, simple_event, "Cont."))

# By count, type
ggplot(data = kyle, aes(px, pz, color = simple_pitch_type)) + 
  facet_grid(balls~strikes) +
  geom_point(alpha = .4) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()


##### Individual Counts ######

# 0-2 count by type, hand
ggplot(data = filter(kyle, count == "0-2"), aes(px, pz, color = simple_pitch_type)) + 
  facet_wrap(~b_hand) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

# 0-2 count by type, result, hand
ggplot(data = filter(kyle, count == "0-2"), aes(px, pz, color = pitch_ab_res)) + 
  facet_grid(b_hand~simple_pitch_type) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

(tab.02 <- table(kyle$simple_pitch_type[kyle$count == "0-2"], kyle$b_hand[kyle$count == "0-2"]))
prop.table(tab.02, 2)


# Fastballs up
kyle %>% filter(simple_pitch_type == "FF", count == "0-2", b_hand == "L") %>% View()

a <- kyle.seq %>% filter(prev_count %in% c("0-2", "1-2"), prev_pitch == "FF", b_hand == "L")

ggplot(data = a, aes(x = px, y = pz, color = simple_pitch_type)) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()


# 1-2 count by type, hand
ggplot(data = filter(kyle, count == "1-2"), aes(px, pz, color = simple_pitch_type)) + 
  facet_wrap(~b_hand) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

# 1-2 count by type, result, hand
ggplot(data = filter(kyle, count == "1-2"), aes(px, pz, color = pitch_ab_res)) + 
  facet_grid(b_hand~simple_pitch_type) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

(tab.12 <- table(kyle$simple_pitch_type[kyle$count == "1-2"], kyle$b_hand[kyle$count == "1-2"]))
prop.table(tab.12, 2)



# 2 strikes by count, hand, type
ggplot(data = filter(kyle, strikes == 2), aes(px, pz, color = simple_pitch_type)) + 
  facet_grid(b_hand~balls) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

# Location by type, result
ggplot(data = kyle, aes(px, pz)) + 
  facet_grid(simple_pitch_type~pitch_ab_res) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()


# How did kyle get strikeouts?
table(kyle$simple_pitch_type[kyle$strikes == 2])/sum(kyle$strikes == 2)
table(kyle$simple_pitch_type[kyle$last & kyle$simple_event == "K"])/sum(kyle$last & kyle$simple_event == "K")


table(kyle$simple_pitch_type[kyle$last & kyle$simple_event == "K"])
table(kyle$count[kyle$last & kyle$simple_event == "K"],
      kyle$simple_pitch_type[kyle$last & kyle$simple_event == "K"])
table(kyle$count[kyle$last & kyle$simple_event == "K"],
      kyle$simple_pitch_type[kyle$last & kyle$simple_event == "K"],
      kyle$b_hand[kyle$last & kyle$simple_event == "K"])


####################
# Pitch sequencing #
####################

kyle.seq <- kyle %>% group_by(gid, ab_num) %>%
  mutate(prev_count = lag(count, 1, default = "None"),
         prev_pitch = lag(as.character(simple_pitch_type), 1, default = "None"),
         back_2 = lag(as.character(simple_pitch_type), 2, default = "None"),
         next_pitch = lead(as.character(simple_pitch_type), 1, default = "None"),
         pitch_num = 1:n()) %>%
  ungroup()


kyle.seq %>% select(pitch_result, prev_pitch, simple_pitch_type, next_pitch) %>% View()


table(kyle.seq$prev_pitch, kyle.seq$simple_pitch_type) %>% prop.table(1)


##### Individual Counts #####

# 0-2 count
ggplot(data = filter(kyle.seq, count == "0-2"), aes(px, pz, color = simple_pitch_type)) + 
  facet_grid(prev_count~prev_pitch) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

table(kyle.seq$prev_pitch[kyle$count == "0-2"],
      kyle.seq$simple_pitch_type[kyle$count == "0-2"]) %>%
  prop.table(1)

table(kyle.seq$prev_pitch[kyle$count == "0-2"],
      kyle.seq$simple_pitch_type[kyle$count == "0-2"],
      kyle.seq$b_hand[kyle$count == "0-2"]) %>%
  prop.table(c(1, 3))

ggplot(data = filter(kyle.seq, count == "0-2"), aes(x = b_hand, fill = simple_pitch_type)) +
  facet_grid(prev_count~prev_pitch) + geom_bar(position = "fill")


# 1-2 count
ggplot(data = filter(kyle.seq, count == "1-2"), aes(px, pz, color = simple_pitch_type)) + 
  facet_grid(prev_count~prev_pitch) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

table(kyle.seq$prev_pitch[kyle$count == "1-2"],
      kyle.seq$simple_pitch_type[kyle$count == "1-2"]) %>%
  prop.table(1)

table(kyle.seq$prev_pitch[kyle$count == "1-2"],
      kyle.seq$simple_pitch_type[kyle$count == "1-2"],
      kyle.seq$b_hand[kyle$count == "1-2"]) %>%
  prop.table(c(1, 3))

ggplot(data = filter(kyle.seq, count == "1-2"), aes(x = b_hand, fill = simple_pitch_type)) +
  facet_grid(prev_count~prev_pitch) + geom_bar(position = "fill")


# Two-strike fouls
table(kyle.seq$prev_pitch[kyle.seq$count == kyle.seq$prev_count & kyle.seq$b_hand == "R"],
      kyle.seq$simple_pitch_type[kyle.seq$count == kyle.seq$prev_count & kyle.seq$b_hand == "R"])


# 2-2 count
ggplot(data = filter(kyle.seq, count == "2-2"), aes(px, pz, color = simple_pitch_type)) + 
  facet_grid(prev_count~prev_pitch) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

table(kyle.seq$prev_pitch[kyle.seq$count == "2-2"],
      kyle.seq$simple_pitch_type[kyle.seq$count == "2-2"]) %>%
  prop.table(1)

table(kyle.seq$prev_pitch[kyle$count == "2-2"],
      kyle.seq$simple_pitch_type[kyle$count == "2-2"],
      kyle.seq$b_hand[kyle$count == "2-2"]) %>%
  prop.table(c(1, 3))

ggplot(data = filter(kyle.seq, count == "2-2"), aes(x = b_hand, fill = simple_pitch_type)) +
  facet_grid(prev_count~prev_pitch) + geom_bar(position = "fill")





# 3-2 count
ggplot(data = filter(kyle.seq, count == "3-2"), aes(px, pz, color = simple_pitch_type)) + 
  facet_grid(prev_count~prev_pitch) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

table(kyle.seq$prev_pitch[kyle$count == "3-2"],
      kyle.seq$simple_pitch_type[kyle$count == "3-2"]) %>%
  prop.table(1)

table(kyle.seq$prev_pitch[kyle$count == "3-2"],
      kyle.seq$simple_pitch_type[kyle$count == "3-2"],
      kyle.seq$b_hand[kyle$count == "3-2"]) %>%
  prop.table(c(1, 3))

ggplot(data = filter(kyle.seq, count == "3-2"), aes(x = b_hand, fill = simple_pitch_type)) +
  facet_grid(prev_count~prev_pitch) + geom_bar(position = "fill")



# Second pitch
ggplot(data = filter(kyle.seq, count %in% c("1-0", "0-1")), aes(x = b_hand, fill = simple_pitch_type)) +
  facet_grid(count~prev_pitch) + geom_bar(position = "fill")


#############
# By inning #
#############

table(kyle$simple_pitch_type, kyle$inning, kyle$b_hand) %>% prop.table(c(2, 3))

inning.df <- kyle %>% group_by(inning) %>% 
  summarize(Fastball = sum(simple_pitch_type == "FF")/n(),
            Slider = sum(simple_pitch_type == "SL")/n(),
            Changeup = sum(simple_pitch_type == "CH")/n(),
            Total = n()) %>%
  ungroup() %>%
  gather(key = Pitch, value = Frequency, Fastball, Slider, Changeup)

ggplot(data = filter(inning.df, inning <= 7), aes(x = inning, y = Frequency, color = Pitch)) + geom_line() + geom_point()

inning.df <- kyle %>% group_by(gid) %>% 
  mutate(into_seventh = any(inning >= 6)) %>% filter(into_seventh) %>%
  ungroup() %>% group_by(inning) %>% 
  summarize(Fastball = sum(simple_pitch_type == "FF")/n(),
            Slider = sum(simple_pitch_type == "SL")/n(),
            Changeup = sum(simple_pitch_type == "CH")/n(),
            Total = n()) %>%
  ungroup() %>%
  gather(key = Pitch, value = Frequency, Fastball, Slider, Changeup)

ggplot(data = filter(inning.df, inning <= 7), aes(x = inning, y = Frequency, color = Pitch)) + 
  geom_line() + geom_point()


##################
# By baserunners #
##################

kyle_b <- kyle %>% mutate(baserunner = !is.na(runner_1) | !is.na(runner_2) | !is.na(runner_3),
                            on_third = !is.na(runner_3),
                            on_second = !is.na(runner_2))

table(kyle_b$baserunner, kyle_b$simple_pitch_type) %>% prop.table(1)
table(kyle_b$on_third, kyle_b$simple_pitch_type) %>% prop.table(1)
table(kyle_b$on_second, kyle_b$simple_pitch_type) %>% prop.table(1)

#################
# Pitch strings #
#################

# Actual

abid <- paste0(kyle$gid, kyle$ab_num)
pitches.1 <- kyle$simple_pitch_type %>% as.character() %>% substr(1, 1)

seqs <- tapply(pitches.1, abid, paste0, collapse = "", simplify = T) %>%
  as.vector()

ps <- c("F", "S", "C")
all.duos <- paste0(rep(ps, each = 3), ps)
all.trios <- paste0(rep(all.duos, each = 3), ps)


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
  pitches.1.r <- kyle$simple_pitch_type %>% as.character() %>% substr(1, 1) %>% sample()
  
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

duos.df <- seqs.df[1:9,]
trios.df <- seqs.df[10:nrow(seqs.df),]

ggplot(data = duos.df, aes(x = exp, y = freq, label = pattern)) + 
  geom_text() + 
  geom_abline(slope = 1, intercept = 0, color = "red")

ggplot(data = trios.df, aes(x = exp, y = freq, label = pattern)) + 
  geom_text() + 
  geom_abline(slope = 1, intercept = 0, color = "red")

ggplot(data = filter(duos.df, exp > 100), aes(x = reorder(pattern, -p_diff), y = p_diff)) + 
  geom_bar(stat = "identity", fill = twins_blue, color = twins_gold, size = 1) + theme_minimal()
ggplot(data = filter(trios.df, exp > 100), aes(x = reorder(pattern, -p_diff), y = p_diff)) + geom_bar(stat = "identity")







