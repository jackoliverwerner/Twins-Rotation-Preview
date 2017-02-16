############
# Preamble #
############

library(plyr)
library(dplyr)
library(ggplot2)

setwd("C:/Users/jack.werner1/Documents/BB")
#setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Scraping Files")

# Read data
pitch <- read.csv(file = "pitch_data_2016.csv") #%>% filter(pitcher == 429722)

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

# Pitch results
simplePitchResults <- data.frame(pitch_result = sort(as.character(unique(pitch$pitch_result))),
                                 simple_pitch_result = c("Ball", "Ball", "Ball", "Strike", "Foul",
                                                         "Foul", "Foul", "Foul", "HBP", "InPlay",
                                                         "InPlay", "InPlay", "Ball", "Strike", "Ball", 
                                                         "Strike", "Strike", "Strike"),
                                 stringsAsFactors = F
)

# Player names/IDs
pitcher_names <- read.csv("playerid_list.csv") %>%
  mutate(name = paste0(FIRSTNAME, " ", LASTNAME), id = MLBCODE) %>%
  select(name, id)


######################
# Manipulate dataset #
######################

# Add Simple Event, Simple Pitch Type, Fastball, Player Names
ervin.pre <- pitch %>% filter(pitcher == 429722) %>%
  left_join(simpleResults, by = "event") %>%
  left_join(simplePitches, by = "pitch_type") %>%
  left_join(pitcher_names, by = c("batter" = "id")) %>%
  rename(batter_name = name) %>%
  left_join(pitcher_names, by = c("pitcher" = "id")) %>%
  rename(pitcher_name = name)

# A
ervin <- ervin.pre %>% 
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

table(ervin$simple_pitch_type)

# Get rid of unknowns
ervin <- ervin %>% filter(simple_pitch_type != "UN") %>%
  mutate(simple_pitch_type = as.character(simple_pitch_type))

# Break
ggplot(data = ervin, aes(pfx_x, pfx_z, color = simple_pitch_type)) + geom_point()

# Velocity
ggplot(data = ervin, aes(start_speed)) + facet_grid(simple_pitch_type~.) + geom_histogram()

ggplot(data = ervin, aes(start_speed, fill = simple_pitch_type)) + 
  geom_density(alpha = .5, color = "grey50", size = 1)

ervin <- ervin %>% 
  mutate(simple_pitch_type = ifelse(simple_pitch_type == "FT", "FF", simple_pitch_type))



# Try getting pitch types through clustering

ervin.mat <- ervin %>% select(pfx_x, pfx_z, start_speed) %>% as.matrix() %>% scale()

ervin.km <- kmeans(ervin.mat, centers = 3)

table(ervin.km$cluster, ervin$simple_pitch_type)

ervin$cluster_type <- ifelse(ervin.km$cluster == 1, "FF", ifelse(ervin.km$cluster == 2, "SL", "CH"))

ervin <- ervin %>% mutate(mismatch = simple_pitch_type == cluster_type)

# Look at groups by break
ggplot(data = ervin, aes(pfx_x, pfx_z, color = simple_pitch_type, size = mismatch)) + geom_point() +
  scale_size_manual(values = c(2, 1)) +
  ggtitle("Colored by Pitchf/x")

ggplot(data = ervin, aes(pfx_x, pfx_z, color = cluster_type, size = mismatch)) + geom_point() +
  scale_size_manual(values = c(2, 1)) +
  ggtitle("Colored by Cluster")

ggplot(data = ervin, aes(pfx_x, pfx_z, color = mismatch)) + geom_point() +
  scale_color_manual(values = c("red", "grey70"))


# Look at groups by velocity
ggplot(data = ervin, aes(start_speed, pfx_z, color = simple_pitch_type, size = mismatch)) + geom_point() +
  scale_size_manual(values = c(2, 1)) +
  ggtitle("Colored by Pitchf/x")

ggplot(data = ervin, aes(start_speed, pfx_z, color = cluster_type, size = mismatch)) + geom_point() +
  scale_size_manual(values = c(2, 1)) +
  ggtitle("Colored by Cluster")

ggplot(data = ervin, aes(start_speed, pfx_z, color = mismatch)) + geom_point() +
  scale_color_manual(values = c("red", "grey70"))


# Pitch selection by count and handedness
ervin$simple_pitch_type <- factor(ervin$simple_pitch_type, levels = c("SL", "CH", "FF"))

ggplot(data = ervin, aes(b_hand, fill = simple_pitch_type)) + facet_grid(strikes~balls) + geom_bar(position = "fill")




########################
# Pitch Location Plots #
########################

strike.zone <- data.frame(x = c(17/24, 17/24, -17/24, -17/24, 17/24), y = c(1.5812, 3.4499, 3.4499, 1.5812, 1.5812))

# Strike zone
ggplot(data = filter(ervin, pitch_result %in% c("Ball", "Ball In Dirt", "Called Strike")), 
                     aes(px, pz, color = type)) + 
  geom_point() +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()


# By pitch type
ggplot(data = ervin, aes(px, pz)) + geom_point(color = "red", alpha = .4) +
  facet_wrap(~simple_pitch_type) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

ervin <- ervin %>% mutate(k = simple_event == "K" & last,
                          pitch_ab_res = ifelse(last, simple_event, "Cont."))

# By count, type
ggplot(data = ervin, aes(px, pz, color = simple_pitch_type)) + 
  facet_grid(balls~strikes) +
  geom_point(alpha = .4) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()


##### Individual Counts ######

# 0-2 count by type, result, hand
ggplot(data = filter(ervin, count == "0-2"), aes(px, pz, color = pitch_ab_res)) + 
  facet_grid(b_hand~simple_pitch_type) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed() +
  scale_color_manual(values = c("grey40", "red", "blue", "purple"))

# 1-2 count by type, result, hand
ggplot(data = filter(ervin, count == "1-2"), aes(px, pz, color = pitch_ab_res)) + 
  facet_grid(b_hand~simple_pitch_type) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed() +
  scale_color_manual(values = c("grey40", "orange", "red", "blue", "purple"))

# 2 strikes by count, hand, type
ggplot(data = filter(ervin, strikes == 2), aes(px, pz, color = simple_pitch_type)) + 
  facet_grid(b_hand~balls) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()

# Location by type, result
ggplot(data = ervin, aes(px, pz)) + 
  facet_grid(simple_pitch_type~pitch_ab_res) +
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()


# How did Ervin get strikeouts?
table(ervin$simple_pitch_type[ervin$last & ervin$simple_event == "K"])
table(ervin$count[ervin$last & ervin$simple_event == "K"],
      ervin$simple_pitch_type[ervin$last & ervin$simple_event == "K"])
table(ervin$count[ervin$last & ervin$simple_event == "K"],
      ervin$simple_pitch_type[ervin$last & ervin$simple_event == "K"],
      ervin$b_hand[ervin$last & ervin$simple_event == "K"])


####################
# Pitch sequencing #
####################



ervin %>% filter(pitch_ab_res == "K", pz > 3.25) %>% arrange(desc(pz)) %>% View()


###############################
# Pitches by count/handedness #
###############################

tables <- ervin %>% group_by(count, balls, strikes, b_hand) %>%
  summarize(FF = sum(simple_pitch_type == "FF"),
            SL = sum(simple_pitch_type == "SL"),
            CH = sum(simple_pitch_type == "CH"),
            FF_p = FF/n(), SL_p = SL/n(), CH_p = CH/n(), total = n()) %>%
  ungroup()

tables %>% filter(b_hand == "R") %>% as.data.frame()
tables %>% filter(b_hand == "L") %>% as.data.frame()




