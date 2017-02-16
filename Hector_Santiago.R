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


hector <- hector %>% 
  mutate(simple_pitch_type = ifelse(simple_pitch_type == "FC", "FF", simple_pitch_type))


#### Continue from here ####


#############################
# Check out pitch locations #
#############################

strike.zone <- data.frame(x = c(17/24, 17/24, -17/24, -17/24, 17/24), y = c(1.5812, 3.4499, 3.4499, 1.5812, 1.5812))

ggplot(data = filter(hector, pitch_result %in% c("Ball", "Ball In Dirt", "Called Strike")), 
       aes(px, pz, color = type)) + 
  geom_point() +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()



ggplot(data = hector, aes(px, pz, color = simple_pitch_type)) + geom_point()

hector <- hector %>% mutate(k = simple_event == "K" & last)

ggplot(data = filter(hector, count == "0-2"), aes(px, pz, color = simple_pitch_type, shape = k)) + 
  geom_point(size = 2) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA, shape = NA), fill = NA, color = "black") +
  coord_fixed()


ggplot(data = hector, aes(px, pz, color = simple_pitch_type, shape = k)) + 
  geom_point() +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()


###############################
# Pitches by count/handedness #
###############################

tables <- hector %>% group_by(count, balls, strikes, b_hand) %>%
  summarize(FF = sum(simple_pitch_type == "FF"),
            SL = sum(simple_pitch_type == "SL"),
            CH = sum(simple_pitch_type == "CH"),
            FF_p = FF/n(), SL_p = SL/n(), CH_p = CH/n(), total = n()) %>%
  ungroup()

tables %>% filter(b_hand == "R") %>% as.data.frame()








