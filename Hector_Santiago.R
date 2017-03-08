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

ggplot(data = hector, aes(px, pz)) + geom_point() + facet_wrap(~simple_pitch_type) +
  geom_polygon(data = strike.zone, aes(x = x, y = y, color = NA), fill = NA, color = "black") +
  coord_fixed()











