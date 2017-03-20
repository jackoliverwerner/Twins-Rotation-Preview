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
jose.pre <- pitch %>% filter(pitcher == 621244) %>%
  left_join(simpleResults, by = "event") %>%
  left_join(simplePitches, by = "pitch_type") %>%
  left_join(pitcher_names, by = c("batter" = "id")) %>%
  rename(batter_name = name) %>%
  left_join(pitcher_names, by = c("pitcher" = "id")) %>%
  rename(pitcher_name = name)

# A
jose <- jose.pre %>% 
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

table(jose$simple_pitch_type)

# Get rid of unknowns
jose <- jose %>% filter(simple_pitch_type != "UN" & simple_pitch_type != "PO") %>%
  mutate(simple_pitch_type = as.character(simple_pitch_type))

# Break
ggplot(data = jose, aes(pfx_x, pfx_z, color = simple_pitch_type)) + geom_point()

# Velocity
ggplot(data = jose, aes(start_speed)) + facet_grid(simple_pitch_type~.) + geom_histogram()

ggplot(data = jose, aes(start_speed, fill = simple_pitch_type)) + 
  geom_density(alpha = .5, color = "grey50", size = 1)

jose$simple_pitch_type = factor(jose$simple_pitch_type, levels = c("CU", "CH", "FT", "FF"))

ggplot(data = jose, aes(b_hand, fill = simple_pitch_type)) + facet_grid(strikes~balls) + 
  geom_bar(position = "fill")


ggplot(data = jose, aes(b_hand, fill = simple_pitch_type)) + 
  geom_bar(position = "fill")


###########
# By date #
###########

gidToDate <- function(g) {
  gc <- as.character(g) %>%
    strsplit("_")
  
  date.char <- gc[[1]][2:4] %>% paste0(collapse = "")
  
  return(date.char)
}

jose$date <- sapply(jose$gid, gidToDate)
jose$date <- ymd(jose$date)

ggplot(data = jose, aes(date, fill = simple_pitch_type)) + geom_bar(position = "fill")





