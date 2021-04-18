################################################
### tidytuesday 2021 wk 16 - US post offices ###
################################################

# source article 
# https://cblevins.github.io/us-post-offices/


#############
### setup ###
#############

#packages
library(tidytuesdayR)
library(tidyverse)
library(maps)
library(viridis)
library(showtext)
library(gganimate)
library(transformr)

# add font
font_add_google("Chivo", "chivo")
# showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)


# check and load data
# tt_available()
tuesdata <- tt_load('2021-04-13')

post_offices <- tuesdata$post_offices

# have a look
glimpse(post_offices)


########################
### Data Preparation ###
########################

post_offices <- post_offices

# create state and year template
states <- data.frame(state = unique(post_offices$state))
years <- data.frame(year = seq(from = 1639, to = 2002, by = 1))

states <- states %>% mutate(j = 1)
years <- years %>% mutate(j = 1)

# cross join
states_years <- states %>%
  inner_join(years)

# since we're summarising the data and errors look to be infrequent, 
# exact counts (presumably!) don't matter too much, so remove rows with 
# missing data rather than try to correct them

po_growth <- post_offices %>%
  # remove any rows with incorrect established dates
  filter(established >= 1639, established <= 2000) %>%
  # make any incorrect discontinued dates na
  mutate(discontinued = case_when(discontinued > 1639 & discontinued < 2021 ~ discontinued)) %>%
  select(id, state, established, discontinued) %>%
  # unpivot to put dates in one column and get counts
  pivot_longer(cols = c("established", "discontinued"), names_to = c("event"), values_to = "year", values_drop_na = FALSE) %>%
  group_by(state, year, event) %>%
  summarise(offices = n()) %>%
  # pivot and pad out states and dates
  pivot_wider(names_from = event, values_from = offices) %>%
  right_join(states_years) %>%
  select(state, year, established, discontinued) %>%
  # remove any columns with na in year
  filter(!is.na(year)) %>%
  arrange(year) %>%
  # replace na counts with 0
  replace(is.na(.), 0) %>%
  # new post office count columns
  group_by(state) %>%
  mutate(annual_diff = established - discontinued,
         existing_offices = cumsum(annual_diff),
         total_offices_est = cumsum(established),
         total_offices_disc = cumsum(discontinued)) %>%
  # select every decade (10th year)
  filter(year %% 10 == 0) %>%
  # growth compared to previous decade and replace some values with 0
  mutate(growth = (existing_offices - lag(existing_offices))/lag(existing_offices),
         growth = case_when(growth == Inf ~ 0, growth == NaN ~ 0, is.na(growth) ~ 0, TRUE ~ growth))

# Note - this table allows for a variety of further analyses / plots
#        we filter it for what we need in the next step

###########
### Map ###
###########

usa_map <- map_data("state", region = ".")

# update built-in states names table to include DC 
states_inc_dc <- tibble(state = state.name) %>%
  bind_cols(tibble(abb = state.abb)) %>% 
  bind_rows(tibble(state = "District of Columbia", abb = "DC")) %>%
  mutate(state = tolower(state))

usa_map <- usa_map %>%
  left_join(states_inc_dc, by = c("region" = "state"))

# join post office data (one year for now)
po_growth_map_data <- po_growth %>%
  filter(year == 2000) %>%
  inner_join(usa_map, by = c("state" = "abb"))


############
### Plot ###
############

# this page was useful for this part -
# https://www.5haw.com/posts/week-3-making-an-animated-map-using-maps-ggplot2-and-gganimate/
  
growth_map <- ggplot() +
  geom_polygon(data = po_growth_map_data, aes(x = long, y = lat, fill = growth, group = group), colour = "white") +
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
  coord_quickmap() +
  scale_fill_viridis(alpha = 0.7) +
  labs(title = "Net change in post office in each state by decade",
       caption = "Graphic: @pattudor | Data: Blevins, Cameron; Helbock, Richard W., 2021") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    # legend.position = c(0.85, 0.92),
    legend.title = element_blank(),
    legend.text = element_text(family = "chivo", size = 8, colour = "black"),
    plot.margin = margin(10, 10, 10, 10, "mm"),
    plot.background = element_rect(fill = "#e6eaf8"),    
    plot.title = element_text(family = "chivo", hjust = 0, size = 20, colour = "black"),
    plot.caption = element_text(family = "chivo", size = 8, colour = "black")
  )

map_plot <- ggplot() +
  geom_polygon(data = po_growth_map_data, aes(x = long, y = lat, fill = existing_offices, group = group), colour = "black") +
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), color = "white", fill = NA) +
  coord_quickmap() +
  scale_fill_viridis(option = "plasma", alpha = 0.7) +
  labs(title = "Existing post offices in each state - 2000",
       caption = "Graphic: @pattudor | Data: Blevins, Cameron; Helbock, Richard W., 2021") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    # legend.position = c(0.85, 0.92),
    legend.title = element_blank(),
    legend.text = element_text(family = "chivo", size = 5, colour = "white"),
    plot.margin = margin(10, 10, 10, 10, "mm"),
    plot.background = element_rect(fill = "grey20"),    
    plot.title = element_text(family = "chivo", hjust = 0, size = 12, colour = "white"),
    plot.caption = element_text(family = "chivo", size = 4, colour = "white")
  )

map_plot

# fix a location for the year
po_growth_map_data <- po_growth_map_data %>%
  mutate(xloc=-117, yloc=27)

# animate and add year label to animation
map_animated <- map_plot +
  geom_text(data=po_growth_map_data, aes(y=yloc, x=xloc, label=as.character(year)), check_overlap = TRUE, family = "chivo", colour = "white", size=6, fontface="bold") +
  transition_states(year, transition_length = 3, state_length = 20)

map_animated


mapGIF <- animate(map_animated) 

mapGIF

#############
### Notes ###
#############

# 'gnis' - Geographic Names Information System
# https://www.usgs.gov/core-science-systems/ngp/board-on-geographic-names/domestic-names



