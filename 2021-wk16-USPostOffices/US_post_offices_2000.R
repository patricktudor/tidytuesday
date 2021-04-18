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

state_counties <- post_offices %>%
  group_by(state, county1) %>%
  summarise(total_rows = n())

# since we're summarising the data and errors look to be infrequent, 
# exact counts (presumably!) don't matter too much, so remove rows with 
# missing data rather than try to correct them

po_2000 <- post_offices %>%
  # find post offices open in 2000
  filter(established < 2000, discontinued > 2000 | is.na(discontinued)) %>%
  group_by(state, county1) %>%
  summarise(offices = n()) %>%
  full_join(state_counties) %>%
  filter(!is.na(county1)) %>%
  replace(is.na(.), 0) %>%
  rename(county = county1) %>%
  mutate(county = tolower(county))
  

###########
### Map ###
###########

usa_states <- map_data("state", region = ".")
usa_counties <- map_data("county", region = ".")

# update built-in states names table to include DC 
states_inc_dc <- tibble(state = state.name) %>%
  bind_cols(tibble(abb = state.abb)) %>% 
  bind_rows(tibble(state = "District of Columbia", abb = "DC")) %>%
  mutate(state = tolower(state))

usa_map <- usa_counties %>%
  left_join(states_inc_dc, by = c("region" = "state"))

usa_map <- usa_map %>%
  inner_join(po_2000, by = c("abb" = "state", "subregion" = "county"))


############
### Plot ###
############

# this page was useful for this part -
# https://www.5haw.com/posts/week-3-making-an-animated-map-using-maps-ggplot2-and-gganimate/

map_plot <- ggplot() +
  geom_polygon(data = usa_map, aes(x = long, y = lat, fill = offices, group = group)) +
  geom_polygon(data = usa_states, aes(x = long, y = lat, group = group), color = "grey80", fill = NA) +
  coord_quickmap() +
  scale_fill_viridis(option = "plasma", alpha = 0.7) +
  labs(title = "Existing post offices by county in 2000",
       caption = "Graphic: @pattudor | Data: Blevins, Cameron; Helbock, Richard W., 2021") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(family = "chivo", size = 10, colour = "#13306dff"),
    plot.margin = margin(10, 10, 10, 10, "mm"),
    plot.background = element_rect(fill = "grey80"),    
    plot.title = element_text(family = "chivo", size = 25, colour = "#13306dff"),
    plot.caption = element_text(family = "chivo", size = 9, colour = "#13306dff")
  )

map_plot

# add text for counties with most offices
final_plot <- map_plot +
  annotate("text", x = -124, y = 30, label = "Top 5 counties", hjust = 0, family = "chivo", size = 6, colour = "#13306dff") +
  annotate("text", x = -124, y = 28.8, label = "1. Suffolk, NY", hjust = 0, family = "chivo", size = 4, colour = "#13306dff") +
  annotate("text", x = -124, y = 27.9, label = "2. Los Angeles, CA", hjust = 0, family = "chivo", size = 4, colour = "#13306dff") +
  annotate("text", x = -124, y = 27.0, label = "3. Worcester, MA", hjust = 0, family = "chivo", size = 4, colour = "#13306dff") +
  annotate("text", x = -124, y = 26.1, label = "4. Westmorland, PA", hjust = 0, family = "chivo", size = 4, colour = "#13306dff") +
  annotate("text", x = -124, y = 25.2, label = "5. San Bernardino, CA", hjust = 0, family = "chivo", size = 4, colour = "#13306dff") +
  annotate("text", x = -111, y = 28.8, label = "100", hjust = 1, family = "chivo", size = 4, colour = "#13306dff") +
  annotate("text", x = -111, y = 27.9, label = "86", hjust = 1, family = "chivo", size = 4, colour = "#13306dff") +
  annotate("text", x = -111, y = 27.0, label = "85", hjust = 1, family = "chivo", size = 4, colour = "#13306dff") +
  annotate("text", x = -111, y = 26.1, label = "82", hjust = 1, family = "chivo", size = 4, colour = "#13306dff") +
  annotate("text", x = -111, y = 25.2, label = "69", hjust = 1, family = "chivo", size = 4, colour = "#13306dff")

final_plot

# save the plot
png("PostOffices2000.png", width = 1000, height = 1000) # Open png file
final_plot # Create the plot
dev.off() # Close the file


#############
### Notes ###
#############

# 'gnis' - Geographic Names Information System
# https://www.usgs.gov/core-science-systems/ngp/board-on-geographic-names/domestic-names



