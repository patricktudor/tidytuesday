### tt wk 15 - deforestation

# set working directory
setwd("~/GitHub/tidytuesday")

### packages
library(tidytuesdayR)
library(tidyverse)
library(countrycode)
library(showtext)
library(cowplot)
library(magick)

# get font (some inspiration from https://github.com/cnicault/tidytuesday here - cheers!)
font_add_google("Chivo", "chivo")

showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

### source article 
# https://ourworldindata.org/forests-and-deforestation

### load data
tuesdata <- tt_load('2021-04-06')

forest <- tuesdata$forest
forest_area <- tuesdata$forest_area
brazil_loss <- tuesdata$brazil_loss
soybean_use <- tuesdata$soybean_use
vegetable_oil <- tuesdata$vegetable_oil

### add continents

# change datasets from tibble to dataframe
forest <- data.frame(forest)

# add column for continents
forest$continent <- countrycode(sourcevar = forest[, "entity"],
                                origin = "country.name",
                                destination = "continent")

# check country entries by continent and year
yearsum <- forest %>%
  filter(net_forest_conversion != 0) %>%
  group_by(year, continent) %>%
  summarise(Countries = n())

# focus on year = 2015
forest_2015_tmp <- forest %>%
  filter(!is.na(continent),
         year == 2015,
         net_forest_conversion != 0) %>%
  # rank countries by absolute forest conversion for each continent
  group_by(continent) %>%
  mutate(rank_wi_continent = rank(-abs(net_forest_conversion), ties.method = "first")) %>%
  select(continent, entity, year, net_forest_conversion, rank_wi_continent)

# keep top 10 in each continent and bucket the rest together
forest_2015_top <- forest_2015_tmp %>%
  filter(rank_wi_continent <= 10)
  
forest_2015_therest <- forest_2015_tmp %>%
  filter(rank_wi_continent > 10) %>%
  group_by(continent, year) %>%
  summarise(net_forest_conversion = sum(net_forest_conversion)) %>%
  mutate(entity = "Others",
         rank_wi_continent = 11)

forest_2015 <- forest_2015_top %>%
  bind_rows(forest_2015_therest) %>%
  mutate(forest_change = round(abs(net_forest_conversion)/1000, 0),
         Afforestation = case_when(net_forest_conversion > 0 ~ "Afforestation (+)",
                                   net_forest_conversion < 0 ~ "Deforestation (-)"),
         country_kha = paste(entity, forest_change, sep = " ~"))
  

##################
### build plot ###
##################

# this was useful here - 
# https://www.r-graph-gallery.com/297-circular-barplot-with-groups.html

# set an empty bar
empty_bar <- 1

# create dataframe with no values
to_add <- data.frame(matrix(NA, empty_bar * length(unique(forest_2015$continent)), ncol(forest_2015)))
colnames(to_add) <- colnames(forest_2015)
to_add$continent <- rep(unique(forest_2015$continent), each = empty_bar)

# add to main dataframe
forest_2015 <- forest_2015 %>%
  bind_rows(to_add) %>%
  arrange(match(continent, c("Asia", "Oceania", "Africa", "Americas", "Europe")), rank_wi_continent) %>%  # custom order on continents
  rowid_to_column("ID")

# dataframe with labels
label_data <- forest_2015

# calculate angle of labels
number_of_bar <- nrow(label_data)

label_data <- label_data %>%
  mutate(angle = 90 - 360 * ((ID - 0.5) / number_of_bar), # angle of labels. subtract 0.5 to put label in centre of bar
         hjust = if_else(angle < -90, 1, 0), # if on left side then adjust angle
         angle = if_else(angle < -90, angle+180, angle)) # update angles

# prepare data frame for base lines
base_data <- forest_2015 %>% 
  group_by(continent) %>% 
  summarize(start=min(ID), 
            end=max(ID) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# plot
p1 <- ggplot(data = forest_2015, 
            aes(x = ID, 
                y = forest_change,
                fill = Afforestation)) +
  
  # add bars
  geom_bar(stat = "identity", alpha = 0.5) + 
  
  # set colours         
  scale_fill_manual(values = c("#58d54c", "#d72b15")) + 
  
  # negative limit controls size of inner circle
  ylim(-1200,2040) +
  
  # labels
  labs(title = "Net change in forestation during 2015",
      caption = "Graphic: @pattudor | Data: ourworldindata.org") +

  # remove axis elements and grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    #panel.background = element_rect(fill = "whitesmoke"),
    plot.margin = margin(10, 10, 10, 10, "mm"),
    plot.background = element_rect(fill = "#e6eaf8"),
    legend.position = c(0.85, 0.92),
    legend.title = element_blank(),
    legend.text = element_text(family = "chivo", size = 4, colour = "grey40"),
    plot.title = element_text(family = "chivo", hjust = 0, size = 10, lineheight = 3, colour = "grey40"),
    plot.caption = element_text(family = "chivo", size = 3, colour = "grey40")
  ) +
  
  # make coordinate polar instead of cartesian
  coord_polar(start = 0) +
  
  # add labels using label_data
  geom_text(data = label_data, 
            aes(x = ID, 
                y = forest_change + 100, 
                label = country_kha, 
                hjust = hjust
                ),
            colour = "black",
            family = "chivo",
            #fontface = "bold",
            alpha = 0.8,
            size = 0.95,
            angle = label_data$angle,
            inherit.aes = FALSE) +
  
  # add baseline continent info
  geom_segment(data = base_data, 
               aes(x = start, y = -100, xend = end, yend = -100), 
               colour = "grey50", 
               alpha = 0.4, 
               size = 1,
               inherit.aes = FALSE) +
  
  geom_text(data=base_data, 
            aes(x = title, y = -460, label=continent), 
            # hjust=c(1,1,1,0,0), 
            colour = "grey20", 
            alpha=0.7, 
            size=1.05, 
            family="chivo",
            fontface="bold", 
            inherit.aes = FALSE)

# add extra text on top of p1

p2 <- ggdraw() +
  draw_plot(p1) +
  draw_label("Top 10 countries by\nscale of net change\nin each continent displayed.\n\nAll other countries\nwith available data\nare grouped together.\n\nFigures in 1000 hectares.", 
             x = 0.08, y = 0.78, fontfamily = "chivo", hjust = 0, color = "grey50", size = 5, lineheight = 3)

# add image of trees

tree <- image_read('../tidytuesday/images/trees.png')

p3 <- ggdraw(p2) +
  draw_image(
    tree, x = 0.1, y = -0.41,
    width = 0.15
  )

# final plot
# p3

### save the plot

# Open png file
png("NetChangeInForestation2015.png", width = 1000, height = 1000)
# Create the plot
p3
# Close the file
dev.off()






