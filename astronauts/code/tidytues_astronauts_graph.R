# -------Tidy Tuesday 07/14/20-----------
pacman::p_load(tidyverse, colorspace, extrafont, here)

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

# -------exploratory-----------
astro <- astronauts %>%
  mutate(age = year_of_mission - year_of_birth) %>%
  filter(mission_number == 1) %>%
  mutate(days = (hours_mission / 24)) %>%


# --------graph-----------


astro_age_graph <- ggplot(astro) +
  geom_point(aes(
    x = year_of_mission, y = age,
                 color = days, size = days
    )) + 
  scale_color_continuous_sequential(palette = "Sunset") + 
  labs(
    title = "Age of Astronaut on Their First Mission",
    subtitle = "Sizes increased based on days spent in space",
    x = "", y = "",
    caption = "Data from Mariya Stavnichuk and Tatsuya Corlett, used for Tidy Tuesday"
  ) +
  theme(
    plot.title = element_text(color = "white", size = 12, 
                              family = "Verdana"),
    plot.subtitle = element_text(color = "white", size = 10, 
                                 family = "Verdana"),
    plot.caption = element_text(color = "dark grey", 
                                size = 7),
    plot.margin=unit(c(1,1,1,1),"cm"), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#080808"),
    panel.background = element_rect(fill = "#080808"),
    legend.background = element_rect(fill = "#202020"),
    legend.title = element_blank(),
    legend.key = element_rect(fill = "#202020"),
    legend.text = element_text(color = "white"),
    axis.title.y = element_text(color = "white", size = 10),
    axis.title.x = element_text(color = "white", size = 10)
  ) +
  guides(size = F, color = F) +
  geom_curve(
    aes(x = 1968, y = 26.5, xend = 1964, yend = 26),
    arrow = arrow(length = unit(0.03, "npc")), 
    curvature = -.5, color = "#DD7538", linetype = "dotted"
  ) + 
  annotate(
    "text", x = 1970, y = 28,
    label = "Valentina Tereshkova, 
    one of the youngest at 26,
    mission = aprox. 3 days",
    color = "white", size = 3
  ) +
  geom_curve(
    aes(x = 2013, y = 31, xend = 2016, yend = 41.5),
    arrow = arrow(length = unit(0.03, "npc")),
    color = "white", linetype = "dotted"
  ) + 
  annotate(
    "text", x = 2013, y = 30,
    label = "Sergey Ryzhikov, 42,
    spent aprox. 433 days",
    color = "white", size = 3
  )

astro_age_graph

ggsave(here("Dropbox", "projects", "tidy_tuesday", "astronauts",
       "tidytues_astronauts_graph.png"),
       dpi = 300,  width = 11, height = 8)

