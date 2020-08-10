#----------Libraries, Data, Helper functions-----------------
pacman::p_load(tidyverse, wesanderson, here, patchwork)

tuesdata <- tidytuesdayR::tt_load(2020, week = 32)
energy_types <- tuesdata$energy_types 
energy_types <- energy_types %>%
  mutate(country_name = case_when(is.na(country_name) ~ "UK",
                                TRUE ~ country_name)) #UK name is NA


theme_minnew <- function (base_size = 11, base_family = "") {
  theme_classic() %+replace%
    theme(
      panel.spacing = unit(1.5, "lines"),
      plot.margin = unit(c(.5,.5,.5,.5),"cm"),
      #plot.title = element_text(hjust = 0.5)
    )
}
#----------Solar, Wind, Hydro Graph-----------------

# find the top 5 producers of energy among solar, wind and hydro each 
top_5_renewables <- energy_types %>%
  filter(level == "Level 1") %>%
  mutate(total = `2016` + `2017` + `2018`) %>%
  filter(type %in% c("Hydro", "Wind", "Solar")) %>%
  group_by(type) %>%
  slice_max(order_by = total, n = 5, with_ties = FALSE) %>%
  pivot_longer(cols = c(`2016`, `2017`, `2018`), names_to = "Year") 



renewables_plot <- ggplot(top_5_renewables, aes(x = Year, y = value,
                  group = country_name)) +
  geom_line(aes(color = country_name), size = 1) + 
  geom_point(aes(color = country_name), size = 2) + 
  scale_y_continuous(labels = scales::comma) +
  geom_text(data = top_5_renewables %>% filter(Year == "2016"),
            aes(label = country_name), 
            hjust = 1.1, 
            fontface = "bold", 
            size = 2.5) + 
  facet_wrap(~type) +
  theme_minnew() + 
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Pastel1") +
  labs(
    y = "Energy Production", 
    title = "Countries with Highest Renewable Energy Production Between 2016 to 2018",
    subtitle = " Looking at renewable energy production, this graph visualizes changes in energy production \n among the top five countries producing each of the following three energy sources."
    )

# find top 10 countries of highest energy production in 2018

energy_by_country <- energy_types %>%
  filter(level == "Level 1") %>%
  mutate(country_name = case_when(is.na(country_name) ~ "UK",
                                  TRUE ~ country_name)
         ) %>%
  pivot_longer(cols = c(`2016`, `2017`, `2018`), names_to = "Year") %>%
  filter(Year == "2018") %>%
  slice_max(order_by = value, n = 11) # (germany twice, probably better way around this for future me!)




top_10_producers <- energy_types %>%
  pivot_longer(cols = c(`2016`, `2017`, `2018`), names_to = "Year") %>%
  filter(Year == "2018", 
         level == "Level 1") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(Renewable = Hydro + Wind + Solar + Geothermal) %>%
  select(-c(Hydro, Wind, Solar, Geothermal)) %>%
  pivot_longer(cols = c(`Conventional thermal`, Nuclear, Other, Renewable), 
               names_to = "type") %>%
  filter(type != "Other", value != 0) %>%
  group_by(country_name) %>%
  mutate(country_energy = sum(value),
         prop = value / country_energy,
         pct = round(prop*100, 1)) %>%
  filter(country %in% c("FR", "DE", "IT", "UK", "TR",
                        "NO", "PL", "ES", "NL", "UA"))  # filter
 



top_10_2018_plot <- ggplot(top_10_producers, 
                            aes(x=reorder(country_name, country_energy),
                                y = pct,
                                fill = type)) + 
  geom_bar(position = position_stack(), stat = "identity", width = .7) +
  scale_fill_manual(values = wes_palette("Royal1")) +
  theme_minnew() + 
  coord_flip() + 
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.line=element_blank(),
    axis.ticks=element_blank(), 
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 1),
    axis.text.y = element_text(hjust = 1)
    ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    x = "",
    y = "%",
    title = "Energy Production Among European Countries in 2018",
    subtitle = " The countries shown are among the top ten highest producers of energy in 2018 in Europe, from highest to lowest. \n Renewable energy includes the following energy sources: \n hydro, wind, geothermal and solar."
    ) +
  geom_text(
    aes(label = paste0(pct,"%")),
    position = position_stack(vjust = 0.5), 
    size = 2.8, fontface="bold"
  )

energy_both <- top_10_2018_plot / 
  renewables_plot

energy_both <- energy_both + plot_annotation(
  caption = 'Data: Eurostat for Tidy Tuesday | Twwitter: @epenumaka'
  )
  
ggsave(here("energy_plot.png"),
       dpi = 300,  width = 12, height = 14
       )

ggsave(here("energy_plot.pdf"),
       energy_both,
       dpi = 300,  width = 12, height = 14
       )



