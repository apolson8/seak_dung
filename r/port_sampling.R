# A.Olson 
# Objective: Summarize and visualize port sampling data for GKC in SE Alaska 

source("./r/helper.R")

# global ---------
cur_yr = 2022
year <- 2022 # most recent year of data
fig_path <- paste0('figures/', year) # folder to hold all figs for a given year
dir.create(fig_path) # creates YEAR subdirectory inside figures folder
output_path <- paste0('output/', year) # output and results
dir.create(output_path) 

#Import data
dung_port <- read.csv("data/fishery/dung_port_sampling 2015-2020.csv") %>%
  clean_names() -> dung_port

read.csv("data/fishery/dung_harv_season.csv") %>%
  clean_names() -> dung_fish

dung_fish %>%
  ggplot(aes(year, lb)) +
  geom_col() +
  ylab("Harvest (lbs)") +
  xlab("Season") +
  scale_y_continuous(label = scales::comma, 
                     breaks = scales::pretty_breaks(n = 10)) + 
  scale_x_continuous(breaks = seq(0, 2020, 2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(paste0(fig_path, '/dung_fishery_harvest_season.png'), width = 9, height = 6, units = "in", dpi = 400)
  

dung_port %>%
  filter(season_ref == "20-21") %>%
  ggplot(aes(width_millimeters,
             fill = recruit_status,
             color = recruit_status)) +
  geom_histogram(alpha = 0.8,
                 bins = 30) +
  geom_vline(xintercept = 165,
              linetype = "dashed") +
  facet_wrap(~i_fishery) 
  #scale_fill_viridis_d() 
  #scale_color_viridis_d()


dung_port %>%
  ggplot(aes(y = season_ref)) +
  geom_density_ridges(
    aes(x = width_millimeters,
        fill = paste(season_ref, recruit_status)),
    alpha = 0.7, color = "white" 
  )

### Sample size ---------
# adds sample size as a column by year and mgt_area
dung_port %>% 
  group_by(season) %>% 
  summarise(count = n()) -> sample_size

port_summary %>% 
  left_join(sample_size)-> port_summary


# deal with small sample size in 2014 for ICY and graphing issues. 
port_summary2 %>% 
  filter(mgt_area == "Icy Strait",
         year == 2014) %>%
  filter(recruit_status == "Recruit") %>% 
  mutate(TICKET_NO = 12345) -> fill_icy


port_summary2 %>%
  bind_rows(fill_icy) %>% 
  filter(mgt_area == "Icy Strait",
         YEAR > 1999) %>%
  mutate(YEAR = fct_rev(as.factor(year))) %>%
  ggplot(aes(y = year)) + 
  geom_density_ridges(
    aes(x = length_millimeters, fill = paste(year, recruit_status)), 
    alpha = 0.7, color = "white"
  ) + 
  scale_fill_cyclical(
    breaks = c("2000 Recruit", "2000 Post-Recruit"),
    labels = c(`2000 Recruit`= "Recruit", `2000 Post-Recruit` = "Post Recruit"),
    values = c("dark blue", "dark orange", "blue", "orange"),
    name = "Recruit_status", guide = "legend"
  ) + 
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(breaks = seq(0, 200, 10), limits = c(150, 200)) +
  geom_text(aes(x = 188, y = year, label = count), position = position_nudge(y = 0.3)) + # comment this out to remove sample size
  ylab("Year") +
  xlab("Carapace Length (mm)") +
  #facet_wrap(~mgt_area, scales = "free_y") +
  theme(strip.background = element_blank(),
        legend.position = c(0.9, 0.9))

ggsave(paste0(fig_path, '/gkc_icystrait_lengths.png'), width = 7, height = 8, units = "in", dpi = 200)


# Histogram -----------

port_summary %>%
  filter(mgt_area == "East Central",
         year > 1999) %>%
 ggplot(aes(length_millimeters)) + 
 geom_histogram() +
facet_wrap(~year) +
  ggtitle("East Central GKC CL Frequencies")


port_summary %>%
  filter(mgt_area == "East Central",
         YEAR > 2004) %>%
  mutate(YEAR = fct_rev(as.factor(year))) %>%
  ggplot(aes(length_millimeters, color = year)) + 
  geom_freqpoly(size = 1.2) +
  ggtitle("East Central GKC CL Frequencies")


# Stacked Bar Chart -----------
port_summary %>%
  filter(mgt_area == "East Central",
         year > 1999) %>%
  ggplot(aes(year)) +
  geom_bar(aes(fill = recruit_status)) +
  scale_y_continuous(breaks = seq(0, 3000, 100), labels = scales::comma) +
  ylab("Count") +
  xlab("Year") +
  ggtitle("East Central") +
  theme(legend.position = c(0.8, 0.8))
  

port_summary %>%
  filter(mgt_area == "East Central",
         year > 1999) %>% 
  group_by(year) %>%
  count(recruit_status) %>%
  ggplot(aes(year, n, color = recruit_status)) +
  geom_line() + 
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(0, 3000, 100), labels = scales::comma) +
  ylab("Count") +
  xlab("Year") +
  ggtitle("East Central") +
  theme(legend.position = c(0.8, 0.8))



port_summary2 %>% select(year, length_millimeters) %>%
  group_by(length_millimeters) %>% tally()

