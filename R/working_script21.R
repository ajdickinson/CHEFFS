options(scipen = 999)             # Modify global options in R
pacman::p_load(tidyverse, ggplot2, hrbrthemes, tidyr, plotly)
source("~/Documents/scripts/colors/colors.R")

pts_df <- read_csv('./data/pts.csv') %>% select(team, week, pts, opponent) %>% 
  add_row(team = 'mccorkle_mania', week = 0, opponent = NA, pts = 0) %>% 
  add_row(team = 'philadelphia_collins', week = 0, opponent = NA, pts = 0) %>% 
  add_row(team = 'pray_4_ek', week = 0, opponent = NA, pts = 0) %>% 
  add_row(team = 'oft', week = 0, opponent = NA, pts = 0) %>% 
  add_row(team = 'scisneros', week = 0, opponent = NA, pts = 0) %>% 
  add_row(team = 'urban_strokes', week = 0, opponent = NA, pts = 0) %>% 
  add_row(team = 'younghoe_kyu', week = 0, opponent = NA, pts = 0) %>% 
  add_row(team = 'utility_curve', week = 0, opponent = NA, pts = 0) %>% 
  add_row(team = 'hail_murray', week = 0, opponent = NA, pts = 0) %>% 
  add_row(team = 'cursed_by_godwin', week = 0, opponent = NA, pts = 0) %>% 
  add_row(team = 'shits_kittles', week = 0, opponent = NA, pts = 0) %>% 
  add_row(team = 'fuck_oft', week = 0, opponent = NA, pts = 0) %>% 
  arrange(team, week)

pts = pts_df %>% 
  group_by(team) %>% 
    mutate(cum_pts = cumsum(coalesce(pts, 0)) + pts*0,
           ave_cum_pts = cum_pts/week,
           total_pts = sum(pts, na.rm = TRUE)) %>% 
    fill(cum_pts, ave_cum_pts, .direction = 'down') %>% 
  ungroup() %>% 
  group_by(week) %>% 
    mutate(league_ave_cum_pts = sum(cum_pts)/12,
           z_cum_pts = round(cum_pts - league_ave_cum_pts, digits = 2)) %>% 
  ungroup() %>% 
  mutate(playoff = ifelse(week > 14, 1, 0), 
         team_name = case_when(
           team == 'mccorkle_mania' ~ 'Mccorkle Mania', 
           team == 'philadelphia_collins' ~ 'Philadelphia Collins',
           team == 'pray_4_ek' ~ 'Pray 4 Ek',
           team == 'oft' ~ 'Oregon Football Team',
           team == 'scisneros' ~ 'Scisneros',
           team == 'urban_strokes' ~ 'Urban Strokes Crotch',
           team == 'younghoe_kyu' ~ 'Gay for Younghoe Kyu',
           team == 'utility_curve' ~ 'Utility Curve',
           team == 'hail_murray' ~ 'Hail Murray',
           team == 'cursed_by_godwin' ~ 'Cursed by God(win)',
           team == 'shits_kittles' ~ 'Shits and Kittles',
           team == 'fuck_oft' ~ 'Fuck the Oregon Football Team')
         )

oft = pts %>% 
  filter(team == 'oft',
         week > 0) %>% 
  select(week, pts, opponent) %>% 
  mutate(win = ifelse())


pts$team_name <- factor(pts$team_name, levels = c('Mccorkle Mania', 'Philadelphia Collins', 'Pray 4 Ek', 'Oregon Football Team', 'Scisneros',
                                          'Urban Strokes Crotch', 'Gay for Younghoe Kyu', 'Utility Curve', 'Hail Murray', 'Cursed by God(win)',
                                          'Shits and Kittles', 'Fuck the Oregon Football Team'))

ggplot(pts, aes(x = week, y = cum_pts, group = team_name, color = team_name )) +
  geom_point(size = 1.5) +
  geom_line(size = 1.5) +
  theme_ipsum() +
  theme(
    panel.grid.minor = element_blank()
  ) +
  scale_color_viridis_d(option = 'magma', end = 0.95)

playoff_start = 14.25

pts$team_name <- factor(pts$team_name, levels = c('Mccorkle Mania', 'Philadelphia Collins', 'Pray 4 Ek', 'Oregon Football Team', 'Scisneros',
                                          'Urban Strokes Crotch', 'Gay for Younghoe Kyu', 'Utility Curve', 'Hail Murray', 'Cursed by God(win)',
                                          'Shits and Kittles', 'Fuck the Oregon Football Team'))



cum_plot <- ggplot(pts %>% filter(playoff == 0), aes(x = week, y = z_cum_pts, group = team_name, color = team_name)) +
  # geom_rect(ymin = -500, ymax = 500, xmin = playoff_start, xmax = 17, alpha = 0.03, color = "white", fill = "grey90") +
  # geom_vline(xintercept = playoff_start, linetype = 'dashed', size = 0.5) +
  # geom_point(size = 2.5) +
  geom_line(size = 1.5, alpha = 0.85) +
  labs(
    title = "Cumulative points by team",
    subtitle = 'Demeaned week-by-league average',
    x = "Week",
    y = "Cumulative points (demeaned)",
    color = 'Team name'
    ) +
  theme_ipsum() +
  theme(
    panel.grid.minor = element_blank()
  ) +
  # scale_color_viridis_d(option = 'magma', end = 0.95) +
  # scale_color_discrete(type = grant_pal) +
  scale_color_viridis_d(option = 'inferno', begin = 0.1, end = 0.9) +
  scale_x_continuous(breaks = c(0, 7, 14)) +
  scale_y_continuous(breaks = seq(-300, 300, 150))

cum_plot %>% plotly::ggplotly()
