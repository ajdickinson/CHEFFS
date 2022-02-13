# pts %>% 
#   pivot_wider(id_cols = c(team, team_name, opponent), names_from = week, values_from = pts) %>%
#   select(-`0`,-`15`,-`16`,-`17`) %>% 
#   filter(!is.na(opponent)) %>% 
#   group_by(team, team_name) %>% 
#   fill(4:17, .direction = 'downup') %>% View()

# 
# pts_df %>% group_by(team, team_name) %>% 
#   mutate(o_pts = pts$pts[team == opponent])

oft <- pts_df %>% filter(team == 'oft',
                         !is.na(opponent))
# 
# test = pts_df %>% 
#   select(team, week, opponent, pts) %>% 
#   filter(between(week,1,14)) %>% 
#   inner_join(pts %>% filter(between(week,1,14)) %>% select(team, week, pts),
#              by = c('opponent' = 'team', 'week' = 'week')) %>% 
#   mutate(win = ifelse(pts.x > pts.y,1,0)) %>% 
#   group_by(team) %>% 
#     mutate(wins = sum(win)) %>% 
#   ungroup
# 
# opponent = oft$opponent

# sample w/out replacement and repeat weeks 12-14
set.seed(1234)
opponents = pts_df %>% 
  filter(!is.na(opponent)) %>% 
  select(opponent) %>% 
  unique()

# test = expand_grid(weeks = 1:11, home = opponent, away = opponent) %>% filter(home != away)




list = sample_n(opponents, 11, replace = FALSE) %>% as.vector() %>% rename(fake = opponent)
list %<>% add_row(list[1:3,])

oft %>% 
  filter(between(week,1,14)) %>% 
  mutate(list) %>% 
  inner_join(pts %>% filter(between(week,1,14)) %>% select(team, week, pts),
             by = c('fake' = 'team', 'week' = 'week')) %>% 
  mutate(win = ifelse(pts.x > pts.y,1,0)) %>% 
  group_by(team) %>% 
  mutate(wins = sum(win)) %>% 
  ungroup



