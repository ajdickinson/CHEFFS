set.seed(12)

wins_sim = function(data, X, Y, Z, iter = 1) {
  
  team_df = data %>% 
    filter((!! rlang::sym(X)) == Z) 
  
  opponents = data %>%
    filter(!is.na(opponent)) %>% 
    select(opponent) %>% 
    unique() %>% 
    filter((!! rlang::sym(Y)) != Z)
  
  list = sample_n(opponents, 11, replace = FALSE) %>% 
    as.vector() %>% 
    rename(fake = opponent)
  
  list %<>% add_row(list[1:3,])
  
  team_df %>% 
    filter(between(week,1,14)) %>% 
    mutate(list) %>% 
    inner_join(pts %>% filter(between(week,1,14)) %>% select(team, week, pts),
               by = c('fake' = 'team', 'week' = 'week')) %>% 
    mutate(win = ifelse(pts.x > pts.y,1,0),
           iter = iter) %>% 
    group_by(team) %>% 
    mutate(wins = sum(win)) %>% 
    ungroup
  
}

wins_sim(pts_df, X = 'team', Y = 'opponent', Z = 'oft')


season_sim = function(iter = 1, data = pts_df, X = 'team', Y = 'opponent', Z = 'oft'){
  
  sim_list = map(1:iter, wins_sim, data = data, X = X, Y = Y, Z = Z)
  
  sim_df <- bind_rows(sim_list) %>% 
    select(iter, team, week, pts.x, fake, pts.y, win, wins)
  
  return(sim_df)
}

test10000 <- lapply(sort(unique(pts_df$team)),
                    function(i) {
                      
                      season_sim(iter = 10000, data = pts_df, X = 'team', Y = 'opponent', Z = i)

                    }) %>% bind_rows()

test2000 <- season_sim(iter = 2000, data = pts_df, X = 'team', Y = 'opponent', Z = 'oft')

test10000 %>% 
  group_by(team, iter) %>% 
    summarise(wins = mean(wins)) %>%
  ungroup() %>% 
  ggplot(aes(x = wins)) +
    geom_histogram(binwidth = 1, color = 'white') +
    theme_ipsum() +
    facet_wrap(~team, nrow = 6) +
    scale_x_continuous(breaks = seq(1,14,1)) +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
