library(mobr)
library(purrr)
library(dplyr)
library(ggplot2)

dat = read.csv('./data/cartoon_data.csv')

scenarios = c('sad', 'N', 'agg')

mob_in_list = list()
mob_in_list$sad = make_mob_in(dat[ , 2:4], dat[ , c(1, 5:7)], c('x', 'y'))
mob_in_list$N = mob_in_list$sad
mob_in_list$agg = mob_in_list$sad

mob_in_list$sad = subset(mob_in_list$sad, cat == "sad")
mob_in_list$N = subset(mob_in_list$N, cat == "N")
mob_in_list$agg = subset(mob_in_list$agg, cat == "agg")

mob = purrr::map(mob_in_list, function(x)
    get_delta_stats(x, 'elev', n_perm = 20))

plot(mob$sad, eff_sub_effort = F, eff_disp_pts = F, eff_disp_smooth = T)
plot(mob$N, eff_sub_effort = F, eff_disp_pts = F, eff_disp_smooth = T)
plot(mob$agg, eff_sub_effort = F, eff_disp_pts = F, eff_disp_smooth = T)


mob$sad$S_df %>% subset(group == 3) %>%
   mutate(effort = ifelse(sample == "plot",
                          effort * 5.5, effort)) %>%
   group_by(test) %>%
   ggplot(aes(effort, S)) + 
    geom_line(aes(group = test, color = test, lty=test))

mob$N$S_df %>% subset(group == 3) %>%
   mutate(effort = ifelse(sample == "plot",
                          effort * 5.5, effort)) %>%
   group_by(test) %>%
   ggplot(aes(effort, S)) + 
    geom_line(aes(group = test, color = test, lty=test))

mob$agg$S_df %>% subset(group == 3) %>%
   mutate(effort = ifelse(sample == "plot",
                          effort * 5.5, effort)) %>%
   group_by(test) %>%
   ggplot(aes(effort, S)) + 
    geom_line(aes(group = test, color = test, lty=test))
