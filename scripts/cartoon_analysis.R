library(mobr)

dat = read.csv('./data/cartoon_data.csv')

scenarios = c('sad', 'N', 'agg')

mob_in_list = list()
mob_in_list$sad = make_mob_in(dat[ , 2:4], dat[ , c(1, 5:7)], c('x', 'y'))
mob_in_list$N = mob_in_list$sad
mob_in_list$agg = mob_in_list$sad

mob_in_list$sad = subset(mob_in_list$sad, cat == "sad")
mob_in_list$N = subset(mob_in_list$N, cat == "N")
mob_in_list$agg = subset(mob_in_list$agg, cat == "agg")

mob = map(mob_in_list, function(x)
    get_delta_stats(x, 'elev', n_perm = 20))

plot(mob$sad, eff_sub_effort = F, eff_disp_pts = F, eff_disp_smooth = T)
plot(mob$N, eff_sub_effort = F, eff_disp_pts = F, eff_disp_smooth = T)
plot(mob$agg, eff_sub_effort = F, eff_disp_pts = F, eff_disp_smooth = T)
