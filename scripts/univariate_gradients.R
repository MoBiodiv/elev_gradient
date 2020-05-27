library(mobr)
library(vegan)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(egg)
library(pbapply)
library(broom)

# read in data files. 
ant_comm <- read.csv("data/comm_dat.csv", row.names = 1)
ant_plot_attr <- read.csv("data/site_dat.csv", row.names = 1)

ant_mob_in <- make_mob_in(ant_comm, ant_plot_attr, coord_names = c('x', 'y'))

# drop site with low number of individuals
ant_mob_in = subset(ant_mob_in, site != 'NODI', drop_levels = T)


## compute mob stats-------------------

stats <- get_mob_stats(ant_mob_in, group_var = "site", 
                       effort_samples = c(15, 80))

plot(stats)

alphas <- stats$samples_stats
alphas$elevation <- ant_plot_attr$elevation_m[match(alphas$group, ant_plot_attr$site)]

gammas <- stats$groups_stats  
gammas$elevation <- ant_plot_attr$elevation_m[match(gammas$group, ant_plot_attr$site)]

# fit linear models
lm_alpha = alphas %>% group_by(index, effort) %>%
    do(mod = lm(value ~ elevation, data = .))
lm_gamma = gammas %>% group_by(index, effort) %>%
    do(mod = lm(value ~ elevation, data = .))

# get model coefs

mod_coef_alpha = broom::tidy(lm_alpha, mod)
mod_coef_gamma = broom::tidy(lm_gamma, mod)

# get model summary
mod_sum_alpha = broom::glance(lm_alpha, mod)
mod_sum_gamma = broom::glance(lm_gamma, mod)

mob_met = rbind(data.frame(scale = 'alpha', alphas),
                data.frame(scale = 'gamma', gammas))
mob_met$index = factor(mob_met$index, 
                       levels = levels(mob_met$index)[c(2:1, 3:7)])

N1 = mob_met %>%
     subset(index %in% 'N') %>%
     ggplot(aes(x = elevation, y = value)) + 
         geom_point(aes(color = scale)) +
         geom_smooth(aes(color = scale), method = 'lm', se = F) +
         labs(x = "Elevation (m)", y = "Total abundance (N)")
ggsave("grad_vs_N.pdf", plot = N1, path = "./figs", width = 15, height = 12, 
       units = "cm")

       
       
SN1 = mob_met %>% 
     subset(index %in% c('S', 'N')) %>%
     subset(scale == 'alpha') %>%
     ggplot(aes(x = elevation, y = value)) + 
         geom_point() +
         geom_smooth(method = 'lm', se = T) +
         facet_wrap(~ index, scales = "free",  
                    labeller = as_labeller(c(S = "Species richness (S)",
                                             N = "Total abundance (N)"))) +
         labs(x = "Elevation (m)")

ggsave('./figs/grad_vs_S&N.pdf', SN1)


p1 = mob_met %>% 
  subset(abs(value) < 1000) %>%
  subset(index %in% c('S', 'S_n', 'S_PIE')) %>% 
  ggplot(aes(x = elevation, y = value, col = scale)) + 
    geom_point() +
    geom_smooth(method = 'lm', se = F) +
    facet_wrap(. ~ index, scales = "free")


p2 = mob_met %>% 
    subset(abs(value) < 1000) %>%
    subset(index %in% c('beta_S', 'beta_S_n', 'beta_S_PIE')) %>% 
    ggplot(aes(x = elevation, y = value)) + 
    geom_point() +
    geom_smooth(method = 'lm', se = F) +
    facet_wrap(. ~ index, scales = "free")

g = ggarrange(p1, p2)
ggsave("ENS.pdf", plot = g, path = "./figs", width = 20, height = 15, units = "cm")


## continuous analysis ------------------

deltas = get_delta_stats(ant_mob_in, env_var = 'elevation_m',
                         group_var = 'site', stat = c('betas', 'r'),
                         type = 'continuous', spat_algo = 'kNCN',
                         n_perm = 19, overall_p = TRUE)

save(deltas, file = './results/deltas.Rdata')
#load('./results/deltas.Rdata')

deltas$env_var = 'elevation (m)'

plot(deltas, stat = 'b1')

pdf('./figs/deltas_noNODI_b1.pdf')
plot(deltas, stat = 'b1', scale_by = 'indiv',
     eff_sub_effort = F, eff_log_base = 2,
     eff_disp_pts = F,
     eff_disp_smooth = T)
dev.off()


pdf('./figs/deltas_r.pdf')
plot(deltas, stat = 'r', scale_by = 'indiv',
     eff_sub_effort = T, eff_log_base = 2.8,
     eff_disp_pts = T,
     eff_disp_smooth = F)
dev.off()

# drop common low elevation species Aphaenogaster rudis
ant_mob_in$comm = subset(ant_mob_in$comm, select= -a_rudis)
deltas = get_delta_stats(ant_mob_in, stat = c('betas', 'r'), 
                         group_var = 'elevation_m',
                         type = 'continuous', n_perm = 19)

plot(deltas, stat = 'b1', scale_by = 'indiv',
     eff_sub_effort = F, eff_log_base = 2.8,
     eff_disp_pts = F,
     eff_disp_smooth = T)

# doesn't have an influence

# drop crazy high abu sites
#24100 gamma  TRMT     N     NA  1046    462.00
#8100  gamma  GOPR     N     NA   900    941.00
ant_mob_in = subset(ant_mob_in, site != "TRMT" & site != "GOPR")

# not much of an influence either on aggregation

# drop the next least populated site: DBSP & TRPA
ant_mob_in = subset(ant_mob_in, site != 'DBSP' & site != 'TRPA',
                    drop_levels = T)

deltas_highN = get_delta_stats(ant_mob_in, group_var = 'site', env_var = 'elevation_m',
                                type = 'continuous', n_perm = 19)    

g = plot(deltas_highN, 'elevation_m', stat = 'b1', scale_by = 'indiv')
ggsave('deltas_highN_b1.pdf', g, path = './figs', width = 20, height = 16,
       units = 'cm' )

# drop down to the 23 sites with more than 40 individuals
sub = tapply(rowSums(ant_mob_in$comm), list(ant_mob_in$env$site), sum)
sub = names(sub)[sub > 40]
ant_mob_in = subset(ant_mob_in, site %in% sub, drop_levels = T)

deltas_higherN = get_delta_stats(ant_mob_in, group_var = 'site', env_var = 'elevation_m',
                               type = 'continuous', n_perm = 199,
                               log_scale = T)

save(deltas_higherN, file = './results/deltas_higherN.Rdata')
#load('./results/deltas_higherN.Rdata')

g = plot(deltas_higherN, 'elevation_m', stat = 'b1', scale_by = 'indiv')
ggsave('deltas_higherN_b1.pdf', g, path = './figs', width = 20, height = 16,
       units = 'cm' )

# exporation of centering effect size
deltas$S_df <- deltas$S_df %>%
    group_by(test, effort) %>%
    mutate(effect = effect - mean(effect)) %>%
    ungroup()

deltas$S_df[deltas$S_df$test == 'SAD', ] <-
    deltas$S_df %>%
    subset(test == 'SAD') %>%
    group_by(effort) %>%
    mutate(effect = effect - mean(effect))



# examine different explanatory variable npp
ant_mob_in = subset(ant_mob_in, !is.na(npp_g_c_m2), drop_levels = T)

deltas_npp = get_delta_stats(ant_mob_in, group_var = "site",
                             env_var = "npp_g_c_m2",
                             type = 'continuous', n_perm = 19, 
                             inds = )

deltas_lognpp = get_delta_stats(ant_mob_in, group_var = 'site', 
                                env_var = "log_npp",
                             type = 'continuous', n_perm = 199)

save(deltas_lognpp, file = './results/deltas_lognpp.Rdata')
#load('./results/deltas_lognpp.Rdata')

plot(deltas_npp, "npp_g_c_m2", stat = 'b1', scale_by = 'indiv') 
plot(deltas_lognpp, "log_npp", stat = 'b1', scale_by = 'indiv', log2 = 'x')

## analysis of composition change ------------

# indirect gradient analysis -----------
empty_sites = rowSums(ant_comm) == 0
ant_comm_no0 = ant_comm[!empty_sites, ]

ant_mds = metaMDS(ant_comm_no0)
stressplot(ant_mds)

plot(ant_mds, display='sp', type='n')
orditorp(ant_mds, display='sp', priority = colSums(ant_comm_no0))
fit = envfit(ant_mds, ant_plot_attr[!empty_sites, c('elevation_m', 'utm_n', 'utm_e')])
plot(fit)

# at the site rather than trap scale
ant_comm_agg = aggregate(ant_comm, by = list(ant_plot_attr$site), sum)[ , -1]
ant_plot_attr_agg = aggregate(ant_plot_attr, by = list(ant_plot_attr$site),
                              function(x) x[1])[ , -1]

ant_mds = metaMDS(ant_comm_agg)

pdf('./figs/ant_mds.pdf')
plot(ant_mds, display='sp', type='n')
orditorp(ant_mds, display='sp', priority = colSums(ant_comm_no0))
fit = envfit(ant_mds, ant_plot_attr_agg[, c('elevation_m', 'utm_n', 'utm_e')])
plot(fit)
stressplot(ant_mds)
dev.off()


ant_rda = rda(sqrt(ant_comm_agg) ~ elevation_m, data = ant_plot_attr_agg)
ant_rda
RsquareAdj(ant_rda)
anova(ant_rda, by = 'terms', permutations = 2e3)
ant_mso = mso(ant_rda, ant_plot_attr_agg[ , c('utm_e', 'utm_n')], grain = 5000, 
              permutations = 1000)

pdf('./figs/rda_results.pdf')
plot(ant_rda, display = c('sp', 'bp'), type='n')
orditorp(ant_rda, display = 'sp')
text(ant_rda, display = 'bp', col='red')
msoplot(ant_mso)
dev.off()

#elev = ant_plot_attr_agg$elevation_m[rowSums(ant_comm) > 0]
#xy = ant_plot_attr[rowSums(ant_comm) > 0, c('UTM_E', 'UTM_N')]
#ants = ant_comm[rowSums(ant_comm) > 0, ]
ant_cca = cca(sqrt(ant_comm_agg) ~ elevation_m, data = ant_plot_attr_agg)
RsquareAdj(ant_cca)
anova(ant_cca, by = 'terms', permutations = 2e3)
ant_mso_cca = mso(ant_cca, ant_plot_attr_agg[ , c('utm_e', 'utm_n')], grain = 5000, permutations = 1000)

pdf('./figs/cca_results.pdf')
plot(ant_cca, display = c('sp', 'bp'), type='n')
orditorp(ant_cca, display = 'sp')
text(ant_cca, display = 'bp', col='red')
msoplot(ant_mso_cca)
dev.off()
