library(vegan)

load('./results/deltas_noNODI.Rdata')

dat = deltas_noNODI$S_df
#dat$effect = dat$S
n_density = deltas_noNODI$density_stat$ind_dens

pdf('./figs/sum_stat_comparison_noNODI.pdf')
plot(deltas_noNODI, 'elevation_m', stat = 'b1', scale_by = 'indiv')
plot(deltas_noNODI,  'elevation_m', stat = 'r2', scale_by = 'indiv')
plot(deltas_noNODI,  'elevation_m', stat = 'r2adj', scale_by = 'indiv')
plot(deltas_noNODI,  'elevation_m', stat = 'f', scale_by = 'indiv')
plot(deltas_noNODI,  'elevation_m', stat = 'p', scale_by = 'indiv')
dev.off()

ggplot(dat, aes(group, effect)) +
       geom_point(aes(group = effort, color = effort)) +
       facet_grid(. ~ test) 

# first effort needs to be standardized between sample and individual
# based methods using average density

max_n = max(dat$effort[dat$sample == 'indiv'])

dat$virt_effort = with(dat, ifelse(sample == "plot", effort * n_density, 
                                   effort))
new_effort = 1:max_n

agg_effect = dat %>%
             subset(test == 'agg') %>%
             group_by(group) %>%
             group_map( ~ pracma::pchip(.$virt_effort, .$effect, new_effort)) %>%
             unlist()

dat_interp = data.frame(group = rep(unique(dat$group), each = length(new_effort) * 3),
                        test = rep(c('SAD', 'N', 'agg'), each = length(new_effort)),
                        effort = new_effort, 
                        effect = c(dat$effect[dat$test != 'agg'], agg_effect))

 ggplot(dat_interp, aes(group, effect)) +
       geom_point(aes(group = effort, color = effort)) +
       facet_grid(. ~ test) 


dat_interp_spr = dat_interp %>% spread(test, effect)

mod = lm(group ~ effort + SAD + N + agg, data = dat_interp_spr)
summary(mod)
par(mfrow=c(1,4))
termplot(mod, partial.resid = T, se = T)


summary(lm(group ~ SAD + N + agg, data = dat_interp_spr, subset = effort == 8))

summary(lm(group ~ agg, data = dat_interp_spr, subset = effort == 8))


with(dat_interp_spr, 
     varpart(group[effort == 5], ~ SAD, ~ N, ~ agg, data = dat_interp_spr,
             subset = effort == 5))

n = 5

out = lapply(new_effort, function(n)
             with(dat_interp_spr, 
                  varpart(group[effort == n], SAD[effort == n],
                          N[effort == n], agg[effort == n])))

out = sapply(new_effort, function(n)
             with(dat_interp_spr, 
                  varpart(group[effort == n], SAD[effort == n],
                          N[effort == n], agg[effort == n])$part$indfract$Adj.R.square))

round(out, 2)

plot(out[8, ])
