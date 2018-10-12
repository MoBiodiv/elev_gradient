library(devtools)
install_github("T-Engel/mobr")
library(mobr)

# read in data files. 
ant_comm <- read.csv("data/comm_dat.csv", row.names = 1) 

ant_plot_attr <- read.csv("data/site_dat.csv", row.names = 1)

# slightly change elev of RAMY b/c of delta_stats issue
ant_plot_attr$Elevation_m[ant_plot_attr$Site == 'RAMY'] = 942

ant_mob_in <- make_mob_in(ant_comm, ant_plot_attr)
pdf('./figs/sample_based_curves.pdf')
for(i in unique(ant_mob_in$env$Site)){
site<-subset(ant_mob_in,Site ==i)
mobr:::compare_curves(site)
title(paste("Site",i, sep=" "))
}
dev.off()
