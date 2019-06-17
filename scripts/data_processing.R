library(readxl)
library(janitor)
library(leaflet)
library(mapview)


# Note: the following two data files have data on 7 more sites than 
# analyzed in Sanders et al. 2007 GEB
dat_All <- read_xlsx('./data/180926-AllRawData.xlsx',
                     sheet = 'All')
dat_SItes <- read_xlsx('./data/180926-AllRawData.xlsx',
                     sheet = 'SItes')

# Note: the following enviornmental data is only from the 22 sites analyzed
# in Sanders et al. 2007 GEB
dat_Smokies <- read_xlsx('./data/SmokiesAbioticData.xlsx')


# fix column names 
dat_All <- clean_names(dat_All)
dat_SItes <- clean_names(dat_SItes)
dat_Smokies <- clean_names(dat_Smokies)

# rename Site to Site_Name so that a conflict does not arise
names(dat_SItes)[1:2] <- c("site_name", "site")

# fix Site variable in dat_All
dat_All$site <- ifelse(dat_All$site == "CATALOOCHEE", "CATA",
                       dat_All$site)

# remove species that never occurs, Pheidole bicarinata
dat_All <- subset(dat_All, select = -pheidole_bicarinata)

# merge subplot ids into site data
site_dat <- merge(dat_All[ , c("code", "site", "subplot", "sample")],
                 dat_SItes, by.x = "site", by.y = "site",
                 all.x = TRUE)
site_dat <- merge(site_dat, dat_Smokies, all.x = TRUE)

# remove incomplete redundant elevation variable
site_dat <- subset(site_dat, select = -elevation)

# compute new coordinates based upon sampling design---------
# see ./figs/plot_design.pdf

calc_coord <- function(x, y, subplot, sample) {
    for(i in seq_along(x)) {
        if (subplot[i] == "A" | subplot[i] == "C")
            x[i] = x[i] - 20
        else 
            x[i] = x[i] + 20
        if (subplot[i] == "A" | subplot[i] == "B")
            y[i] = y[i] + 20
        else
            y[i] = y[i] - 20
        if (sample[i] == 1 | sample[i] == 3) 
            x[i] = x[i] - 4.5
        else
            x[i] = x[i] + 4.5
        if (sample[i] == 1 | sample[i] == 2)
            y[i] = y[i] + 4.5
        else
            y[i] = y[i] - 4.5
    }
    return(data.frame(x, y))
}
    
tst_coords <- data.frame(x = rep(25,16),
                         y = rep(25, 16),
                         subplot = rep(c("A","B","C","D"), each=4),
                         sample = rep(1:4, 4))

new_coords <- with(tst_coords, calc_coord(x, y, subplot, sample))
centroids <- aggregate(new_coords, list(tst_coords$subplot), mean)

pdf('./figs/coord_convert_test.pdf')
plot(new_coords)
text(centroids[1, c('x', 'y')], labels = "A")
text(centroids[2, c('x', 'y')], labels = "B")
text(centroids[3, c('x', 'y')], labels = "C")
text(centroids[4, c('x', 'y')], labels = "D")
dev.off()

# inspect distribution of spatial distances of the plot design--------
dists <- dist(new_coords, method = "euclidean")

hist(dists)

par(mfrow = c(1,1))
dist.distr <- as.data.frame(table(dists))
dist.distr$dists <- as.character(dist.distr$dists)
dist.distr$dists <- as.numeric(dist.distr$dists)
plot(dist.distr)
pdf('./figs/distance_distribution.pdf')
plot(dist.distr)
dev.off()

coords <- with(site_dat, calc_coord(utm_e, utm_n, subplot, sample))
site_dat <- cbind(site_dat, coords)
row.names(site_dat) = site_dat$code

# explot coordinates on map ------------------
m <- leaflet(data = site_dat) %>% 
       addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
       addMarkers(~e, ~n)
mapshot(m, file = paste0(getwd(), "/figs/sample_map.pdf"))

# there appears to be considerable heterogenity in the aspect, slope,
# and other topoedaphic features of these sites beyond just elevation

# export cleaned site and comm data to file----------

write.csv(site_dat, file = './data/site_dat.csv')

dat_All = as.data.frame(dat_All)
row.names(dat_All) = dat_All$code

write.csv(dat_All[ , -(1:4)], file = './data/comm_dat.csv')
