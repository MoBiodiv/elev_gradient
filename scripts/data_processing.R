library(readxl)

dat_All <- read_xlsx('./data/180926-AllRawData.xlsx',
                     sheet = 'All')
dat_SItes <- read_xlsx('./data/180926-AllRawData.xlsx',
                     sheet = 'SItes')

# fix column names replacing spaces with "_"
names(dat_All) <- sub(" ", "_", names(dat_All), fixed=T)
names(dat_All) <- sub(".", "_", names(dat_All), fixed=T)

names(dat_SItes) <- sub(" ", "_", names(dat_SItes), fixed=T)
names(dat_SItes) <- sub(".", "_", names(dat_SItes), fixed=T)
names(dat_SItes) <- sub("(", "", names(dat_SItes), fixed=T)
names(dat_SItes) <- sub(")", "", names(dat_SItes), fixed=T)

# rename Site to Site_Name so that a conflict does not arise
names(dat_SItes)[1] <- "Site_Name"

# fix Site variable in dat_All
dat_All$Site <- ifelse(dat_All$Site == "CATALOOCHEE", "CATA",
                       dat_All$Site)

# merge subplot ids into site data
site_dat <- merge(dat_All[ , c("Code", "Site", "Subplot", "Sample")],
                 dat_SItes, by.x = "Site", by.y = "Site_Code",
                 all.x = TRUE)

# compute new coordinates based upon sampling design
# see ./figs/plot_design.pdf

calc_coord <- function(x, y, subplot, sample) {
    for(i in seq_along(x)) {
        if (subplot[i] == "A" | subplot[i] == "D")
            x[i] = x[i] - 20
        else 
            x[i] = x[i] + 20
        if (subplot[i] == "A" | subplot[i] == "B")
            y[i] = y[i] + 20
        else
            y[i] = y[i] - 20
        if (sample[i] == 1 | sample[i] == 4) 
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

coords <- with(site_dat, calc_coord(UTM_E, UTM_N, Subplot, Sample))
site_dat <- cbind(site_dat, coords)
row.names(site_dat) = site_dat$Code


# export cleaned site and comm data to file

write.csv(site_dat, file = './data/site_dat.csv')

dat_All = as.data.frame(dat_All)
row.names(dat_All) = dat_All$Code

write.csv(dat_All[ , -(1:4)], file = './data/comm_dat.csv')
