
# =========== Tropical forest tree community ecology: Barro Colorado Island data from the vegan package ===========

# Data from the 50ha Forest Dynamics Project on Barro Colorado Island, Panama
# These data are tree species abundance counts for a grid of 50 equally sized sub-plots (20x20m) comprising a full 50ha plot
# *All* individuals with a stem diameter >= 10cm at breast height were counted in each subplot
# Making this an extremely comprehensive dataset of ecological community data, sampled in a standardised way
# It is therefore an ideal dataset to examine how individual species abundances are distributed over space
# As well as how communities assemble and patterns of biodiversity respond to landscape factors like habitat
# Becuase it is a complete inventory of *all individuals* in the entire area, 
# we can also use these data to explore how different sampling approaches might affect our understanding of the system

# Some details about the project in this paper
# Figure 2 shows how individuals of several species are distributed between quadrats - spatial clustering
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/j.1365-2745.2001.00615.x

# And this paper provides an example of where they can be used to feed into broader synthetic research on biodiversity patterns
# (Here, understanding the distance-decay of community similarity)
# https://www.science.org/doi/10.1126/science.1066854

# We accessed the data from the 'vegan' package and processed to create this dataframe
# see script "get_bci_data.R"

# Learning objectives
# Understand what is sampling - why do we sample the world to understand it?
# Understand what are parameters and statistics, and what assumptions are we making when we estimate them?
# What are statistical distributions and why do we approximate data with distributions?
# How does the way the data have been generated - the "data generating process" - impact how we should analyse and interpret them?



# ============= 1. SET UP ENVIRONMENT AND READ IN DATA =============

# a. Set your working directory to the appropriate location and load the dependencies
setwd("C:/Users/roryj/Documents/PhD/202205_ucl_lassa/teaching/BIOS0002/workshop_bci/")
library(vegan); library(sp); library(ggplot2); library(dplyr); library(raster); library(magrittr)

# b. Read the data "bci_communities.csv" into R using the read.csv function
bci <- read.csv("./data/bci_communities.csv")

# c. Explore the data - how are they structured? How many sampling sites are in the data, and how many species?
length(unique(bci$site_id)) # 50 sites
length(unique(bci$species)) # 225 species






# ============ 2. DESCRIBING SPECIES-LEVEL PATTERNS OF RELATIVE ABUNDANCE =============

# These data contain a wealth of information about how abundant each species is in each plot, and across the survey area as a whole
# One of the most ubiquitous patterns in community ecology is that most species in a community are relatively uncommon (i.e. low abundance)
# whereas a few species are relatively common (i.e. high abundance) 
# If we plot species level abundances in a graph we get the characteristic "hollow curve" of relative abundance that is ubiquitous throughout nature
# Let's explore the dataset and look at how species abundances are distributed

# Firstly, let's calculate a simple measure - total abundance (i.e. the total number of individuals) for each species across the entire study area
# (Remember, this data contains *every* individual in a 50ha plot!)
# Species abundances in each subplot are stored in the variable 'count'

# a. Use subsetting to calculate the total abundance for the species "Andira inermis" 
# (a nitrogen-fixing tree that produces huge pink blossoms) across all 50 plots
sum(bci$count[ bci$species == "Andira inermis" ]) # 28

# b. Now let's use the same principle to calculate the total abundance of *every* tree species
# Write some lines of code to calculate the total abundance of every species and save it into a new dataframe called "abund"
# Hint: you could do this using a for loop, or using the dplyr functions group_by() and summarise()

abund = data.frame(species <- unique(bci$species), total_abundance=NA)
for(i in 1:nrow(abund)){
  sp_i <- abund$species[i]
  abund$total_abundance[i] <- sum(bci$count[ bci$species == sp_i ]) 
}

abund = bci %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(total_abundance = sum(count))

# c. Look at the data frame you have created - what are the top 3 most abundant species?
# ANSWER: "Faramea occindentalis", "Trichilia tuberculata", "Alseis blackiana"

# d. Can you identify how many species have only 1 individual present in the entire study area?
sum(abund$total_abundance == 1) # ANSWER: 19

# e. Now we have our total abundances, let's look at how they are overall distributed across species.
# Plot a histogram to examine the relative abundance distribution across all tree species
# Hint: you can use the "hist" function or ggplot to do this
# What do you notice about the distribution of abundances? What does the shape tell you about the structure of the tree community?

hist(abund$total_abundance, 100)

ggplot(abund) + 
  geom_histogram(aes(x=total_abundance), binwidth=20, fill="coral2", col="black") + 
  theme_bw()




# So we can clearly see the "j-shaped" curve of relative abundance across species, for total abundance across the 50 ha plot
# But total abundance is only one possible metric describing species population characteristics
# And it is quite simple - it does not capture any variability in abundance across space
# This is important because the whole study area is quite large, and covers a wide gradient of habitats and elevation levels
# So many species may be quite heterogeneously distributed across this area - as we can see in [X PICTURE] individuals often tend to cluster across habitats
# Perhaps one species may be very abundant overall, but only in one or two subplots, whereas others may be more evenly distributed across the whole area
# So another, potentially more comparable measure of species abundance is *density* (i.e. number of individuals per by unit area)
# If each subplot is one unit of area, for each species we have 50 counts of individuals per unit area (i.e. abundance per subplot)
# This provides us useful information about not only how abundant overall a species is, but how evenly distributed it is
# This is where plotting the distribution of the data within species becomes useful. 

# Let's look at this for an example species, "Oenocarpus mapora", a species of palm that is the 4th most abundant overall in the study site

# f. Subset the dataframe to only the records for this species, and make a few plots to explore the variation in subplot level counts
# Hint: histograms, boxplots and dotplots are all useful  
# How would you describe the distribution of counts across subplots for this species? 

om = bci[ bci$species == "Oenocarpus mapora", ]

hist(om$count, 25)
boxplot(om$count)
dotplot(om$count)

ggplot(om) + geom_histogram(aes(x=count), color="black", fill="coral", binwidth=5) + xlab("Number of individuals per subplot")
ggplot(om) + geom_boxplot(aes(y=count), color="black", fill="coral") 

# g. Now let's compare this to the next most abundant species, Poulsenia armata (the 5th most abundant overall)
# This species has a very similar total number of individuals in the overall plot to O mapora (724 versus 755)
# Create a boxplot that visualises the distribution of counts for both species alongside each other (hint: ggplot and geom_boxplot are your friends)
# What looks different in this distribution - how would you describe them in comparison to each other?

dat = bci[ bci$species %in% c("Poulsenia armata","Oenocarpus mapora"),  ]
ggplot(dat) + geom_boxplot(aes(x= species, y=count), color="black", fill="coral") + xlab("Number of individuals per subplot")


# These differences are interesting - what's going on? 
# Can you speculate about ecological reasons for why these distributions might look so different,
# despite both species having a very similar number of total individuals in the entire study area?

# Often, ecologists are interested in calculating summary metrics to describe key species or population characteristics, such as mean population density
# We can then use these to compare between species or populations in different areas or experiencing different conditions (e.g. experimental treatments)
# This usually involves calculating summary parameters describing central tendency (e.g. mean or median) or variation (e.g. standard deviation)
# It is important to remember that these parameters are simplified descriptions of *distributions of data points* like those we've seen for these two species
# Calculating these *loses information* - we no longer have all the individual counts, but instead a single parameter that summarises them
# This can be very useful, but it is important to be vigilant around how different metrics are sensitive to the *shape* of the data distribution and so might be more or less appropriate
# Let's look at this for our two focal species

# h. Calculate the mean species abundance per subplot for Oenocarpus mapora and Poulsenia armata - what do you notice?
mean(bci$count[ bci$species == "Oenocarpus mapora"])
mean(bci$count[ bci$species == "Poulsenia armata"])

# i. Let's try a different measure. Create a new data frame with a row for each species, and two columns respectively showing the mean and the median abundance
# Hint: you can do this using subsetting, or if you are familiar with dplyr, you can try dplyr's group_by and summarise functions
# How similar are the means and medians, and how does this vary between the two species? What do you think is going on?

comp = data.frame(
  species = c("Poulsenia armata",
              "Oenocarpus mapora"),
  mean_abund = c(mean(bci$count[ bci$species == "Poulsenia armata"]),
                 mean(bci$count[ bci$species == "Oenocarpus mapora"])),
  median_abund = c(median(bci$count[ bci$species == "Poulsenia armata"]),
                   median(bci$count[ bci$species == "Oenocarpus mapora"]))
)

comp = bci %>% 
  dplyr::filter(species %in% c("Poulsenia armata", "Oenocarpus mapora")) %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(
    mean_abund = mean(count),
    median_abund = median(count)
  )

# One way to see what's going on is to visualise this. This code creates boxplots with the mean and median overlaid as points
# So we can see both the underlying distributions, and the two measures of central tendency calculated from them

# data
dat = bci[ bci$species %in% c("Poulsenia armata", "Oenocarpus mapora"),  ]

# means/meds
metrics = data.frame(
  species = rep(c("Poulsenia armata", "Oenocarpus mapora"), each=2),
  value = c(mean(bci$count[ bci$species == "Poulsenia armata"]),
            median(bci$count[ bci$species == "Poulsenia armata"]),
            mean(bci$count[ bci$species == "Oenocarpus mapora"]),
            median(bci$count[ bci$species == "Oenocarpus mapora"])),
  metric = rep(c("mean", "median"), 2)
  )

# plot
ggplot() + 
  geom_boxplot(data=dat, aes(x=species, y=count), fill="grey98") + 
  geom_point(data=metrics, aes(x=species, y=value, color=metric, pch=metric), size=5) + 
  theme_bw() + 
  scale_color_viridis_d(begin=0.1, end=0.7) +
  xlab("Species") + ylab("Abundance per subplots")


# k. Why are the means and medians so different for one species and not the other? 
# Can you comment on which measures might be more appropriate for describing the central tendency of these data?
# If we were to only calculate and compare the means for these two species, what information would we be losing?




# ================== WHY DO WE SAMPLE THE WORLD, AND WHY DOES SAMPLING DESIGN MATTER? =================

# The BCI dataset is unusual in that it comes from a long-term monitoring project on a relatively small plot (50ha) where every single individual is counted
# This is feasible because the plot is relatively small and the subject of ongoing, focused research activity
# However, most of the time when we want to understand ecological systems - for example, how communities assemble in relation to the environment, species and interactions -
# it is almost always infeasible to measure and count everything because of time or resource constraints (especially when we're doing anything across broader areas)
# Instead we *sample* the world - we design surveys to take a representative sample of the system we're interested in
# For example, following on from the last exercise, we might want to determine the mean population density of one or more tree species, by sampling a subset of plots across a broader area
# We assume this *sample mean* to be an accurate representation of the true mean, provided we have an adequate sample size and an appropriate sampling design

# But how we design our sample matters - for example, how big a sample size do we need to get a good estimate of the population mean?
# How might this differ between species whose populations have different spatial structures, or that naturally occur at either very low or high densities?

# One advantage of the BCI dataset is that we can use it to explore some of these issues, precisely because every single individual is in the dataset
# So we have the unusual ability to compare the *true* values with the values we estimate from sampling the system
# In this exercise we'll explore this, again focusing on our two tree species we looked at in the last exercise

# Firstly, you'll recall that one species had a much wider variance in counts - a more *overdispersed* distribution with more very low and very high values - than the other
# One excellent way to get a deeper understanding of this is to look at the data spatially
# In the dataframe we have plot coordinates (in this case columns x_n and y_n) that describe the locations of each subplot
# We can use this to plot the spatial distribution of tree counts for each of our focal species, and compare the difference

# a. Use ggplot and geom_tile to plot a tile plot of of the abundances of each species per subplot, with x_n and y_n as coordinates
# Hint: the "fill" aesthetic can be used to scale the tile colour by the species abundances, and facet_wrap can be used to create multipanel plots
# How would you describe the spatial distribution of each species?
# Can you speculate on how these differences might affect the reliability of population means calculated from different sample sizes?

dat = bci[ bci$species %in% c("Poulsenia armata", "Oenocarpus mapora"),  ]
ggplot(dat) + 
  geom_tile(aes(x_n, y_n, fill=count)) + 
  theme_bw() + 
  scale_fill_viridis_c(option="magma", direction=-1) + 
  facet_wrap(~species) +
  coord_fixed()

# We know the true population means for these two species across all the 50 plots - we calculated them above
# (15.1 indivs per plot for P armata, and 15.76 indivs per plot for O mapora)
# Let's explore how accurate a random sample of plots is at estimating the true population mean

# b. Use the 'sample' function to select a random sample of 5 subplots for each species
# From this random sample, calculate a *sample mean* abundance for each species
# How similar is this random sample to the true population mean?

mean(sample(dat$count[ dat$species == "Poulsenia armata" ], 5, replace=FALSE))
mean(sample(dat$count[ dat$species == "Oenocarpus mapora" ], 5, replace=FALSE))

# This is just one random sample - there are thousands of possible permutations of sites we could select. 
# Try re-running your lines of code above a few times - they should give different answers each time.
# To get a better idea of the possible range of sample means we might obtain from a random sample of 5 plots, we can use simulation.
# We can repeat the random sample process many times, to build up a picture of the distribution of sample means for a given sample size.

# c. Write some code to repeat the sampling process 100 times for each species:
# Each time, draw a random sample of 5 sites, calculate the sample mean, then add it to a vector
# At the end you should have a vector of 100 sample means for each species
# Hint: you will probably need to use a "for" loop for this

pa_samp = c()
for(i in 1:100){ pa_samp = c(pa_samp, mean(sample(dat$count[ dat$species == "Poulsenia armata" ], 5, replace=FALSE))) }

om_samp = c()
for(i in 1:100){ om_samp = c(om_samp, mean(sample(dat$count[ dat$species == "Oenocarpus mapora" ], 5, replace=FALSE))) }


# d. Now plot a histogram of the distribution of sample means for each species, and add a vertical line for the true population mean
# Hint: you can do this using the plot() and abline() functions in base plot, or you can use ggplot
# What do you notice about the differences between the two histograms of sample means, and their relationship to the true population mean? 
# Can you speculate about why they are different? 

hist(pa_samp, 20, xlab="Sample mean"); abline(v=15.1, col="red")
hist(om_samp, 20, xlab="Sample mean"); abline(v=15.76, col="red")

ggplot() + 
  geom_histogram(aes(x = pa_samp), binwidth=2, fill="grey95", col="black") + 
  theme_bw() + 
  geom_vline(xintercept=15.1, col="red") + 
  xlab("Sample mean") + ggtitle("Poulsenia armata")
  
ggplot() + 
  geom_histogram(aes(x = om_samp), binwidth=2, fill="grey95", col="black") + 
  theme_bw() + 
  geom_vline(xintercept=15.76, col="red") + 
  xlab("Sample mean") + ggtitle("Oenocarpus mapora")



# Only 5 plots is a fairly small sample size, and is likely to be very sensitive to natural variability.
# Usually, larger sample sizes can help us to get more accurate estimates of the true population mean
# e. Repeat your code above, but this time take samples of 20 sites. Can you see a difference? How does this vary between the 2 species?

pa_samp = c()
for(i in 1:100){ pa_samp = c(pa_samp, mean(sample(dat$count[ dat$species == "Poulsenia armata" ], 20, replace=FALSE))) }

om_samp = c()
for(i in 1:100){ om_samp = c(om_samp, mean(sample(dat$count[ dat$species == "Oenocarpus mapora" ], 20, replace=FALSE))) }

ggplot() + 
  geom_histogram(aes(x = pa_samp), binwidth=2, fill="grey95", col="black") + 
  theme_bw() + 
  geom_vline(xintercept=15.1, col="red") + 
  xlab("Sample mean") + ggtitle("Poulsenia armata")

ggplot() + 
  geom_histogram(aes(x = om_samp), binwidth=2, fill="grey95", col="black") + 
  theme_bw() + 
  geom_vline(xintercept=15.76, col="red") + 
  xlab("Sample mean") + ggtitle("Oenocarpus mapora")



# Let's look at how this varies across a range of different sample sizes, and between species.
# The code below writes a function called randomSampleMean which generates 100 random samples of plots, of a specified size, then calculates the sample mean
# We run this for various different sample sizes for our two species, and compare the resulting distributions of sample means to the true population mean (which we actually know!)

# MAYBE WE GIVE THEM THIS CODE
# function to generate 100 sample means from random samples of specified size, for a given species "spp"
randomSampleMean = function(spp, sample_size){
  
  # generate sample
  samp = c()
  for(i in 1:100){ samp = c(samp, mean(sample(bci$count[ bci$species == spp ], sample_size, replace=FALSE))) }
  
  # create and return results dataframe
  result = data.frame(
    species = spp,
    sample_mean = samp,
    sample_size = sample_size
  )
  
  return(result)
}

# generate across a range of sample sizes for each species
pa5 = randomSampleMean(spp = "Poulsenia armata", sample_size = 5)
pa10 = randomSampleMean(spp = "Poulsenia armata", sample_size = 10)
pa20 = randomSampleMean(spp = "Poulsenia armata", sample_size = 20)
pa30 = randomSampleMean(spp = "Poulsenia armata", sample_size = 30)

om5 = randomSampleMean(spp = "Oenocarpus mapora", sample_size = 5)
om10 = randomSampleMean(spp = "Oenocarpus mapora", sample_size = 10)
om20 = randomSampleMean(spp = "Oenocarpus mapora", sample_size = 20)
om30= randomSampleMean(spp = "Oenocarpus mapora", sample_size = 30)

# combine and plot
data_for_plot = do.call(
  rbind.data.frame,
  list(pa5, pa10, pa20, pa30, om5, om10, om20, om30)
)

# true means to add to the plot
true_means = data.frame(
  species = c("Poulsenia armata",
              "Oenocarpus mapora"),
  mean_abund = c(mean(bci$count[ bci$species == "Poulsenia armata"]),
                 mean(bci$count[ bci$species == "Oenocarpus mapora"]))
  )

# plot
ggplot(data_for_plot) + 
  geom_histogram(aes(x=sample_mean, fill=species)) + 
  geom_vline(data=true_means, aes(xintercept=mean_abund), col="darkblue", lty=2) + 
  facet_grid(sample_size~species) + 
  theme_bw()


# f. Run the code above to simulate sampling the BCI plot for each species across a range of sample sizes.
# Can you comment on the main differences you can see between the two species? 
# How relatively big a sample size do you need to get an accurate estimate of the true mean for O. mapora, compared to P. armata?
# Thinking back to the spatial plot above, how do you think these differences might relate to the differing ecology of the two species?#

# g. The BCI data frame also contains metadata about some key characteristics of each subplot, including habitat type and environmental heterogeneity
# Explore the data for these two species - what other factors might we need to account for in our sampling or analysis design?

ggplot(dat) + 
  geom_boxplot(aes(x=Habitat, y=count,  fill=species)) + 
  facet_wrap(~species)

ggplot(dat) + geom_point(aes(x=EnvHet, y=count, col=species)) 





# ================= WHAT ARE STATISTICAL DISTRIBUTIONS AND HOW DO THEY HELP US DEAL WITH THESE ISSUES? ===============

# normal distribution vs oenocarpus
# poisson distribution vs poulsenia

a = rnorm(n = 10000, mean=15.5, sd=7.4)
hist(a)
hist(om$count)




dat %>%
  ggplot() + 
  geom_point(aes(x, y, size=count, col=count)) + 
  theme_bw() + 
  facet_wrap(~species)
















fo = bci[ bci$species == "Trichilia tuberculata", ]
ggplot(fo) + geom_histogram(aes(x=count), color="black", fill="coral", binwidth=4) + xlab("Number of individuals per subplot")
ggplot(fo) + geom_boxplot(aes(y=count), color="black", fill="coral") + xlab("Number of individuals per subplot")


# abundance isn't a fixed thing
# But what about density (i.e. abundance per unit area) - an important life-history characteristic and one that varies over space
# this is where we can see how much variation there is











# -------------- get spatial aspects of data ------------

# environment
env = BCI.env %>%
  dplyr::mutate(site_id = 1:nrow(BCI.env)) 

# points in lat-lon
points = data.frame(x=env$UTM.EW, y=env$UTM.NS)
spts = SpatialPoints(points, proj4string=CRS("+proj=utm +zone=17 +datum=WGS84")) 

# BCI topography (downloaded from here https://www.dropbox.com/s/yc4cbled4erfk7i/BCI_ColoredShaded_Relief.zip?dl=1)
topo = raster::raster("C:/Users/roryj/Desktop/BCI_ColoredShaded_Relief/BCI_ColoredShaded_Relief.tif")
topo = crop(topo, extent(spts) + 1500)

# plot with even sampling grid
topo %>%
  as.data.frame(xy=TRUE) %>%
  ggplot() +
  geom_raster(aes(x, y, fill=BCI_ColoredShaded_Relief)) + 
  coord_fixed() + 
  scale_fill_viridis_c(option="cividis", name="Altitude (m)") +
  theme_classic() + 
  geom_point(data=points, aes(x, y))

# sptransform and get lat lons
spts = spTransform(spts, CRS("+proj=longlat +datum=WGS84"))
env = cbind(env, coordinates(spts))


# --------------- combine into BCI data --------------

# bci data
dd = BCI %>%
  dplyr::mutate(site_id = 1:nrow(BCI)) %>%
  reshape2::melt(id.vars = "site_id") %>%
  dplyr::mutate(variable = unlist(lapply(strsplit(as.vector(variable), "[.]"), paste, collapse=" "))) %>%
  left_join(env) %>%
  dplyr::rename("species"=variable, "abundance"=value)


# ----------- some descriptive stuff -----------

# boxplots of site-level abundance from all species ordered by median abundance
# some nice measures of different means and dispersion
mm = dd %>% 
  group_by(species) %>%
  dplyr::summarise(abund = median(abundance)) %>%
  dplyr::arrange(desc(abund))
dd %>%
  dplyr::mutate(species = factor(species, levels = mm$species, ordered=TRUE)) %>%
  ggplot() + 
  geom_boxplot(aes(species, abundance, group=species)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))

# summarise this - see how dispersion increases with higher mean values (poisson process)
dd %>% 
  group_by(species) %>%
  dplyr::summarise(abundance_mean = mean(abundance),
                   abundance_sd = sd(abundance)) %>%
  ggplot() + 
  geom_point(aes(abundance_mean, abundance_sd))

# visualise for the top 25 most abundant species
# nicely shows the ecological property of a few very abundant species and lots of low abundance species
dd %>% 
  dplyr::filter(species %in% mm$species[1:25]) %>%
  dplyr::mutate(species = factor(species, levels = mm$species[1:25], ordered=TRUE)) %>%
  ggplot() + 
  geom_boxplot(aes(species, abundance, group=species), fill="coral1") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))



# ---------- calculate some site-level diversity indices ----------

# calculate plant diversity
div = dd %>% 
  dplyr::group_by(site_id) %>%
  dplyr::summarise(sr = sum(abundance > 0),
                   shannon = vegan::diversity(abundance, index="shannon")) %>%
  dplyr::left_join(env)

# some visualisation of environmental characteristics versus richness and diversity
# can see that young growth forests on average have SR within the bounds of old growth
# but much lower diversity when you account for evenness (i.e. dominated by a few individuals)
p1 = div %>% 
  ggplot() + 
  geom_boxplot(aes(Habitat, sr), fill="skyblue2") + 
  xlab("Habitat type") + ylab("Species richness") + 
  theme_minimal()
p2 = div %>% 
  ggplot() + 
  geom_boxplot(aes(Habitat, shannon), fill="skyblue2") + 
  xlab("Habitat type") + ylab("Shannon diversity") + 
  theme_minimal()
gridExtra::grid.arrange(p1, p2, nrow=1)



# --------- show how increasing sample size improves inference of population mean -------

sampleExample = function(species){
  
  ds = dd %>% dplyr::filter(species == species)
  
  # take 100 samples of 10
  samp10 = c()
  for(i in 1:100){ samp10 = c(samp10, mean(sample(ds$abundance, 5, replace=FALSE))) }
  
  # take 100 samples of 20
  samp20 = c()
  for(i in 1:100){ samp20 = c(samp20, mean(sample(ds$abundance, 10, replace=FALSE))) }
  
  # take 100 samples of 30
  samp30 = c()
  for(i in 1:100){ samp30 = c(samp30, mean(sample(ds$abundance, 30, replace=FALSE))) }
  
  # plot against true mean
  for_plot = data.frame(
    sample_size = rep(c(5, 10, 30), each=100),
    sample_mean = c(samp10, samp20, samp30)
  )
  print(
    for_plot %>%
      ggplot() + 
      ggforce::geom_sina(aes(x=factor(sample_size), sample_mean, col=factor(sample_size))) + 
      geom_hline(yintercept=mean(ds$abundance), lty=2) +
      theme_minimal() + ggtitle(species)
  )
}

# run for various species from the most abundant to the least abundant
sampleExample(mm$species[1])
sampleExample(mm$species[3])
sampleExample(mm$species[5])
sampleExample(mm$species[5])
sampleExample(mm$species[7])
sampleExample(mm$species[100])
sampleExample(mm$species[150])

sampleExample(mm$species[150])
