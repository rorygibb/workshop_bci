
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
# Calculating these *loses information* - we no longer have all the individual counts, but instead 1 or 2 parameters that summarise them
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

comp

# One way to see more clearly what's going on is to visualise this. This code creates boxplots with the mean and median overlaid as points
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


# k. Why are the means and medians so different for one species and not the other, and how is this related to the assumptions you are making when you calculate the mean?
# Can you comment on which measures might be more appropriate for describing the central tendency of these data for each species?
# If we were to only calculate and compare the means for these two species, what information would we be losing?




# ================== AN INTRODUCTION TO STATISTICAL DISTRIBUTIONS ==================

# In the previous exercise, we encountered an important challenge in data analysis, which is how to calculate summary parameters to describe our variable of interest
# Often we will use measures like the mean or the median to summarise the central tendency of a dataset (e.g. the average population density)
# But their reliability as a measure of central tendency depends on how the underlying variable is *distributed*
# As we have seen for the O mapora and P armata, if the distribution of data is skewed, this can lead to a skewed estimate of the central tendency
# (where, in this example, the mean for P armata is strongly influenced by a few very high abundance values, even though abundance in most subplots is lower)
# This is because, although we may not often consider it, we are making *assumptions* about how the data are distributed when we calculate even a simple measure like the mean
# Which means that an analysis that compared only the means between the two species would conclude they had a very similar average population density
# Whereas if we compared the medians, or other metrics that describe their distribution, we would come to quite differnet conclusions

# Statistical distributions are useful tools for formalising these assumptions about how data are distributed
# They become very important later when we want to statistically test for differences or relationships between variables
# because they provide a tool to examine the assumptions we are making - such as the problem of skewed data distributions when we are comparing means
# In this exercise we will examine 2 distributions that are very commonly used in statistics - the normal (Gaussian) and the Poisson distribution


# When we calculate the mean as a measure of central tendency, we are assuming that the data are evenly (i.e. symmetrically) distributed around the centre of the distribution
# This was pretty much the case for O mapora as we saw in the exercise above
# One distribution that has this kind of even spread is the NORMAL DISTRIBUTION - the classic symmetrical "bell-curve" shaped distribution that is very widely used in statistics
# Because the normal distribution is perfectly symmmetrical, the mean perfectly describes the central tendency of the distribution,
# and the standard deviation describes its width (i.e. how widely spread the observations are)
# With 68% of observations falling within 1 sd of the mean, and 95% falling within 2 sds of the mean
# So the distribution is described by 2 parameters: the mean (mu) and the standard deviation (sigma)

# a. The rnorm function in R generates random samples from a normal distribution with a specified mean and standard deviation
# We can use this to generate lots of samples to build up a picture of the distribution's shape
# Use the line of code below to generate 100 samples from a normal distribution with a mean of 0 and a standard deviation of 1
# Look at the vector this has created ("norm") then write some code to plot a histogram of the samples
# Add a vertical line for the mean, and vertical lines for the mean plus/minus 1 standard deviation, and for the mean plus/minus 2 standard deviations

# THIS LINE IS PROVIDED
normA = rnorm(n=100, mean=0, sd = 1)

# ANSWER
hist(normA, 25); abline(v=0, col="red"); abline(v=c(-1, 1), col="blue")

ggplot() + 
  geom_histogram(aes(x=normA), bins=25, fill="grey90", col="black") +
  geom_vline(xintercept = 0, col="red") +
  geom_vline(xintercept=c(-1, 1), col="blue") + 
  theme_bw()

# b. Modify your code from question (a) to instead generate 10,000 samples and save to a vector called "normB"
# How different does this look from when you generated only 100 samples - why do you think this is?
normB = rnorm(n=10000, mean=0, sd = 1)

ggplot() + 
  geom_histogram(aes(x=normB), bins=25, fill="grey90", col="black") +
  geom_vline(xintercept = 0, col="red") +
  geom_vline(xintercept=c(-1, 1), col="blue") + 
  theme_bw()

# The symmetrical nature of the normal distribution means that the mean perfectly describes its central tendency 
# c. Calculate the mean of these two random samples (normA and normB) - how similar are they to the mean we specified when we generated them using rnorm?
# If we, as ecologists, went out and collected data on a phenomenon (e.g. tree abundance) that were perfectly normally distributed, 
# would the mean be an appropriate measure of central tendency? What if the data were more skewed?
mean(normA)
mean(normB)

# d. The two parameters - mean and standard deviation - define the position and width of a normal distribution
# Using the code above, plot a few histograms to explore what happens if you change the mean and the standard deviation parameters
# (Hint: try changing the sd parameter without changing the mean - what happens?)
normX = rnorm(n=10000, mean=10, sd = 5)
ggplot() + 
  geom_histogram(aes(x=normX), bins=25, fill="grey90", col="black") +
  theme_bw()

# When we do statistics, we are almost always making assumptions about data distributions, as an approximation of a complex reality
# Let's take a look in more depth at how well the normal distribution approximates the distribution of our tree species O mapora
# (Which you will recall was less skewed in distribution than P armata)

# e. Calculate the mean and standard deviation of counts acros all subplots for O mapora
mean(om$count)
sd(om$count)

# f. Modify your code above to generate and visualise 50 samples from a normal distribution with the same mean and sd as O mapora
# (By drawing 50 samples we are mimicking the same sample of 50 subplots as in the BCI data)
normX = rnorm(n=50, mean=15.76, sd = 7.42)
ggplot() + 
  geom_histogram(aes(x=normX), bins=25, fill="grey90", col="black") +
  theme_bw()

# g. Compare this to the distribution of observed counts for O mapora. How well do you think this real-world sample is approximated by a normal distribution?
# Do you notice anything about the normal distribution that does not match the ecological reality?
# (Hint: remember rnorm generates a random sample, so it can be useful to explore re-running the sample a few times)
ggplot() + 
  geom_histogram(aes(x=om$count), bins=25, fill="grey90", col="black") +
  theme_bw()


# You may have noticed a few differences between our generated normal distribution values, and the real world sample, such as...
# Our counts are only whole numbers whereas the normal distribution allows fractional values (but you can't really have 0.3 of a tree) 
# The normal distribution can produce negative values, whereas we can't have -1 trees

# One distribution that is very commonly used in ecology to approximate count data (discrete numbers) that are non-negative is the POISSON DISTRIBUTION
# This generates only whole numbers and is constrained to be non-negative
# The Poisson distribution is described by a single parameter, lambda, that defines both the mean and variance of the distribution
# Because of this, its shape is less symmetrical than the normal distribution, particularly at low values

# h. The rpois function generates random samples from a Poisson distribution. Generate 1000 samples from a Poisson distribution with lamba (mean) of 15.
# Look at the vector of samples and plot a histogram, what do you notice?
rpois(n=1000, lambda=15)
ggplot() + 
  geom_histogram(aes(x=rpois(n=50, lambda=15)), fill="grey90", col="black", binwidth = 1) +
  theme_bw()

# i. Explore how the shape of this distribution changes with different lambda values (hint: look at when lambda is very low)
# What do you notice compared to a normal distribution?

# j. O mapora has a mean count of 15.7 and P armata has a mean count of 15.1. Compare the distributions of observed counts for these two species,
# with Poisson distributions generated using the same mean. How similar is each species to a Poisson distribution?





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
# One way to get a deeper understanding of this is to look at the data spatially
# In the dataframe we have plot coordinates (in this case columns x_n and y_n) that describe the grid locations of each subplot
# We can use this to plot the spatial distribution of tree counts for each of our focal species, and compare the difference

# a. Use ggplot and geom_tile to plot a tile plot of of the abundances of each species per subplot, with x_n and y_n as coordinates
# Hint: the "fill" aesthetic can be used to scale the tile colour by the species abundances, and facet_wrap can be used to create multipanel plots
# How would you describe the spatial distribution of each species?
# If we were to sample a subset of the total area, 
# can you speculate on how these differences might affect the reliability of population means calculated from different sample sizes?

dat = bci[ bci$species %in% c("Poulsenia armata", "Oenocarpus mapora"),  ]
ggplot(dat) + 
  geom_tile(aes(x_n, y_n, fill=count)) + 
  theme_bw() + 
  scale_fill_viridis_c(option="magma", direction=-1) + 
  facet_wrap(~species) +
  coord_fixed()

# We know the true population mean density for these two species across all the 50 plots - we calculated them above
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
# Hint: you will need to use a "for" loop to do this

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



# Only 5 plots is a fairly small sample size, and is likely to be very sensitive to natural variability over space.
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
# The code below create a function called randomSampleMean which generates 100 random samples of plots, of a specified size, then calculates the sample mean
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
om30 = randomSampleMean(spp = "Oenocarpus mapora", sample_size = 30)

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
# How relatively large a sample size do you need to get an accurate estimate of the true mean for O. mapora, compared to P. armata?
# Thinking back to the spatial plot above, how do you think these differences might relate to the differing ecology of the two species?

# g. The BCI data frame also contains metadata about some key characteristics of each subplot, including habitat type and environmental heterogeneity
# Explore the data for these two species - what other factors might we need to account for in our sampling or analysis design?

ggplot(dat) + 
  geom_boxplot(aes(x=Habitat, y=count,  fill=species)) + 
  facet_wrap(~species)

ggplot(dat) + geom_point(aes(x=EnvHet, y=count, col=species)) 




# ================= FURTHER EXERCISES ==================

# a. The "vegan" package in R for community ecology, which provides the BCI data as a sample dataset, provides
# functions to calculate various biodiversity indices. Explore patterns of plot-level alpha diversity in the BCI dataset
# (hint: you could look at measures such as species richness, Shannon or Simpson diversity).
# Do the distributions of these different metrics differ? How do they vary between different habitats?

###### LOTS THAT COULD BE EXPLORED HERE - ONE EXAMPLE #######

div = bci %>% 
  dplyr::group_by(site_id) %>%
  dplyr::summarise(sr = sum(count > 0), # SR
                   shannon = vegan::diversity(count, index="shannon"),
                   habitat = head(Habitat, 1))  # shannon

ggplot(div) + geom_boxplot(aes(y=sr))
ggplot(div) + geom_boxplot(aes(y=shannon))

# nicely shows that species richness in young (secondary) veg is similar to old growth
# but diversity measured via shannon (accounting for evenness) is much lower
# i.e. community is much more dominated by a few very abundant species
p1 = div %>% 
  ggplot() + 
  geom_boxplot(aes(habitat, sr), fill="skyblue2") + 
  xlab("Habitat type") + ylab("Species richness") + 
  theme_minimal()
p2 = div %>% 
  ggplot() + 
  geom_boxplot(aes(habitat, shannon), fill="skyblue2") + 
  xlab("Habitat type") + ylab("Shannon diversity") + 
  theme_minimal()
gridExtra::grid.arrange(p1, p2, nrow=1)

########################


# b. ANY OTHER IDEAS?



