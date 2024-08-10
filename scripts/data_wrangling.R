# In this section we will work on data wrangling and manipulation. We will look at how 
# to use functions to filter the data down to what we want and get it into the form we want.


# because we have a project where our current directory is always the base directory
# of the project that we are in, we don't have to worry about setting the working directory

# read the data in

# read.csv is a function that reads in data where the columns are separated by commas
# we highly recommend that you store your data as text files rather than excel files,
# i.e. csv's (comma separated values) or tsv's (tab separated values)

d <- read.csv("data/observations-465928.csv")

# let's take a quick look at what is in the data using str()

str(d)

# we are really only interested in the date observed, longitude, latitude, and scientific name,
# so let's pull out just those columns

dat <- d[,c(2,7:9)]

# let's look at it quick to make sure that we have the columns we want now.

str(dat)

# let's look at the unique values in scientific_name, we want to just be looking
# at species in the "Diplacus" genus
unique(dat$scientific_name)

# how many observations are under each name
table(dat$scientific_name)

# let's filter to have just the Diplacus individuals
dip_dat <- dat[grepl(pattern = "Diplacus", dat$scientific_name), ] # remember to leave the columns blanck to return all columns.

#># challenge to do the above with subset!

# see how many of each species now
table(dip_dat$scientific_name)

# ok, so now we have just the species we want, let's make sure there are no duplicate 
# observations if there are no duplicates

duplicated(dip_dat$latitude)

# the list of T/Fs is hard to look at, let's see how many duplicated columns there are
sum(duplicated(dip_dat$latitude))

# just to make sure these are from the same spots, let's include longitude
sum(duplicated(dip_dat[,c("latitude", "longitude")]))

# ok so some of the duplicated latitudes have different longitudes, let's keep them,
# so we will get rid of everything that has the same latitude and longitude

dip_dat_u <- dip_dat[!duplicated(dip_dat[ , c("latitude", "longitude")]),]

# check it again
str(dip_dat_u)

# let's turn the observed on date, which is characters into something that is more
# easily inturpreted like a julian date

# we need to first change it into a "Date" class instead of a "character"
obs_on <- as.Date(dip_dat_u$observed_on)

class(obs_on)

# now we can change it from YYYY-MM-DD to julian date
julian <- format(obs_on, "%j")

julian

class(julian)

# now we have julian dates, but it is still a character class, so we have to change
# it to numeric so that we can use it in other functions as a numeric value

julian <- as.numeric(julian)

class(julian)

# so now that it's numeric, lets replace the observed_on column in the data frame with
# the julian vector

dip_dat_u$observed_on <- julian

# now we see that the observed on date is of the numeric class
str(dip_dat_u)

# let's make a couple functions to transform the data to make it easier to interpret
# first let's make a formula that standardizes the data

standardize <- function(data){
  (data - mean(data))/sd(data)
}

# let's look at the julian observation dates
hist(dip_dat_u$observed_on)

# now let's standardize it so that it has a mean of 0 and standard deviation of 1
dip_dat_u$observed_on <- standardize(dip_dat_u$observed_on)

hist(dip_dat_u$observed_on)

# the data still has the same information, it is just a little easier to interpret
cor(julian, dip_dat_u$observed_on)

# now let's make a function that will take latitude and longitude and put them on a 
# scale from 0 to 1

per_max <- function(x){
  (x - min(x))/(max(x) - min(x))
}

hist(dip_dat_u$longitude)

dip_dat_u$longitude <- per_max(dip_dat_u$longitude)

hist(dip_dat_u$longitude)

dip_dat_u$latitude <- per_max(dip_dat_u$latitude)

# finally, we need to convert the species names to factors
str(dip_dat_u)

dip_dat_u$scientific_name <- factor(dip_dat_u$scientific_name)

str(dip_dat_u)

# ok, now we can try to look for some patterns

# do the different species flower (well, are observed actually) at different times?
lm(observed_on ~ scientific_name - 1, data = dip_dat_u)

plot(observed_on ~ scientific_name, data = dip_dat_u)

# is observation time related to latitude? Longitude

lat_mod <- lm(observed_on ~ latitude, data = dip_dat_u)

summary(lat_mod)

plot(observed_on ~ latitude, data = dip_dat_u)
abline(a = coef(lat_mod)[1], b = coef(lat_mod)[2], col = "red")

lon_mod <- lm(observed_on ~ longitude, data = dip_dat_u)

plot(observed_on ~ longitude, data = dip_dat_u)
abline(a = coef(lon_mod)[1], b = coef(lon_mod)[2], col = "red")

# we may want to save this data set for later analysis, so let's write it as a csv in the data directory

write.csv(dip_dat_u, "data/dip_dat.csv", row.names = F)
