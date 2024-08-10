# In this section the goal will be visualizing data in order to have a better understanding of the variable
# distributions and the relationships between them.


#Loading the dataset

# We will be using a sample data set which is provided in the 'datasets' package. The dataset shows the weight of
# a sample of chickens reared on different types of feed.

# First we will use the data() function to create a temporary variable which holds our data and then save it to
# my.chickens so we can have an easier time retrieving this data later on

library(datasets)
data("chickwts")
my.chickens <- chickwts

# In order to visualize how many chickens are in each feed group, we should get the percentage of the total for
# each feed category. We will do this with the help of the table() function.

table(my.chickens$feed)
n <- nrow(my.chickens)
percent_feed <- table(my.chickens$feed)/n*100


# The best way to visualize these percentages would be using the barplot() function

barplot(percent_feed, ylim=c(0,25), ylab = 'Percent')

# Lets try to make it a little more informative by adding titles to our axes using the ylab and main arguments 
# from the barplot() function

barplot(percent_feed, ylim=c(0,25), ylab = 'Percent (%)', main = 'Percent feed given to chickens')

# Notice that not all bars have a x axis label, this is because R is printing the names horizontally there is no 
# space to print all 6 of our groups. One way to get around this is by printing them vertically which we can do
# using the las argument. By default las=1, we will try with las=2

barplot(percent_feed, ylim=c(0,25), ylab = 'Percent (%)', main = 'Percent feed given to chickens', las = 2)

# Finally we can lven up the plot by changing the colors using the col argument

barplot(percent_feed, ylim=c(0,25), ylab = 'Percent (%)', main = 'Percent feed given to chickens', las = 2, col = 'pink')

# If you want to be a little more specific with your colors, you can use the rgb() (red,green,blue) function as such

barplot(percent_feed, ylim=c(0,25), ylab = 'Percent (%)', main = 'Percent feed given to chickens', las = 2, col = rgb(1,0,0,0.35)) # the fourth number in the rgb() function indicates transparency, very useful for certain situations

# Next, we can use the hist() function to create a histogram in order to get a sense of the distribution of our weights

# The breaks argument sets our desired binsize while xlim and ylim sets the range of our axes

hist(my.chickens$weight, breaks= 20, col = 'red', xlim = c(0,500), ylim=c(0,15))

# We can also put two histograms in one plot by using the add = TRUE argument and subsetting our table. 
# Remember to set the axis range accordingly and to use the transparency option in the rgb() function.
# Make sure to adjust the titles and figure legend (the legend() function) so people know what they are looking at

linseed <- subset(my.chickens, my.chickens$feed == 'linseed')
casein <- subset(my.chickens, my.chickens$feed == 'casein')

hist(linseed$weight, col = rgb(1,0,0,0.25), xlim = c(0,600), ylim = c(0,6), main = 'Overlapping histograms of linseed and casein feeds', xlab = 'Weight (g)', )
hist(casein$weight, col = rgb(0,0,1,0.25),add = T)
legend("topright", c('Linseed','Casein'), col = c("red", "blue"), lwd = 10)

# We can use the boxplot function to show which type of feed made the bigger chickens on average
# Remember to adjust the axes titles as before
boxplot(weight ~ feed, data = my.chickens, las = 2, main = 'Weight of Chicken by Feed', xlab = '', ylab = 'Weight (g)')

# We could color each of the groups the same color by just assigning col to a specific color, but we want each of
# the groups to have their own. One way to do this would be to set a color vector as such

boxplot.colors <- c('red','blue','hotpink','purple','cyan','yellow')
boxplot(weight ~ feed, data = my.chickens, las = 2, main = 'Weight of Chicken by Feed', xlab = '', ylab = 'Weight (g)', col = boxplot.colors)

# To save our made plots, I use the pdf() function. First you tell R using the pdf() that you want to create a
# new pdf file, the file argument is used to name your pdf.

pdf(file = 'figures/my.best.plots.pdf', useDingbats = F) # Always tell it to not use DingBats as it may mess up some of
                                                 # your plots when you open them in pdf

# Then you create any plot as you did before. If you make multiple before closing the pdf, the pdf file will
# have multiple pages. 

boxplot.colors <- c('red','blue','hotpink','purple','cyan','yellow')

barplot(percent_feed, ylim=c(0,25), ylab = 'Percent (%)', main = 'Percent feed given to chickens', las = 2, col = boxplot.colors)

hist(my.chickens$weight, breaks= 20, col = 'red', xlim = c(0,500), ylim=c(0,15))

boxplot(weight ~ feed, data = my.chickens, las = 2, main = 'Weight of Chicken by Feed', xlab = '', ylab = 'Weight (g)', col = boxplot.colors)

2+2 # Anything that doesn't create new plots wont affect your pdf such as this line here

# Lastly, we need to tell R that we are done editing this pdf and we want it to save what we plotted there so far
# To do this, we just need to run the following line
dev.off()


