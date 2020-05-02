

# Load data
data("quakes")


# Scatterplot Matrices
require(graphics)
pairs(quakes, main = "Fiji Earthquakes, N = 1000", cex.main = 1.2, pch = ".")

# First 10 quakes 
head(quakes, 10)

# Help
?quakes

summary(quakes$mag) # minimum, first quartile, median, mean, third quartile, maximum
var(x = quakes$mag) # dispersion
sd(x = quakes$mag) # standard deviation
boxplot(quakes$mag, main = "Magnitutde", col = "blue", outcol = "red")


summary(quakes$depth) # minimum, first quartile, median, mean, third quartile, maximum
var(x = quakes$depth) # dispersion
sd(x = quakes$depth) # standard deviation
boxplot(quakes$depth, main = "Depth", col = "green", outcol = "red")

?quakes

summary(quakes$stations) # minimum, first quartile, median, mean, third quartile, maximum
var(x = quakes$stations) # dispersion
sd(x = quakes$stations) # standard deviation
boxplot(quakes$stations, main = "Depth", col = "green", outcol = "red")



