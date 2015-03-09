####
rm(list=ls(all=TRUE))
setwd("D:/Rpresentation")
getwd()
FDIdata = read.csv("FDI.csv")  # read csv file
par(mar=c(5,5,5,5))
plot(FDIdata$Number, type="o", col="blue", axes=FALSE, ann=FALSE)
axis(1, at=1:24, lab=c("1988-1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","Prel 2013"))
g_range <- range(0, 1600)
axis(2, las=1, at=300*0:g_range[2])
par(new=T)
plot(FDIdata$Registered, type="o", col="red", axes=FALSE, ann=FALSE, ylim=c(0,75000))
g_range2 <- range(0, 75000)
axis(4, las=1, at=15000*0:g_range2[2],cex.axis=0.75)
box()
# add a title for the right axis 
mtext("FDI Registered in Mil USD", side=4, line=3, cex.lab=1,las=0, col="red")
# add a title for the left axis 
mtext("Number of FDI Projects in Vietnam", side=2, line=3, cex.lab=1,las=0, col="blue") 
# add a main title and bottom.
title("FDI in Vietnam by the Numbers", xlab="Years")

####
# Read car and truck values from tab-delimited autos.dat
autos_data <- read.table("autos.dat", header=T, sep="\t") 

# Compute the largest y value used in the data (or we could
# just use range again)
max_y <- max(autos_data)

# Define colors to be used for cars, trucks, suvs
plot_colors <- c("blue","red","forestgreen")

# Start PNG device driver to save output to figure.png
###png(filename="D:/Rpresentation/figure.png", height=295, width=300, 
### bg="white")

# Graph autos using y axis that ranges from 0 to max_y.
# Turn off axes and annotations (axis labels) so we can 
# specify them ourself
plot(autos_data$cars, type="o", col=plot_colors[1], 
   ylim=c(0,max_y), axes=FALSE, ann=FALSE)

# Make x axis using Mon-Fri labels
axis(1, at=1:5, lab=c("Mon", "Tue", "Wed", "Thu", "Fri"))

# Make y axis with horizontal labels that display ticks at 
# every 4 marks. 4*0:max_y is equivalent to c(0,4,8,12).
axis(2, las=1, at=4*0:max_y)

# Create box around plot
box()

# Graph trucks with red dashed line and square points
lines(autos_data$trucks, type="o", pch=22, lty=2, 
   col=plot_colors[2])

# Graph suvs with green dotted line and diamond points
lines(autos_data$suvs, type="o", pch=23, lty=3, 
   col=plot_colors[3])

# Create a title with a red, bold/italic font
title(main="Autos", col.main="red", font.main=4)

# Label the x and y axes with dark green text
title(xlab= "Days", col.lab=rgb(0,0.5,0))
title(ylab= "Total", col.lab=rgb(0,0.5,0))

# Create a legend at (1, max_y) that is slightly smaller 
# (cex) and uses the same line colors and points used by 
# the actual plots
legend(1, max_y, names(autos_data), cex=0.8, col=plot_colors, 
   pch=21:23, lty=1:3);

####
# Define the cars vector with 5 values
cars <- c(1, 3, 6, 4, 9)
# Graph cars
barplot(cars)

####
# Read values from tab-delimited autos.dat 
autos_data <- read.table("autos.dat", header=T, sep="\t")
   
# Graph autos with adjacent bars using rainbow colors
barplot(as.matrix(autos_data), main="Autos", ylab= "Total",
   beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("Mon","Tue","Wed","Thu","Fri"), cex=0.6, 
   bty="n", fill=rainbow(5));

####
# Read values from tab-delimited autos.dat
autos_data <- read.table("autos.dat", header=T, sep="\t") 

# Expand right side of clipping rect to make room for the legend
par(xpd=T, mar=par()$mar+c(0,0,0,4))

# Graph autos (transposing the matrix) using heat colors,  
# put 10% of the space between each bar, and make labels  
# smaller with horizontal y-axis labels
barplot(t(autos_data), main="Autos", ylab="Total", 
   col=heat.colors(3), space=0.1, cex.axis=0.8, las=1,
   names.arg=c("Mon","Tue","Wed","Thu","Fri"), cex=0.8) 
   
# Place the legend at (6,30) using heat colors
legend(6, 30, names(autos_data), cex=0.8, fill=heat.colors(3));
   
# Restore default clipping rect
par(mar=c(5, 4, 4, 2) + 0.1)


####
# Get a random log-normal distribution
r <- rlnorm(1000)
# Get the distribution without plotting it using tighter breaks
h <- hist(r, plot=F, breaks=c(seq(0,max(r)+1, .1)))
# Plot the distribution using log scale on both axes, and use
# blue points
plot(h$counts, log="xy", pch=20, col="blue",
	main="Log-normal distribution",
	xlab="Value", ylab="Frequency")

####
# Define cars vector with 5 values
cars <- c(1, 3, 6, 4, 9)
# Create a pie chart with defined heading and
# custom colors and labels
pie(cars, main="Cars", col=rainbow(length(cars)),
   labels=c("Mon","Tue","Wed","Thu","Fri"))

####
# Define cars vector with 5 values
cars <- c(1, 3, 6, 4, 9)
# Define some colors ideal for black & white print
colors <- c("white","grey70","grey90","grey50","black")
# Calculate the percentage for each day, rounded to one 
# decimal place
car_labels <- round(cars/sum(cars) * 100, 1)
# Concatenate a '%' char after each value
car_labels <- paste(car_labels, "%", sep="")
# Create a pie chart with defined heading and custom colors
# and labels
pie(cars, main="Cars", col=colors, labels=car_labels,
   cex=0.8)
# Create a legend at the right   
legend(1.5, 0.5, c("Mon","Tue","Wed","Thu","Fri"), cex=0.8, 
   fill=colors)

####
# Read values from tab-delimited autos.dat
autos_data <- read.table("autos.dat", header=T, sep="\t")

# Create a colored dotchart for autos with smaller labels
dotchart(t(autos_data), color=c("red","blue","darkgreen"),
   main="Dotchart for Autos", cex=0.8)

#### Using ggplot for advance graphs
library(gcookbook)
library(ggplot2)
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb, colour=sex)) +
geom_point(alpha=.5) +
scale_size_area() + # Make area proportional to numeric value
scale_colour_brewer(palette="Set1")

####Mulitple plot on the same graph using ggplot
library(gridExtra)
# This example uses the ChickWeight dataset, which comes with ggplot2
# First plot
p1 <- 
    ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
    geom_line() +
    ggtitle("Growth curve for individual chicks")

# Second plot
p2 <- 
    ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) +
    geom_point(alpha=.3) +
    geom_smooth(alpha=.2, size=1) +
    ggtitle("Fitted growth curve per diet")

# Third plot
p3 <- 
    ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet)) +
    geom_density() +
    ggtitle("Final weight, by diet")

# Fourth plot
p4 <- 
    ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) +
    geom_histogram(colour="black", binwidth=50) +
    facet_grid(Diet ~ .) +
    ggtitle("Final weight, by diet") +
    theme(legend.position="none")        # No legend (redundant in this graph)
grid.arrange(p1,p2,p3,p4, ncol = 2, main = "Main title")

## Set up data to plot with two ordinates - points and lines
#line.x <- 2:10
#bar.x <- 2:10
bar.y <- seq(3,7,by=0.5)+rnorm(9)
#bar.x <- 1:15
bar.x <- 2:10
line.x <- 1:15
line.y <- rev(60:74)+rnorm(15)
x.range <- range(bar.x, line.x)
## Plot the data
par(mar=c(5,4,4,4)+0.1)    ## Make enough room for both labels
plot(y=bar.y, x=bar.x, type='h', lwd=25, lend=4, xlim=x.range, xlab="Sequence", ylab="", main="Plot with two ordinates - points and lines")
par(new=TRUE)
plot(y=line.y, x=line.x, col='red', type='o', xlim=x.range, axes=FALSE, xlab='', ylab='')

## Set up the axes and labels
axis(side=4, col='red', labels=FALSE)
at = axTicks(4)
mtext(side = 4, text = at, at = at, col = "red", line = 1) 
## Add x-axis labels; this allows customization of the how far out labels are
mtext(text='Descending values', side=4, line=2, col='red')
mtext(text='Ascending values', side=2, line=2)
