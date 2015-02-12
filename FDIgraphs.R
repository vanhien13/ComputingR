####
# Define cars vector with 5 values
par(mai=c(0.5,0.05,1.2,1.2))
FDInumbers <- c(440505, 26518, 22333, 341207)
# Define some colors
colors <- c("green","blue","yellow","red")
# Calculate the percentage for each day, rounded to one
# decimal place
FDI_labels <- round(FDInumbers/sum(FDInumbers) * 100, 1)
# Concatenate a '%' char after each value
FDI_labels <- paste(FDI_labels, "%", sep="")
# Create a pie chart with defined heading and custom colors
# and labels
pie(FDInumbers, main="FDI Investment in 2013 by Industries", col=colors, labels=FDI_labels,
    cex=0.8, radius = 1)
# Create a legend at the right
legend(1.1, 0.5, c("Agriculture,forestry,fishing
","Mining,quarrying
","Manufacturing
","Everything else"), cex=0.5,
       fill=colors)
