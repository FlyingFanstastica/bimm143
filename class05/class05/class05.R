----
title: "CLass 05"
author: "Xiao"
date: "04/18/19"
output: github_document
----

# Class 5 R graphics

# 2A. Line plot
weight <- read.table("bimm143_05_rstats/weight_chart.txt", header = TRUE)
plot(weight$Age, weight$Weight, typ = "o", 
     pch = 15, cex = 1.5, lwd = 2, ylim = c(2, 10), 
     xlab = "Age (months)", ylab = "Weight (kg)", 
     main = "Baby weight with age") 

# 2B. Barplot
mouse <- read.table("bimm143_05_rstats/feature_counts.txt", 
                    sep = "\t",header = TRUE)

barplot(mouse$Count, names.arg = mouse$Feature, ylab="",
        main = "Number of features in the mouse GRCm38 genome", horiz = TRUE,
        las = 1, xlim = c(0,80000))

# 2C. Histograms
hist(c(rnorm(10000),rnorm(10000)+4), breaks = 1000)

# Section 3
mf <- read.delim("bimm143_05_rstats/male_female_counts.txt")
barplot(mf$Count, names.arg = mf$Sample, col = rainbow(nrow(mf)), 
        las = 2, ylab = "Counts")
