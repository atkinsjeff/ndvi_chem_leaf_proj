# Initial sandbox script for NDVI/Leaf chemistry scaling project based on the 2019 REU summer project by Evan Paris
# Author:  jeff atkins (jwatkins6@vcu.edu and @atkinsjeff)

#libraries
require(tidyverse)
require(ggplot2)
require(viridis)

# import data
df <- read.csv("./data/ltreb_ndvi.csv")

#### 
# need to add scaling stuff in here....obviously. 

# make total lai
head(df[,15:19])

df$lai <- rowMeans(df[,15:19], na.rm=TRUE)

# chl adjust by lai
df$chl_b_est <- df$chl_b * df$lai

# test plot
x11()
ggplot(df, aes(x = NDVI_IndexAvg, y = lai, color = Plot))+
  geom_point()

# test stats
lm.lai.ndvi <- lm(NDVI_IndexAvg ~ lai, data = df)
summary(lm.lai.ndvi)

x11()
ggplot(df, aes(x = NDVI_IndexAvg, y = chl_b_est, color = Plot))+
  geom_point()

# test stats
lm.chl_b <- lm(NDVI_IndexAvg ~ chl_b_est, data = df)
summary(lm.chl_b)