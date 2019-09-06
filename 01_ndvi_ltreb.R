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

#adding age
df$stand.age <- 2019 - df$est_year

# make total lai
head(df[,15:19])

df$lai <- rowMeans(df[,15:19], na.rm=TRUE)

# scale CANOPY N using LMA generic 54.1 g per m^2 from Abrams and Kubiske (og is 5.41 mg per cm^2)
df$can.N <- ((df$nitrogen * 54.1) / 100 ) * df$lai
 
# chl adjust by lai
df$chl_b_est <- df$chl_b * df$lai

# let's look at LAI vs NDVI and address the autocorrelation issue first
lm.lai.ndvi <- lm(NDVI_IndexAvg ~ lai, data = df)
summary(lm.lai.ndvi)

# intercept
lm.lai.ndvi[[1]][1]

# slope
lm.lai.ndvi[[1]][2]


# now I want to add that equation to the plot by hand
# I know we can do auto, I just want to type it all out.
lai.fun <- function(x) (x * 0.2024) + 0.072727

# test plot
x11()
ggplot(df, aes(x = lai, y = NDVI_IndexAvg, color = Plot))+
  geom_point(size = 4)+
  ylab("LAI")+
  xlab("NDVI")+
  stat_function(fun = lai.fun)

x11()
ggplot(df, aes(x = stand.age, y = NDVI_IndexAvg, color = Plot))+
  geom_point(size = 4)+
  ylab("NDVI")+
  xlab(" Stand Age")

x11()
ggplot(df, aes(x = stand.age, y = can.N, color = Plot))+
  geom_point(size = 4)+
  ylab("Canopy N")+
  xlab(" Stand Age")

x11()
ggplot(df, aes(x = stand.age, y = chl_b_est, color = Plot))+
  geom_point(size = 4)+
  ylab("Chl B")+
  xlab(" Stand Age")

# let's remove residuals now
df$ndvi.resid <- abs( ((df$lai * 0.2024) + 0.072727) - df$NDVI_IndexAvg)

#let's plot these
x11()
ggplot(df, aes(y = ndvi.resid, x = Plot))+
  geom_boxplot()

#let's plot these
x11()
ggplot(df, aes(y = can.N, x = Plot))+
  geom_boxplot()

# test stats
lm.chl <- lm(chl_b_est ~ NDVI_IndexAvg, data = df)
summary(lm.chl)

lm.can.N <- lm(can.N ~ NDVI_IndexAvg, data = df)
summary(lm.can.N)



################# 
# let's look at plot averages

df %>%
  group_by(Plot) %>%
  summarise(ndvi = mean(NDVI_IndexAvg), ndvi.sd = sd(NDVI_IndexAvg),
            chl_b = mean(chl_b_est, na.rm = TRUE), chl_b.sd = sd(chl_b_est, na.rm = TRUE),
            nitro = mean(can.N, na.rm = TRUE), nitro.sd = sd(can.N, na.rm = TRUE)) -> df.means


lm.chl.means <- lm(chl_b ~ ndvi, data = df.means)
summary(lm.chl.means)

lm.can.N.means <- lm(nitro ~ ndvi, data = df.means)
summary(lm.can.N.means)

# plot chl_b
x11()
ggplot(df.means, aes(y = chl_b, x = ndvi))+
  geom_point(size = 4)+
  geom_pointrange(aes(ymin = chl_b - chl_b.sd, ymax = chl_b + chl_b.sd))+ 
  ylab("Chl B")+
  xlab("NDVI")

# plot canopy N
x11()
ggplot(df, aes(y = can.N, x = NDVI_IndexAvg))+
  geom_point(size = 4)+
  #geom_pointrange(aes(ymin = chl_b - chl_b.sd, ymax = chl_b + chl_b.sd))+ 
  ylab("Canopy N (g m^-2)")+
  xlab("NDVI")

x11()
ggplot(df, aes(y = chl_b_est, x = NDVI_IndexAvg))+
  geom_point(size = 4)+
  #geom_pointrange(aes(ymin = chl_b - chl_b.sd, ymax = chl_b + chl_b.sd))+ 
  ylab("Chl B (g m^-2)")+
  xlab("NDVI")

x11()
ggplot(df.means, aes(y = nitro, x = ndvi))+
  geom_point(size = 4)+
  #geom_pointrange(aes(ymin = chl_b - chl_b.sd, ymax = chl_b + chl_b.sd))+ 
  ylab("Canopy N (g m^-2)")+
  xlab("NDVI")

x11()
ggplot(df.means, aes(y = nitro, x = chl_b))+
  geom_point(size = 4)+
  #geom_pointrange(aes(ymin = chl_b - chl_b.sd, ymax = chl_b + chl_b.sd))+ 
  ylab("Canopy N (g m^-2)")+
  xlab("Chl B")
