#
require(ggplot2)
require(tidyverse)


litter <- read.csv("./data/ltreb_litterfall_data.csv")
ndvi.drone <- read.csv("./data/drone_ndvi_clean.csv")
df <- read.csv("./data/ltreb_ndvi.csv")

#################################
# adjusting values in df 
#adding age
df$stand.age <- 2019 - df$est_year

# make total lai
head(df[,15:19])

df$lai <- rowMeans(df[,15:19], na.rm=TRUE)

# scale CANOPY N using LMA generic 54.1 g per m^2 from Abrams and Kubiske (og is 5.41 mg per cm^2)
df$can.N <- ((df$nitrogen * 54.1) / 100 ) * df$lai

# chl adjust by lai
df$chl_b_est <- df$chl_b * df$lai
df$chl_a_est <- df$chl_a * df$lai

# Rename NDVI column in R
colnames(df)[colnames(df) == "NDVI_IndexAvg"] <- "ndvi"
#################################


# let's compare NDVI
# df %>%
#   group_by(plot_id) %>%
#   summarise(ndvi = mean(NDVI_IndexAvg), ndvi.sd = sd(NDVI_IndexAvg),
#             chl_b = mean(chl_b_est, na.rm = TRUE), chl_b.sd = sd(chl_b_est, na.rm = TRUE),
#             nitro = mean(can.N, na.rm = TRUE), nitro.sd = sd(can.N, na.rm = TRUE)) -> df.means

# 10 m!
df %>%
  filter(Distance == 0) %>%
  select(plot_id, ndvi) -> df.center
   
ndvi.drone %>%
  filter(buffer == 5) %>%
  select(plot_id, ndvi) -> df.drone.5
  
# merge

df.10m <- merge(df.center, df.drone.5, by = "plot_id")

colnames(df.10m)[colnames(df.10m) == "ndvi.x"] <- "cam.ndvi"
colnames(df.10m)[colnames(df.10m) == "ndvi.y"] <- "drone.ndvi"

x11(width = 3, height = 3)
ggplot(df.10m, aes(x = cam.ndvi, y = drone.ndvi))+
  geom_point(size = 4, shape = 21, color = "black", fill = "#693476")+
  geom_abline(slope = 1)+
  xlim(0, 1)+
  ylim(0, 1)+
  xlab("Camera NDVI")+
  ylab("Drone NDVI")+
  ggtitle("10 m Plot")+
  theme_light()+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  guides(fill = guide_legend(title = NULL))

# 20 m FOV!
df %>%
  filter(Distance != 10) %>%
  select(plot_id, ndvi) %>%
  group_by(plot_id) %>%
  summarise(ndvi = mean(ndvi)) -> df.c_and_5


ndvi.drone %>%
  filter(buffer != 20) %>%
  select(plot_id, ndvi) %>%
  group_by(plot_id) %>%
  summarise(ndvi = mean(ndvi)) -> df.drone.10

# merge

df.20m <- merge(df.c_and_5, df.drone.10, by = "plot_id")

colnames(df.20m)[colnames(df.20m) == "ndvi.x"] <- "cam.ndvi"
colnames(df.20m)[colnames(df.20m) == "ndvi.y"] <- "drone.ndvi"

x11(width = 3, height = 3)
ggplot(df.20m, aes(x = cam.ndvi, y = drone.ndvi))+
  geom_point(size = 4, shape = 21, color = "black", fill = "#4da6a3")+
  geom_abline(slope = 1)+
  xlim(0, 1)+
  ylim(0, 1)+
  xlab("Camera NDVI")+
  ylab("Drone NDVI")+
  ggtitle("20 m Plot")+
theme_light()+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  guides(fill = guide_legend(title = NULL))

# 40 m FOV!
df %>%
  select(plot_id, ndvi) %>%
  group_by(plot_id) %>%
  summarise(ndvi = mean(ndvi)) -> df.wide


ndvi.drone %>%
  select(plot_id, ndvi) %>%
  group_by(plot_id) %>%
  summarise(ndvi = mean(ndvi)) -> df.drone.wide

# merge

df.40m <- merge(df.wide, df.drone.wide, by = "plot_id")

colnames(df.40m)[colnames(df.40m) == "ndvi.x"] <- "cam.ndvi"
colnames(df.40m)[colnames(df.40m) == "ndvi.y"] <- "drone.ndvi"

x11(width = 3, height = 3)
ggplot(df.40m, aes(x = cam.ndvi, y = drone.ndvi))+
  geom_point(size = 4, shape = 21, color = "black", fill = "#fdec51")+
  geom_abline(slope = 1)+
  xlim(0, 1)+
  ylim(0, 1)+
  xlab("Camera NDVI")+
  ylab("Drone NDVI")+
  ggtitle("40 m Plot")+
theme_light()+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  guides(fill = guide_legend(title = NULL))


########## STATS!

lm.10 <- lm(drone.ndvi ~ cam.ndvi, data = df.10m)
summary(lm.10)

lm.20 <- lm(drone.ndvi ~ cam.ndvi, data = df.20m)
summary(lm.20)


lm.40 <- lm(drone.ndvi ~ cam.ndvi, data = df.40m)
summary(lm.40)



############
# let's look at these values again motherfucker

# let's compare NDVI
df %>%
  group_by(plot_id) %>%
  summarise(ndvi = mean(ndvi), ndvi.sd = sd(ndvi),
            chl_a = mean(chl_a_est, na.rm = TRUE), chl_a.sd = sd(chl_a_est, na.rm = TRUE),
            chl_b = mean(chl_b_est, na.rm = TRUE), chl_b.sd = sd(chl_b_est, na.rm = TRUE),
            nitro = mean(can.N, na.rm = TRUE), nitro.sd = sd(can.N, na.rm = TRUE)) -> df.means

df.means <- data.frame(df.means)

x11(width = 3, height = 3)
ggplot(df.means, aes(x = ndvi, y = chl_b))+
  geom_point(size = 4, shape = 21, color = "black", fill = "#fdec51")+
  geom_abline(slope = 1)+
  # xlim(0, 1)+
  # ylim(0, 1)+
  xlab("Camera NDVI")+
  ylab("Chl B")+
  theme_light()+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  guides(fill = guide_legend(title = NULL))

x11(width = 3, height = 3)
ggplot(df.means, aes(x = ndvi, y = nitro))+
  geom_point(size = 4, shape = 21, color = "black", fill = "#693476")+
  geom_abline(slope = 1)+
  # xlim(0, 1)+
  # ylim(0, 1)+
  xlab("Camera NDVI")+
  ylab("Canopy nitrogen")+
  theme_light()+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  guides(fill = guide_legend(title = NULL))

lm.chlb <- lm(chl_b)