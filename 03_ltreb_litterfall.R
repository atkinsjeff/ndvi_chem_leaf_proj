#
require(ggplot2)
require(tidyverse)


litter <- read.csv("./data/ltreb_litterfall_data.csv")
ndvi.drone <- read.csv("./data/drone_ndvi_clean.csv")
df <- read.csv("./data/ltreb_ndvi.csv")

#################################
# adjusting values in df 
# Rename NDVI column in R
colnames(df)[colnames(df) == "NDVI_IndexAvg"] <- "ndvi"

#adding age
df$stand.age <- 2019 - df$est_year

# add site specfic SLA these values come from vogel and are in units of m^2 / kg
df$sla <- df.2$plot.sla[match(df$plot_id, df.2$plot_id)]

# not to conver thtat to LMA in g per m^2
df$lma <- (1 / df$sla) * 1000

# make total lai
head(df[, 15:19])

df$lai <- rowMeans(df[,15:19], na.rm = TRUE)
df$ci <- rowMeans(df[27:31], na.rm = TRUE)

# scale CANOPY N using LMA generic 54.1 g per m^2 from Abrams and Kubiske (og is 5.41 mg per cm^2)
#df$can.N <- ((df$nitrogen / 100) * 54.1 ) * df$lai

df$can.N <- ((df$nitrogen / 100) * df$lma ) * df$lai

x11()
ggplot(df, aes(x = ndvi, y = can.N, color = plot_id))+
  geom_point(size = 2)

x11()
ggplot(df, aes(x = ndvi, y = chl_b_est, color = plot_id))+
  geom_point(size = 2)

lm.n <- lm(can.N ~ ndvi, data = df)


# chl adjust by lai    LMA here is converted to 0.00541 mg per m^2
# converted to mg per m2 by dividing by 10000
# df$chl_b_est <- ((df$chl_b / 1000) * 0.00541) * df$lai   #converts to mg per 
# df$chl_a_est <- ((df$chl_a / 1000) * 0.00541) * df$lai
df$chl_b_est <- (df$chl_b / 1000) * (df$lma / 10000) * df$lai   #converts to mg per 
df$chl_a_est <- (df$chl_a / 1000) * (df$lma / 10000) * df$lai

# converting ltreb litter (kg ha-1) to lai 
# divide the mass / area value by 10 to get g per m^2
# using LMA generic 54.1 g per m^2 converted to SLA (1/54.1)
litter$lma <- df$lma[match(litter$plot_id, df$plot_id)]
litter$lai <- (litter$leaf_mass / 10) * (1 / litter$lma)  
litter$lai.gen <- (litter$leaf_mass / 10) * (1 / 54.1)  

ggplot(litter, aes(x = lai, y = lai.gen, color = plot_id))+
  geom_point(size = 3)

litter %>%
  group_by(plot_id) %>%
  filter(year >= 2016) %>%
  summarize(lai = mean(lai, na.rm = TRUE), lai.sd = sd(lai, na.rm = TRUE),
            leaf_mass = mean(leaf_mass, na.rm = TRUE), leaf_mass.sd = mean(leaf_mass, na.rm = TRUE)) -> litter.means

litter.means <- data.frame(litter.means)

# lets redo this shit with the litter lai
df$lai.trap <- litter.means$lai[match(df$plot_id, litter.means$plot_id)]
df$mass.trap <- litter.means$leaf_mass[match(df$plot_id, litter.means$plot_id)]

# 
df$can.N.trap <- ((df$nitrogen / 100) * df$lma ) * df$lai.trap
df$can.N.trap.mass <- (df$nitrogen / 100) * (df$mass.trap / 10)

# chl in mg per m by trap biomass in mg per m
df$chl_a_trap <- (df$chl_a / 1000) * (df$mass.trap / 1000000)
df$chl_b_trap <- (df$chl_b / 1000) * (df$mass.trap / 1000000)
x11()

ggplot(df, aes(x = can.N, y = can.N.trap, color = plot_id))+
  geom_point(size = 3)


x11(width = 3, height = 3)
ggplot(df, aes(x = ndvi, y = can.N.trap))+
  geom_point(size = 4, shape = 21, color = "black", fill = "#693476")+
  geom_abline(slope = 1)+
  # xlim(0, 6)+
  # ylim(0, 6)+
  xlab("NDVI")+
  ylab("Canopy N")+
  theme_light()+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  guides(fill = guide_legend(title = NULL))

x11(width = 3, height = 3)
ggplot(df, aes(x = ndvi, y = can.N.trap, color = plot_id))+
  geom_point(size = 4)+
  # xlim(0, 6)+
  # ylim(0, 6)+
  xlab("NDVI")+
  ylab("Canopy N")+
  theme_light()+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  guides(fill = guide_legend(title = NULL))


#################################
# orders plots by stand age
df$plot_id = factor(df$plot_id, levels = unique( df$plot_id[order(df$stand.age)]), ordered = FALSE)

# let's look at some shit
x11()
ggplot(df, aes(x = plot_id, y =can.N, fill = plot_id)) +
  geom_boxplot()+
  xlab("")+
  ylab("Canopy N (%)")+
  theme_light()+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  guides(fill = guide_legend(title = NULL))


x11()
ggplot(df, aes(x = plot_id, y = chl_a, fill = plot_id)) +
  geom_boxplot()+
  xlab("")+
  ylab("Chl A (ug/mg)")+
  theme_light()+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  guides(fill = guide_legend(title = NULL))

x11()
ggplot(df, aes(x = plot_id, y = chl_b, fill = plot_id)) +
  geom_boxplot()+
  xlab("")+
  ylab("Chl B (ug/mg)")+
  theme_light()+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  guides(fill = guide_legend(title = NULL))

x11(width = 4, height = 4)
ggplot(df, aes(x = plot_id, y = can.N, fill = plot_id)) +
  geom_boxplot()+
  xlab("")+
  ylab("Canopy N scaled by camera LAI (g/m^2)")+
  theme_light()+
  ylim(0,8)+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  theme(legend.position = "none")

x11(width = 4, height = 4)
ggplot(df, aes(x = plot_id, y = can.N.trap, fill = plot_id)) +
  geom_boxplot()+
  xlab("")+
  ylab("Canopy N scaled by litter trap LAI (g/m^2)")+
  theme_light()+
  ylim(0,8)+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  theme(legend.position = "none")

x11(width = 4, height = 4)
ggplot(df, aes(x = plot_id, y = can.N.trap.mass, fill = plot_id)) +
  geom_boxplot()+
  xlab("")+
  ylab("Canopy N scaled by litter trap biomass (g/m^2)")+
  theme_light()+
  ylim(0,8)+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  theme(legend.position = "none")



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
# df %>%
#   group_by(plot_id) %>%
#   summarise(ndvi = mean(ndvi), ndvi.sd = sd(ndvi),
#             chl_a = mean(chl_a_est, na.rm = TRUE), chl_a.sd = sd(chl_a_est, na.rm = TRUE),
#             chl_b = mean(chl_b_est, na.rm = TRUE), chl_b.sd = sd(chl_b_est, na.rm = TRUE),
#             nitro = mean(can.N, na.rm = TRUE), nitro.sd = sd(can.N, na.rm = TRUE),
#             lai = mean(lai, na.rm = TRUE), lai.sd = sd(lai, na.rm = TRUE)) -> df.means
df %>%
  group_by(plot_id) %>%
  summarise(ndvi = mean(ndvi, na.rm = TRUE), ndvi.sd = sd(ndvi, na.rm = TRUE),
            chl_a = mean(chl_a_trap, na.rm = TRUE), chl_a.sd = sd(chl_a_trap, na.rm = TRUE),
            chl_b = mean(chl_b_trap, na.rm = TRUE), chl_b.sd = sd(chl_b_trap, na.rm = TRUE),
            nitro = mean(can.N.trap, na.rm = TRUE), nitro.sd = sd(can.N.trap, na.rm = TRUE),
            lai = mean(lai.trap, na.rm = TRUE), lai.sd = sd(lai.trap, na.rm = TRUE)) -> df.means

df.means <- data.frame(df.means)

litter %>%
  group_by(plot_id) %>%
  #filter(year >= 2016) %>%
  summarize(lai = mean(lai, na.rm = TRUE), lai.sd = sd(lai, na.rm = TRUE)) -> litter.means
litter.means <- data.frame(litter.means)

# adding standard error
df.means$ndvi.se <- df.means$ndvi.sd / sqrt(9)
df.means$chl_a.se <- df.means$chl_a.sd / sqrt(5)
df.means$chl_b.se <- df.means$chl_b.sd / sqrt(5)
df.means$nitro.se <- df.means$nitro.sd / sqrt(5)

# plot labels
R2.exp <- expression(paste(" ",R^2 ,"= 0.647"))
n.label <- expression(paste("Canopy N (g ",m^-2,")"))
chla.label <- expression(paste("Chl A (mg ",m^-2,")"))
chlb.label <- expression(paste("Chl B (mg ",m^-2,")"))

x11(width = 3, height = 3)
ggplot(df.means, aes(x = ndvi, y = chl_a))+
  geom_errorbar(aes(ymin = chl_a - chl_a.se, ymax = chl_a + chl_a.se, width = .005))+
  geom_point(size = 4, shape = 21, color = "black", fill = "#fdec51")+
    #geom_errorbarh(aes(xmin = ndvi - ndvi.se, xmax = ndvi + ndvi.se, height = 0.2))+
    # xlim(0, 1)+
  # ylim(0, 1)+
  xlab("Camera NDVI")+
  ylab(chla.label)+
  theme_light()+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  guides(fill = guide_legend(title = NULL))

x11(width = 3, height = 3)
ggplot(df.means, aes(x = ndvi, y = chl_b))+
  geom_errorbar(aes(ymin = chl_b - chl_b.se, ymax = chl_b + chl_b.se, width = .005))+
  geom_point(size = 4, shape = 21, color = "black", fill = "#4da6a3")+
  # xlim(0, 1)+
  # ylim(0, 1)+
  xlab("Camera NDVI")+
  ylab(chlb.label)+
  theme_light()+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  guides(fill = guide_legend(title = NULL))

x11(width = 3, height = 3)
ggplot(df.means, aes(x = ndvi, y = nitro))+
  geom_errorbar(aes(ymin = nitro - nitro.se, ymax = nitro + nitro.se, width = .005))+
  geom_point(size = 4, shape = 21, color = "black", fill = "#693476")+
  geom_abline(slope = 1)+
  # xlim(0, 1)+
  # ylim(0, 1)+
  xlab("Camera NDVI")+
  ylab(n.label)+
  theme_light()+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  guides(fill = guide_legend(title = NULL))

x11(width = 3, height = 3)
ggplot(df.means, aes(x = ndvi, y = nitro, color = plot_id))+
  geom_errorbar(aes(ymin = nitro - nitro.se, ymax = nitro + nitro.se, width = .005))+
  geom_point(size = 4)+
  geom_abline(slope = 1)+
  # xlim(0, 1)+
  # ylim(0, 1)+
  xlab("Camera NDVI")+
  ylab(n.label)+
  theme_light()+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  theme(legend.position = "none")
# how does camera LAI compare to 
lai <- merge(df.means, litter.means, by = "plot_id")

colnames(lai)[colnames(lai) == "lai.x"] <- "cam.lai"
colnames(lai)[colnames(lai) == "lai.y"] <- "trap.lai"

x11(width = 3, height = 3)
ggplot(lai, aes(x = cam.lai, y = trap.lai))+
  geom_point(size = 4, shape = 21, color = "black", fill = "#693476")+
  geom_abline(slope = 1)+
  xlim(0, 6)+
  ylim(0, 6)+
  xlab("Camera LAI")+
  ylab("Trap LAI")+
  theme_light()+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  guides(fill = guide_legend(title = NULL))

x11(width = 3, height = 3)
ggplot(lai, aes(x = ndvi, y = trap.lai))+
  geom_point(size = 4, shape = 21, color = "black", fill = "#693476")+
  geom_abline(slope = 1)+
  # xlim(0, 6)+
  # ylim(0, 6)+
  xlab("NDVI")+
  ylab("Trap LAI")+
  theme_light()+
  theme(  panel.grid.major = element_line(colour = "#e8e9eb"), 
          panel.grid.minor = element_line(colour = "#e8e9eb"))+
  guides(fill = guide_legend(title = NULL))

lm.trap <- lm(trap.lai ~ ndvi, data = lai)
lm.lai <- lm(cam.lai ~ ndvi, data = lai)

lm.nitro <- lm(nitro ~ ndvi, data = df.means)

lm.chla <- lm(chl_a ~ ndvi, data = df.means)
lm.chlb <- lm(chl_b ~ ndvi, data = df.means)

summary(lm.nitro)
summary(lm.chla)
summary(lm.chlb)