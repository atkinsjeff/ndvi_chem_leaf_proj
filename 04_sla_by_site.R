# making SLA by site test run
require(tidyverse)

prop <- read.csv("./data/plot_spp_prop.csv")

# these are all in m^2 per kg
sla <- read.csv("./data/umbs_sla_spp.csv")

head(prop)

# make it a long boi
prop %>%
gather( key = "spp", value = "prop", -plot_id) -> df

# bring in SLA
df$sla <- sla$sla_kg_m2[match(df$spp, sla$species)]

# make prop sla
df$prop.sla <- df$prop * df$sla

# make plot level SLA
df %>%
  group_by(plot_id) %>%
  summarize(plot.sla = sum(prop.sla)) -> df.2

# goes to g per m^2
df.2$lma <- (1 / df.2$plot.sla) * 1000

