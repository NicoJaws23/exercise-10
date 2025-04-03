library(tidyverse)
library(ggplot2)
library(cowplot)
library(infer)
library(skimr)
#Still using bird data
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/AVONETdataset1.csv"
d <- read_csv(f, col_names = TRUE)
#Selecting data and mutating migration
d <- d |>
  select(Species1, Family1, Order1, Beak.Length_Culmen, Beak.Width, Beak.Depth, 
         Tarsus.Length, Wing.Length, Tail.Length, Mass, Habitat, Migration, Trophic.Level, 
         Trophic.Niche, Min.Latitude, Max.Latitude, Centroid.Latitude, Primary.Lifestyle, Range.Size) |>
  mutate(Migration = as.factor(Migration)) |>
  mutate(Beak.Length_CulmenLog = log(Beak.Length_Culmen), Tarsus.LengthLog = log(Tarsus.Length))
skim(d)

#challenge 1
#Step 1
#Boxplots
#plots, use drop_na to get rid of na observations
#trophic level in relation to log(Mass)
tlM <- ggplot(data = d |> drop_na(Trophic.Level), aes(x = Trophic.Level, y = log(Mass))) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("log(Mass) and Trophic Level")

#Migration (as factor) in relation to log(Mass)
mm <- ggplot(data = d |> drop_na(Migration), aes(x = Migration, y = log(Mass))) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("log(Mass) and Migration")

plot_grid(tlM, mm)

#Step 2, linear models
#Mass and trophic level model
m1 <- lm(log(Mass) ~ Trophic.Level, data = d)
#Madd and Migration model
m2 <- lm(log(Mass) ~ Migration, data = d)

#Step 3, Post-hoc Tukey HSD
m2aov <- aov(log(Mass) ~ Migration, data = d)
posthoc <- TukeyHSD(m2aov, which = "Migration", conf.level = 0.95)

#Step 4, F-stat perm w/ {infer}
original.F <- aov(log(Mass) ~ Trophic.Level, data = d) |>
  broom::tidy()|>
  filter(term == "Trophic.Level")
original.F #F stat is "statistic"

#Permutation distribution using {infer}
d <- d |> mutate(logMass = log(Mass))
permuted.F <- d |>
  specify(logMass ~ Trophic.Level) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate(stat = "F")
visualize(permuted.F) +
  shade_p_value(obs_stat = original.F$statistic, direction = "greater")
p.value <- permuted.F |>
  get_p_value(obs_stat = original.F$statistic, direction = "greater")


#Challenge 2
#Step 1 Calculate residules
m3 <- lm(Beak.Length_CulmenLog ~ log(Mass), data = d)
m4 <- lm(Tarsus.LengthLog ~ log(Mass), data = d)
d <- d |>
  mutate(blcRes = m3$residuals, tlRes = m4$residuals) |>
  mutate(Primary.Lifestyle = factor(Primary.Lifestyle, levels = c("Aerial", "Aquatic",
  "Insessorial", "Terrestrial", "Generalist"))) |>
  mutate(Trophic.Niche = factor(Trophic.Niche, levels = c("Nectarivore", "Herbivore aquatic", "Frugivre", "Granivore", 
                                                          "Herbivore terrestrial", "Aquatic predator", "Invertivore", "Vertivore", "Scavenger",
                                                          "Omnivore")))


#Step 2 plots
a <- ggplot(data = d, mapping = aes(x = Primary.Lifestyle, y = tlRes)) +
  geom_boxplot() +
  ggtitle("Relative Tarsus Length and Primary Lifestyle") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

b <- ggplot(data = d |> drop_na(Trophic.Niche), mapping = aes(x = Trophic.Niche, y = blcRes)) +
  geom_boxplot() +
  ggtitle("Relative Beak Length and Trophic Niche") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_grid(a, b)
#Step 3, anova on range size and migration
#Dist of range.size is wack, log() it
d <- d |>
  mutate(logRange = log(Range.Size))

d$Migration <- relevel(d$Migration, ref = 1)
rangM1 <- lm(logRange ~ Migration, data = d)
summary(rangM1)
#Releveling to see whats different
d$Migration <- relevel(d$Migration, ref = 2)
rangM2 <- lm(logRange ~ Migration, data = d)
summary(rangM2)

d$Migration <- relevel(d$Migration, ref = 3)
rangM3 <- lm(logRange ~ Migration, data = d)
summary(rangM3)

#Relevel back to og
d$Migration <- relevel(d$Migration, ref = 1)
#Tukey HSD
rangM_aov <- aov(logRange ~ Migration, data = d)
rangeM_THSD <- TukeyHSD(rangM_aov, which = "Migration", conf.level = 0.95)

#Step 4, Passeriformes analysis
dp <- d |>
  filter(Order1 == "Passeriformes") |>
  mutate(Trophic.Level = as.factor(Trophic.Level))

p1 <- ggplot(data = dp, mapping = aes(x = Primary.Lifestyle, y = blcRes)) +
  geom_boxplot() +
  ggtitle("Rel Beak Length:Primary Lifestyle")
p2 <- ggplot(data = dp, mapping = aes(x = Trophic.Level, y = blcRes)) +
  geom_boxplot() +
  ggtitle("Rel Beak Length:Trophic Level")
plot_grid(p1, p2)

dp <- dp |>
  mutate(PL.TL = paste(Primary.Lifestyle, Trophic.Level))
p3 <- ggplot(data = dp, mapping = aes(x = PL.TL, y = blcRes)) +
  geom_boxplot() +
  ggtitle("Rel Beak Length: Lifestyle & Trophic Level")
p3

Plm1 <- lm(blcRes ~ Primary.Lifestyle, data = dp)
summary(Plm1)
Plm2 <- lm(blcRes ~ Trophic.Level, data = dp)
summary(Plm2)

#Step 5, two factor model
P2wayaov <- aov(data = dp, blcRes ~ Primary.Lifestyle + Trophic.Level)
summary(P2wayaov)

#Step 6, two factor with interaction
PaovInteraction <- aov(data = dp, 
                       blcRes ~ Primary.Lifestyle + Trophic.Level + Primary.Lifestyle:Trophic.Level)
summary(PaovInteraction)

#Step 7, interaction plot, visuzalise interaction between PL and TL
interaction.plot(x.factor= dp$Primary.Lifestyle, xlab = "Primary Lifestyle", trace.factor = dp$Trophic.Level,
                 trace.label = "Trophic Level", fun = base::mean, response = dp$blcRes, ylab = "Relative Beak Length")

#Step 8, comparing largest and smallest within-grouping level sd
#useing for "equal" variances, plots first
#log(Range) and migration
rM <- ggplot(data  = d |> drop_na(Migration), mapping = aes(x = Migration, y = logRange)) +
  geom_violin() +
  ggtitle("All Species, Geographic Range and Migration")
mig <- d |>
  drop_na(Migration) |>
  drop_na(logRange) |>
  group_by(Migration) |>
  summarize(sdRange = sd(logRange))
s <- max(mig$sdRange)/min(mig$sdRange)

#Rel beak length and Primary Lifestyle
bPL <- ggplot(data = dp, mapping = aes(x = Primary.Lifestyle, y = blcRes)) +
  geom_violin() +
  ggtitle("Passeriformes, Relative Beak Length and Primary Lifestyle") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
life <- dp |>
  drop_na(Primary.Lifestyle) |>
  group_by(Primary.Lifestyle) |>
  summarize(sdBeak = sd(blcRes))
p <- max(life$sdBeak)/min(life$sdBeak)

#Rel beak length and Trophic Level
bTL <- ggplot(data = dp, mapping = aes(x = Trophic.Level, y = blcRes)) +
  geom_violin() +
  ggtitle("Passeriformes, Relative Beak Length and Trophic Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
troph <- dp |>
  drop_na(Trophic.Level) |>
  group_by(Trophic.Level) |>
  summarize(sdBeak = sd(blcRes))
t <- max(troph$sdBeak)/min(troph$sdBeak)


plot_grid(rM, bPL, bTL)
print(paste("Migration Varience:", s, "; Primary Lifestyle Varience:", p, "; Trophic Level Varience:", t))

#Plots of residules
#Migrarion and range size
hist(rangM1$residuals)
#Lifestyle and beak size
hist(Plm1$residuals)
#Trophic level and beak size
hist(Plm2$residuals)
