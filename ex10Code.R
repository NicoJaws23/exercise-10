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
original.F <- aov(aov(log(Mass) ~ Trophic.Level, data = df)) |>
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
  mutate(blcRes = m3$residuals, tlRes = m4$residuals)

#Step 2 plots
a <- ggplot(data = d, mapping = aes(x = Primary.Lifestyle, y = tlRes)) +
  geom_boxplot() +
  ggtitle("log(Tarsus Length) Residules and Primary Lifestyle")

b <- ggplot(data = d, mapping = aes(x = Trophic.Niche, y = blcRes)) +
  geom_boxplot() +
  ggtitle("log(Beak Length Culmen) Residules and Trophic Niche")

plot_grid(a, b)

#Step 3 
