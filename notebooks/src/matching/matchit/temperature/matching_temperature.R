library("MatchIt")

# for plotting
library("ggplot2")

# for analysis
library("lmtest")
library("sandwich")

matching <- read.csv(file = 'df_matching.csv')

# nearest neighbor
m.out1 <- matchit(temperature_treated ~ High.school.graduation.raw.value + Median.household.income.raw.value + 
                   Some.college.raw.value + Unemployment.raw.value +
                   RUCC + Ratio.of.population.to.mental.health.providers + Average.Precipitation,
                 data = matching, method = "nearest", distance = "glm", replace = TRUE,
                 reuse.max = 4, caliper = 0.125)
summary(m.out1)
plot(m.out1, type = "jitter", interactive = FALSE)

# See https://kosukeimai.github.io/MatchIt/articles/estimating-effects.html

# Poor mental health days
gm <- get_matches(m.out1)
table(table(gm$id[gm$temperature_treated == 0]))

fitgm <- lm(Poor.mental.health.days.raw.value ~ temperature_treated + High.school.graduation.raw.value + Median.household.income.raw.value + Some.college.raw.value + Unemployment.raw.value + RUCC + Ratio.of.population.to.mental.health.providers + Average.Precipitation,
            data = gm,
            weights = weights)

coeftest(fitgm, vcov. = vcovCL, cluster = ~subclass + id)["temperature_treated",,drop = FALSE]
gm$temperature_treated <- as.factor(gm$temperature_treated)
ggplot(data = gm, mapping = aes(y = temperature_treated, x = Poor.mental.health.days.raw.value)) +
  geom_violin() +
  geom_boxplot(width=0.1)

# Suicide rate
gm2 <- get_matches(m.out1)
table(table(gm2$id[gm2$temperature_treated == 0]))

fitgm2 <- lm(Crude.Rate ~ temperature_treated + High.school.graduation.raw.value + Median.household.income.raw.value + Some.college.raw.value + Unemployment.raw.value + RUCC + Ratio.of.population.to.mental.health.providers + Average.Precipitation,
             data = gm2, 
             weights = weights)

coeftest(fitgm2, vcov. = vcovCL, cluster = ~subclass + id)["temperature_treated",,drop = FALSE]
gm2$temperature_treated <- as.factor(gm2$temperature_treated)
ggplot(data = gm2, mapping = aes(y = temperature_treated, x = Crude.Rate)) +
  geom_violin() +
  geom_boxplot(width=0.1)