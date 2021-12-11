library("MatchIt")

# for plotting
library("ggplot2")

# for analysis
library("lmtest")
library("sandwich")

matching <- read.csv(file = 'df_matching.csv')

# nearest neighbor
m.out1 <- matchit(precipitation_treated ~ High.school.graduation.raw.value + Median.household.income.raw.value + 
                   Some.college.raw.value + Unemployment.raw.value +
                   RUCC + Ratio.of.population.to.mental.health.providers + Average.Temperature,
                 data = matching, method = "nearest", distance = "glm", replace = TRUE,
                 reuse.max = 6, caliper = 0.275)
summary(m.out1)
plot(m.out1, type = "jitter", interactive = FALSE)

# See https://kosukeimai.github.io/MatchIt/articles/estimating-effects.html

# Poor mental health days
gm <- get_matches(m.out1)
table(table(gm$id[gm$precipitation_treated == 0]))

fitgm <- lm(Poor.mental.health.days.raw.value ~ precipitation_treated + High.school.graduation.raw.value + Median.household.income.raw.value + Some.college.raw.value + Unemployment.raw.value + RUCC + Ratio.of.population.to.mental.health.providers + Average.Temperature,
            data = gm,
            weights = weights)

coeftest(fitgm, vcov. = vcovCL, cluster = ~subclass + id)["precipitation_treated",,drop = FALSE]
gm$precipitation_treated <- as.factor(gm$precipitation_treated)
ggplot(data = gm, mapping = aes(y = precipitation_treated, x = Poor.mental.health.days.raw.value)) +
  geom_violin() +
  geom_boxplot(width=0.1)

# Suicide rate
gm2 <- get_matches(m.out1)
table(table(gm2$id[gm2$precipitation_treated == 0]))

fitgm2 <- lm(Crude.Rate ~ precipitation_treated + High.school.graduation.raw.value + Median.household.income.raw.value + Some.college.raw.value + Unemployment.raw.value + RUCC + Ratio.of.population.to.mental.health.providers + Average.Temperature,
             data = gm2, 
             weights = weights)

coeftest(fitgm2, vcov. = vcovCL, cluster = ~subclass + id)["precipitation_treated",,drop = FALSE]
gm2$precipitation_treated <- as.factor(gm2$precipitation_treated)
ggplot(data = gm2, mapping = aes(y = precipitation_treated, x = Crude.Rate)) +
  geom_violin() +
  geom_boxplot(width=0.1)