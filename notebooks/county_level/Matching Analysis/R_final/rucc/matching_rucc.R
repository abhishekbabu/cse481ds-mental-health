library("MatchIt")

# for plotting
library("ggplot2")

# for analysis
library("lmtest")
library("sandwich")

matching <- read.csv(file = 'df_matching.csv')

# nearest neighbor
m.out1 <- matchit(rucc_treated ~ High.school.graduation.raw.value + Median.household.income.raw.value + 
                   Some.college.raw.value + Unemployment.raw.value +
                   Average.Precipitation + Ratio.of.population.to.mental.health.providers + Average.Temperature,
                 data = matching, method = "nearest", distance = "glm", replace = TRUE,
                 caliper = 0.3)
summary(m.out1)
plot(m.out1, type = "jitter", interactive = FALSE)

# See https://kosukeimai.github.io/MatchIt/articles/estimating-effects.html

# Poor mental health days
gm <- get_matches(m.out1)
table(table(gm$id[gm$rucc_treated == 0]))

fitgm <- lm(Poor.mental.health.days.raw.value ~ rucc_treated + High.school.graduation.raw.value + Median.household.income.raw.value + Some.college.raw.value + Unemployment.raw.value + Average.Precipitation + Ratio.of.population.to.mental.health.providers + Average.Temperature,
            data = gm,
            weights = weights)

coeftest(fitgm, vcov. = vcovCL, cluster = ~subclass + id)["rucc_treated",,drop = FALSE]
gm$rucc_treated <- as.factor(gm$rucc_treated)
ggplot(data = gm, mapping = aes(y = rucc_treated, x = Poor.mental.health.days.raw.value)) +
  geom_violin() +
  geom_boxplot(width=0.1)

# Suicide rate
gm2 <- get_matches(m.out1)
table(table(gm2$id[gm2$rucc_treated == 0]))

fitgm2 <- lm(Crude.Rate ~ rucc_treated + High.school.graduation.raw.value + Median.household.income.raw.value + Some.college.raw.value + Unemployment.raw.value + Average.Precipitation + Ratio.of.population.to.mental.health.providers + Average.Temperature,
             data = gm2, 
             weights = weights)

coeftest(fitgm2, vcov. = vcovCL, cluster = ~subclass + id)["rucc_treated",,drop = FALSE]
gm2$rucc_treated <- as.factor(gm2$rucc_treated)
ggplot(data = gm2, mapping = aes(y = rucc_treated, x = Crude.Rate)) +
  geom_violin() +
  geom_boxplot(width=0.1)