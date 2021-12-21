library("MatchIt")

# for plotting
library("ggplot2")

# for analysis
library("lmtest")
library("sandwich")

matching <- read.csv(file = 'df_matching.csv')

# nearest neighbor
m.out1 <- matchit(household_income_treated ~ High.school.graduation.raw.value + Unemployment.raw.value + 
                   Some.college.raw.value + Ratio.of.population.to.mental.health.providers +
                   RUCC + Average.Temperature + Average.Precipitation,
                 data = matching, method = "nearest", distance = "glm", replace = TRUE,
                 reuse.max = 9, caliper = 0.25)
summary(m.out1)
plot(m.out1, type = "jitter", interactive = FALSE)

# See https://kosukeimai.github.io/MatchIt/articles/estimating-effects.html

# Poor mental health days
gm <- get_matches(m.out1)
table(table(gm$id[gm$household_income_treated == 0]))

fitgm <- lm(Poor.mental.health.days.raw.value ~ household_income_treated + High.school.graduation.raw.value + Unemployment.raw.value + Some.college.raw.value + Ratio.of.population.to.mental.health.providers + RUCC + Average.Temperature + Average.Precipitation,
            data = gm,
            weights = weights)

coeftest(fitgm, vcov. = vcovCL, cluster = ~subclass + id)["household_income_treated",,drop = FALSE]
gm$household_income_treated <- as.factor(gm$household_income_treated)
ggplot(data = gm, mapping = aes(y = household_income_treated, x = Poor.mental.health.days.raw.value)) +
  geom_violin() +
  geom_boxplot(width=0.1)

# Suicide rate
gm2 <- get_matches(m.out1)
table(table(gm2$id[gm2$household_income_treated == 0]))

fitgm2 <- lm(Crude.Rate ~ household_income_treated + High.school.graduation.raw.value + Unemployment.raw.value + Some.college.raw.value + Ratio.of.population.to.mental.health.providers + RUCC + Average.Temperature + Average.Precipitation,
             data = gm2, 
             weights = weights)

coeftest(fitgm2, vcov. = vcovCL, cluster = ~subclass + id)["household_income_treated",,drop = FALSE]
gm2$household_income_treated <- as.factor(gm2$household_income_treated)
ggplot(data = gm2, mapping = aes(y = household_income_treated, x = Crude.Rate)) +
  geom_violin() +
  geom_boxplot(width=0.1)