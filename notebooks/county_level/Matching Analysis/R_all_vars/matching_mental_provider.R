library("MatchIt")

# for plotting
library("ggplot2")

# for analysis
library("lmtest")
library("sandwich")

# # 1:1 NN PS matching w/o replacement
matching <- read.csv(file = '../../../../data/processed_data/df_matching.csv')
# 
# # matching types
methods = c("nearest", "optimal", "full", "genetic")
distances = c("glm", "mahalanobis")

cor_matrix = cor(matching[complete.cases(matching[ , 12]),5:13])

# nearest neighbor
m.out <- matchit(mental_health_provider_treated ~ High.school.graduation.raw.value +
                 Unemployment.raw.value + Median.household.income.raw.value +
                   Some.college.raw.value + RUCC + Average.Temperature + Average.Precipitation,
                 data = matching, method = methods[1], distance = distances[1], replace = TRUE, reuse.max = 3)
summary(m.out)
plot(m.out, type = "jitter", interactive = FALSE)

# estimate effect
gm <- get_matches(m.out)
table(table(gm$id[gm$mental_health_provider_treated == 0]))
fitmd <- lm(Poor.mental.health.days.raw.value ~ mental_health_provider_treated + High.school.graduation.raw.value +
              Unemployment.raw.value + Median.household.income.raw.value +
              Some.college.raw.value + RUCC + Average.Temperature + Average.Precipitation, data = gm, 
            weights = weights)

coeftest(fitmd, vcov. = vcovCL, cluster = ~subclass + id)["mental_health_provider_treated",,drop = FALSE]

# Validate data
med <- summary(matching$Ratio.of.population.to.mental.health.providers)[3]
# count the number of 0's for above median: should contain all 0s
above_median <- (matching$Ratio.of.population.to.mental.health.providers > med)
table(matching[above_median,]$mental_health_provider_treated) # correct

# plotting
gm$mental_health_provider_treated <- as.factor(gm$mental_health_provider_treated)
ggplot(data = gm, mapping = aes(y = mental_health_provider_treated, x = Poor.mental.health.days.raw.value)) +
  geom_violin() +
  geom_boxplot(width=0.1)

# Suicide
fitmd2 <- lm(Crude.Rate ~ mental_health_provider_treated + High.school.graduation.raw.value +
               Unemployment.raw.value + Median.household.income.raw.value +
               Some.college.raw.value + RUCC + Average.Temperature + Average.Precipitation, data = gm, 
            weights = weights)
coeftest(fitmd2, vcov. = vcovCL, cluster = ~subclass + id)["mental_health_provider_treated",,drop = FALSE]
# plotting
ggplot(data = gm, mapping = aes(y = mental_health_provider_treated, x = Crude.Rate)) +
  geom_violin() +
  geom_boxplot(width=0.1)
