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
m.out <- matchit(temperature_treated ~ High.school.graduation.raw.value + Some.college.raw.value + Unemployment.raw.value + Ratio.of.population.to.mental.health.providers + 
                   RUCC + Average.Precipitation,
                 data = matching, method = methods[1], distance = distances[1], replace = TRUE, reuse.max = 2)
summary(m.out)
plot(m.out, type = "jitter", interactive = FALSE)

# estimate effect
gm <- get_matches(m.out)
table(table(gm$id[gm$temperature_treated == 0]))
fitmd <- lm(Poor.mental.health.days.raw.value ~ temperature_treated + Average.Precipitation + RUCC, data = gm, 
            weights = weights)

coeftest(fitmd, vcov. = vcovCL, cluster = ~subclass + id)["temperature_treated",,drop = FALSE]

# Suicide
fitmd2 <- lm(Crude.Rate ~ temperature_treated + Average.Precipitation + RUCC, data = gm, 
             weights = weights)
coeftest(fitmd2, vcov. = vcovCL, cluster = ~subclass + id)["temperature_treated",,drop = FALSE]
# plotting
ggplot(data = gm, mapping = aes(y = mental_health_provider_treated, x = Crude.Rate)) +
  geom_violin() +
  geom_boxplot(width=0.1)
