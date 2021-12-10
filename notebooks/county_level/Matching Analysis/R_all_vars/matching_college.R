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
m.out <- matchit(matching$college_treated ~ High.school.graduation.raw.value + Unemployment.raw.value + 
                 Median.household.income.raw.value + Ratio.of.population.to.mental.health.providers +
                 RUCC + Average.Temperature + Average.Precipitation,
                 data = matching, method = methods[1], distance = distances[1])
summary(m.out)
plot(m.out, type = "jitter", interactive = FALSE)

matching_sub = matching

# get rid of treated with propensity score greater than control
m.data1 <- match.data(m.out)
untreated <- (m.data1[["college_treated"]] == 0)
untreated.data <- m.data1[untreated,]
distance_greatest <- (untreated.data[["distance"]] == max(untreated.data[["distance"]]))
untreated_greatest_dist <- untreated.data[distance_greatest, "distance"]

# create a new column: assign "too high" if distance greater than untreated_greatest_dist, "normal"
m.data1$too_high <- (m.data1$distance >untreated_greatest_dist)

# plotting
ggplot(data = m.data1) + # Unemployment.raw.value
  geom_point(mapping = aes(x = Median.household.income.raw.value, y = Unemployment.raw.value, color = too_high)) +
  geom_vline(xintercept=income_values[3], linetype="dashed", color = "red")

ggplot(data = m.data1) +
  geom_point(mapping = aes(x = Median.household.income.raw.value, y = RUCC, color = too_high)) +
  geom_vline(xintercept=income_values[6], linetype="dashed", color = "red")

# Use less than 90% of college (0.7547552) for now
# Cut off unemployment + low RUCC
income_values <- quantile(matching$Median.household.income.raw.value, c(.25, .60,  .95, .96, .99, .94))
normal_income <- (matching$Median.household.income.raw.value <= income_values[6])
high_RUCC <- (matching$RUCC > 1)
matching_sub = matching[normal_income & high_RUCC, ]

# Do matching on unemployment again
m.out <- matchit(college_treated ~ High.school.graduation.raw.value + Unemployment.raw.value + 
                   Median.household.income.raw.value + Ratio.of.population.to.mental.health.providers +
                   RUCC + Average.Temperature + Average.Precipitation,
         data = matching_sub, method = methods[1], distance = distances[1], replace = TRUE, reuse.max = 4)
summary(m.out)
plot(m.out, type = "jitter", interactive = FALSE)

# TODO: estimating effect of unemployment. A bit trickier because replacement is used
# See https://kosukeimai.github.io/MatchIt/articles/estimating-effects.html
# Not sure how to do it with replacement
gm <- get_matches(m.out)
table(table(gm$id[gm$college_treated == 0]))
fitmd <- lm(Poor.mental.health.days.raw.value ~ college_treated + Unemployment.raw.value + Median.household.income.raw.value + Ratio.of.population.to.mental.health.providers + RUCC,
            data = gm, 
            weights = weights)

coeftest(fitmd, vcov. = vcovCL, cluster = ~subclass + id)["college_treated",,drop = FALSE]
gm$college_treated <- as.factor(gm$college_treated)
ggplot(data = gm, mapping = aes(y = college_treated, x = Poor.mental.health.days.raw.value)) +
  geom_violin() +
  geom_boxplot(width=0.1)

# for suicide
fitmd2 <- lm(Crude.Rate ~ college_treated + Unemployment.raw.value + Median.household.income.raw.value + Ratio.of.population.to.mental.health.providers + RUCC,
            data = gm, 
            weights = weights)

coeftest(fitmd2, vcov. = vcovCL, cluster = ~subclass + id)["college_treated",,drop = FALSE]
gm$college_treated <- as.factor(gm$college_treated)
ggplot(data = gm, mapping = aes(y = college_treated, x = Poor.mental.health.days.raw.value)) +
  geom_violin() +
  geom_boxplot(width=0.1)
