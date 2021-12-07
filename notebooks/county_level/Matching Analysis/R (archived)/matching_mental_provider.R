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
m.out <- matchit(mental_health_provider_treated ~ Unemployment.raw.value + Median.household.income.raw.value + Some.college.raw.value,
                 data = matching, method = methods[1], distance = distances[1], replace = TRUE, reuse.max = 3)
summary(m.out)
plot(m.out, type = "jitter", interactive = FALSE)

matching_sub = matching

# get rid of treated with propensity score greater than control
m.data1 <- match.data(m.out)
untreated <- (m.data1[["mental_health_provider_treated"]] == 0)
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
  #geom_hline(yintercept=72092.05, linetype="dashed", color = "red") +
  geom_vline(xintercept=income_values[3], linetype="dashed", color = "red")

# Use less than 90% of college (0.7547552) for now
# Cut off unemployment + low RUCC
income_values <- quantile(matching$Median.household.income.raw.value, c(.25, .60,  .95, .96, .99))
normal_income <- (matching$Median.household.income.raw.value <= income_values[3])
matching_sub = matching[normal_income, ]

# # second iteration
# unemployment_values <- quantile(m.data1$Unemployment.raw.value, c(.1, .50,  .75, .90, .95))
# normal_unemployment <- (matching$Unemployment.raw.value > unemployment_values[1])
# high_RUCC <- (matching$RUCC > 2)
# 
# # third iteration
# Exclude_first_RUCC <- (matching$RUCC > 1)
# normal_college <- (matching$Some.college.raw.value <= college_values[3])

# Do matching on unemployment again
m.out <- matchit(college_treated ~ Unemployment.raw.value + Median.household.income.raw.value + RUCC,
                 data = matching_sub, method = methods[1], distance = distances[1], replace = TRUE, reuse.max = 4)
summary(m.out)
plot(m.out, type = "jitter", interactive = FALSE)

# TODO: estimating effect of unemployment. A bit trickier because replacement is used
# See https://kosukeimai.github.io/MatchIt/articles/estimating-effects.html
# Not sure how to do it with replacement
gm <- get_matches(m.out)
table(table(gm$id[gm$mental_health_provider_treated == 0]))
fitmd <- lm(Poor.mental.health.days.raw.value ~ mental_health_provider_treated + Unemployment.raw.value + Median.household.income.raw.value + Some.college.raw.value, data = gm, 
            weights = weights)

coeftest(fitmd, vcov. = vcovCL, cluster = ~subclass + id)["mental_health_provider_treated",,drop = FALSE]

# Validate data
med <- summary(matching$Ratio.of.population.to.mental.health.providers)[3]
# count the number of 0's for above median: should contain all 0s
above_median <- (matching$Ratio.of.population.to.mental.health.providers > med)
table(matching[above_median,]$mental_health_provider_treated) # correct
