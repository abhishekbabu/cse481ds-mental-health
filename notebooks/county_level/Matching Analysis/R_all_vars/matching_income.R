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
m.out <- matchit(matching$household_income_treated ~ Some.college.raw.value + Unemployment.raw.value + Ratio.of.population.to.mental.health.providers + 
                   RUCC,
                 data = matching, method = methods[1], distance = distances[1])
summary(m.out)
plot(m.out, type = "jitter", interactive = FALSE)

matching_sub = matching

# get rid of treated with propensity score greater than control
m.data1 <- match.data(m.out)
untreated <- (m.data1[["household_income_treated"]] == 0)
untreated.data <- m.data1[untreated,]
distance_greatest <- (untreated.data[["distance"]] == max(untreated.data[["distance"]]))
untreated_greatest_dist <- untreated.data[distance_greatest, "distance"]

# create a new column: assign "too high" if distance greater than untreated_greatest_dist, "normal"
m.data1$too_high <- (m.data1$distance >untreated_greatest_dist)

# plotting
ggplot(data = m.data1) + # Unemployment.raw.value
  geom_point(mapping = aes(x = Some.college.raw.value, y = RUCC, color = too_high))

ggplot(data = m.data1) +
  geom_point(mapping = aes(x = Some.college.raw.value, y = Median.household.income.raw.value, color = unemployment_treated)) +
  geom_hline(yintercept=72092.05, linetype="dashed", color = "red") +
  geom_vline(xintercept=0.7489542, linetype="dashed", color = "red")

# Use less than 90% of college (0.7547552) for now
# Cut off unemployment + low RUCC
college_values <- quantile(matching$Some.college.raw.value, c(.25, .50,  .85, .90, .95))
normal_college <- (matching$Some.college.raw.value <= college_values[4])
matching_sub = matching[normal_college, ]

# second iteration
unemployment_values <- quantile(m.data1$Unemployment.raw.value, c(.1, .50,  .75, .90, .95))
normal_unemployment <- (matching$Unemployment.raw.value > unemployment_values[1])
high_RUCC <- (matching$RUCC > 2)

# third iteration
Exclude_first_RUCC <- (matching$RUCC > 1)
normal_college <- (matching$Some.college.raw.value <= college_values[3])


matching_sub = matching[(normal_college | Exclude_first_RUCC)  & (normal_unemployment | high_RUCC), ]

# Do matching on unemployment again
m.out <- matchit(household_income_treated ~ Some.college.raw.value + Unemployment.raw.value + Ratio.of.population.to.mental.health.providers + 
                   RUCC,
                 data = matching_sub, method = methods[1], distance = distances[1], replace = TRUE, reuse.max = 2)
summary(m.out)
plot(m.out, type = "jitter", interactive = FALSE)

# Estimating effect
gm <- get_matches(m.out)
table(table(gm$id[gm$household_income_treated == 0]))
fitmd <- lm(Poor.mental.health.days.raw.value ~ household_income_treated + Some.college.raw.value + Unemployment.raw.value + Ratio.of.population.to.mental.health.providers + RUCC, data = gm, 
            weights = weights)

coeftest(fitmd, vcov. = vcovCL, cluster = ~subclass + id)["household_income_treated",,drop = FALSE]
gm$household_income_treated <- as.factor(gm$household_income_treated)
ggplot(data = gm, mapping = aes(y = household_income_treated, x = Poor.mental.health.days.raw.value)) +
  geom_violin() +
  geom_boxplot(width=0.1)

fitmd2 <- lm(Crude.Rate ~ household_income_treated + Some.college.raw.value + Unemployment.raw.value + Ratio.of.population.to.mental.health.providers + RUCC, data = gm, 
            weights = weights)
coeftest(fitmd2, vcov. = vcovCL, cluster = ~subclass + id)["household_income_treated",,drop = FALSE]
gm$household_income_treated <- as.factor(gm$household_income_treated)
ggplot(data = gm, mapping = aes(y = household_income_treated, x = Crude.Rate)) +
  geom_violin() +
  geom_boxplot(width=0.1)
