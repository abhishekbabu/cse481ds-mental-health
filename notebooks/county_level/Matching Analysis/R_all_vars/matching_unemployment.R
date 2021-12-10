library("MatchIt")
library("stringr")

# for full matching
library("optmatch")

# for genetic
library("Matching")
library("rgenoud")

# for template
library("Rglpk")

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
 
cor_matrix = cor(matching[complete.cases(matching[ , 12]),4:13])

# nearest neighbor
m.out <- matchit(unemployment_treated ~ High.school.graduation.raw.value + Some.college.raw.value + Median.household.income.raw.value + Ratio.of.population.to.mental.health.providers + 
                 RUCC + Average.Temperature + Average.Precipitation
                 , data = matching, method = methods[1], distance = distances[1], replace = TRUE)
summary(m.out)
plot(m.out, type = "jitter", interactive = FALSE)

# get rid of treated with propensity score greater than control
m.data1 <- match.data(m.out)
untreated <- (m.data1[["unemployment_treated"]] == 0)
untreated.data <- m.data1[untreated,]
distance_greatest <- (untreated.data[["distance"]] == max(untreated.data[["distance"]]))
untreated_greatest_dist <- untreated.data[distance_greatest, "distance"]

# create a new column: assign "too high" if distance greater than untreated_greatest_dist, "normal"
m.data1$too_high <- (m.data1$distance >untreated_greatest_dist)

# plotting
ggplot(data = m.data1) +
  geom_point(mapping = aes(x = Some.college.raw.value, y = Median.household.income.raw.value, color = too_high)) +
  geom_hline(yintercept=72092.05, linetype="dashed", color = "red") +
  geom_vline(xintercept=0.7489542, linetype="dashed", color = "red")

ggplot(data = m.data1) +
  geom_point(mapping = aes(x = Some.college.raw.value, y = Median.household.income.raw.value, color = unemployment_treated)) +
  geom_hline(yintercept=72092.05, linetype="dashed", color = "red") +
  geom_vline(xintercept=0.7489542, linetype="dashed", color = "red")

# Use less than 95% of college (0.7489542) and median household income (72092.05)
college_values <- quantile(m.data1$Some.college.raw.value, c(.25, .50,  .75, .90, .95))
income_values <- quantile(m.data1$Median.household.income.raw.value, c(.25, .50,  .75, .90, .95))
normal_college <- (matching$Some.college.raw.value <= college_values[5])
normal_income <- (matching$Median.household.income.raw.value <= income_values[5])

matching_sub = matching[normal_college & normal_income, ]

# Do matching on unemployment again
m.out <- matchit(unemployment_treated ~ High.school.graduation.raw.value + Some.college.raw.value + Median.household.income.raw.value + Ratio.of.population.to.mental.health.providers + 
                   RUCC + Average.Temperature + Average.Precipitation,
                 data = matching_sub, method = methods[1], distance = distances[1], replace = TRUE, reuse.max = 2)
summary(m.out)
plot(m.out, type = "jitter", interactive = FALSE)

# Estimating effect of unemployment. A bit trickier because replacement is used
gm <- get_matches(m.out)

# On poor mental health days
fitmd <- lm(Poor.mental.health.days.raw.value ~ unemployment_treated + High.school.graduation.raw.value + Some.college.raw.value + Median.household.income.raw.value + Ratio.of.population.to.mental.health.providers + 
              RUCC + Average.Temperature + Average.Precipitation, data = gm, 
             weights = weights)

coeftest(fitmd, vcov. = vcovCL, cluster = ~subclass + id)["unemployment_treated",,drop = FALSE]
gm$unemployment_treated <- as.factor(gm$unemployment_treated)
ggplot(data = gm, mapping = aes(y = unemployment_treated, x = Poor.mental.health.days.raw.value)) +
  geom_violin() +
  geom_boxplot(width=0.1)

fitmd2 <- lm(Crude.Rate ~ unemployment_treated + High.school.graduation.raw.value + Some.college.raw.value + Median.household.income.raw.value + Ratio.of.population.to.mental.health.providers + 
               RUCC + Average.Temperature + Average.Precipitation, data = gm, 
            weights = weights)

coeftest(fitmd2, vcov. = vcovCL, cluster = ~subclass + id)["unemployment_treated",,drop = FALSE]
gm$unemployment_treated <- as.factor(gm$unemployment_treated)
ggplot(data = gm, mapping = aes(y = unemployment_treated, x = Crude.Rate)) +
  geom_violin() +
  geom_boxplot(width=0.1)



