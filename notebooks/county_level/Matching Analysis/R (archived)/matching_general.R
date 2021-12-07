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



# # 1:1 NN PS matching w/o replacement
matching <- read.csv(file = '../../../data/processed_data/df_matching.csv')
# 
# # matching types
methods = c("nearest", "optimal", "full", "genetic")
distances = c("glm", "mahalanobis")
# calipers = (1:10) / 10 # from 0.1 to 1
# 
# # try out
# start_time <- Sys.time()
# m.out2 <- matchit(matching$unemployment_treated ~ matching$Some.college.raw.value + matching$Median.household.income.raw.value,
#                   method = methods[4], distance = distances[1], caliper = 0.3, pop.size = 500)
# m.out.summary <- summary(m.out2)
# print(Sys.time() - start_time)
# matched.sum <- m.out.summary[["sum.matched"]]
# smd <- matched.sum[,3]
# 
# # template matching
# start_time <- Sys.time()
# m.out2 <- matchit(matching$unemployment_treated ~ matching$Some.college.raw.value + matching$Median.household.income.raw.value,
#                   method = "cardinality", ratio = NA)
# print(Sys.time() - start_time)
# m.out.summary <- summary(m.out2)
# print(m.out.summary)
# 
# # pairing after template matching
# m.out3 <- matchit(matching$unemployment_treated ~ matching$Some.college.raw.value + matching$Median.household.income.raw.value,
#            data = matching, method = methods[1],
#            distance = "mahalanobis", replace=TRUE,
#            discard = m.out2$weights == 0)
# print(summary(m.out3))
# 
# # real stuff below
# # chosen
# best.method = methods[1]
# #best.distances = distances[1]
# best.caliper = calipers[1]
# max.matched.treated = 0
# 
# # do matching
# for (method in methods) {
#   print(str_interp("Try out method: ${method}"))
#   # TODO: binary search of caliper (largest that SMD satisfy)
#   for (cali in calipers) {
#     # do matchng
#     m.out <- matchit(matching$unemployment_treated ~ matching$Some.college.raw.value + matching$Median.household.income.raw.value,
#                      method = method, distance = distances[1], caliper = cali)
#     m.out.summary <- summary(m.out)
#     matched.sum <- m.out.summary[["sum.matched"]]
#     smd <- matched.sum[,3]
# 
#     # check if everything is less than 0.25
#     if (sum(smd > 0.25) == 0) { # Sum of the true's should be 0
#       # compare how many data it retain and if greater than original
#       matched.treated = m.out.summary[["nn"]][4, 2]
# 
#       if (matched.treated > max.matched.treated) {
#         # record the method, distance, caliper and number of matched
#         best.method = method
#         # best.distances = distance
#         best.caliper = cali
#         max.matched.treated = matched.treated
#         print(paste(best.method, cali, max.matched.treated, sep = " "))
#         print("\n")
#       }
#     }
#   }
# }