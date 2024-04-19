# fitting data

# read file
d <- read.csv("fitting data.csv")
data <- d[,-c(1,2,6)]
data$scores[d$scores > 0.5] = 1
data$scores[d$scores <= 0.5] = 0


# fit logistic regression
mdl <- glm(formula = scores~., family = binomial(link = "logit"),data=data)
features <- as.matrix(scale(data[,-1]))

fitted <- predict(mdl,data=features,type="response")
results <- ifelse(fitted > 0.5, 1, 0)
(error <- mean(results != data$scores))

summary(mdl)


mdl2 <- glm(formula = scores~noisiness, family = binomial(link = "logit"),data=data)
features <- as.matrix(data[,-1])
fitted <- predict(mdl2,data=features,type="response")
results <- ifelse(fitted > 0.5, 1, 0)
(error <- mean(results != data$scores))

summary(mdl2)









