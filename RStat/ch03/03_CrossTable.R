
post = read.csv("0303.post.csv",
                header = TRUE,
                na.strings = ".")
post
summary(post)

post$smoking = factor(post$smoking,
                      levels = c(1,2),
                      labels = c("smok", "nsmok"))
post$cancer = factor(post$cancer,
                     levels = c(1,2),
                     labels = c("cancer", "health"))
str(post)

post.n = xtabs(observation~cancer+smoking, data = post)

str(post.n)
summary(post.n)

#install.packages("gmodels")
library(gmodels)

post.n
CrossTable(post.n)



