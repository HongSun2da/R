data <- iris
str(data)
summary(data)


model <- Sepal.Length ~ Sepal.Width  + Petal.Length + Petal.Width
fit <- lm(model, data)
fit.step <- step(fit)
summary(fit.step)



library(GGally)
ggpairs(data)
ggpairs(data, mapping=aes(color=Species))
ggpairs(data, mapping=aes(color=Species, alpha=0.3))


summary(step(lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width
                , data
                , subset=(Species=="setosa"))))


x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
x
col.sums <- apply(x, 2, sum)
row.sums <- apply(x, 1, sum)
col.sums

row.sums


nchar('adfasfs')        
length('adfasfs')



x=c(1,2,3,NA)
mean(x)
max(x)
min(x)


x = 1:100
x

sum(x>50)

1+1
0/0
1/0
0/1



