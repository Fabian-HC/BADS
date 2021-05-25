N <- 10000
df <- data.frame(a = rnorm(N), b = rnorm(N),
                 region = rep(1:100, each = 100), year = rep(1:100, 100))
df$y <- 2 * df$a - 1.5 * df$b + rnorm(N)
model.a <- lm(y ~ a + b + factor(year) + factor(region), data = df)
summary(model.a)
pdf <- pdata.frame(df, index = c("region", "year"))

model.b <- plm(y ~ a + b, data = pdf, model = "within", effect = "twoways")
summary(model.b)