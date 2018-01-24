
step1 <- c(2, 5, 5, 10, 10, 15)
step2 <- c(3, 6, 8, 9, 11, 18)


dens1 <- dense_rank(step1)
dens2 <- dense_rank(step2)
cor(dens1, dens2, method = "spearman")

min1 <- min_rank(step1)
min2 <- min_rank(step2)
cor(min1, min2, method = "spearman")
