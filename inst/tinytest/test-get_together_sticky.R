#test together sticky

#various situations of coming together and splitting

R_inner <- 0.4
R_outer <- 0.6

a <- 1 #apart distance
m <- 0.5 #mid distance (between the thresholds)
t <- 0.3 #together distance

#Test 1
dists <-  c(a, a, m, m, t, t, m, m, a, a)
result <- c(F, F, T, T, T, T, T, T, F, F)

together_inner <- dists <= R_inner
together_outer <- dists <= R_outer

expect_equal(get_together_sticky(together_inner, together_outer), result)

#Test 2
#apart -- mid -- apart -- mid -- together -- mid -- apart
dists <-  c(a, a, m, m, a, a, m, t, t, m, m, a, a)
result <- c(F, F, F, F, F, F, T, T, T, T, T, F, F)

together_inner <- dists <= R_inner
together_outer <- dists <= R_outer

expect_equal(get_together_sticky(together_inner, together_outer), result)

#Test 3
#apart -- mid -- together -- mid -- together -- mid -- apart
dists <-  c(a, a, m, m, a, a, m, t, t, m, m, a, a)
result <- c(F, F, F, F, F, F, T, T, T, T, T, F, F)

together_inner <- dists <= R_inner
together_outer <- dists <= R_outer

expect_equal(get_together_sticky(together_inner, together_outer), result)

#Test 4
#apart -- together -- mid -- apart
dists  <- c(a, a, t, t, m, m, a, a)
result <- c(F, F, T, T, T, T, F, F)

together_inner <- dists <= R_inner
together_outer <- dists <= R_outer

expect_equal(get_together_sticky(together_inner, together_outer), result)

#Test 5
#together -- mid -- apart -- mid -- together
dists <-  c(t, m, a, m, t)
result <- c(T, T, F, T, T)

together_inner <- dists <= R_inner
together_outer <- dists <= R_outer

expect_equal(get_together_sticky(together_inner, together_outer), result)

#Test 6
#together -- apart -- together -- apart -- together
dists <-  c(t, a, t, a, t)
result <- c(T, F, T, F, T)

together_inner <- dists <= R_inner
together_outer <- dists <= R_outer

expect_equal(get_together_sticky(together_inner, together_outer), result)

#Test 7
#NA -- apart -- mid -- together -- mid -- apart
dists <-  c(NA, a, m, t, m, a)
result <- c(NA,  F, T, T, T, F)

together_inner <- dists <= R_inner
together_outer <- dists <= R_outer

expect_equal(get_together_sticky(together_inner, together_outer), result)

#Test 8
#apart -- mid -- together -- NA -- together -- mid -- apart
dists <-  c(a, m, t, NA, t, m, a)
result <- c(F, T, T, NA,  T, T, F)

together_inner <- dists <= R_inner
together_outer <- dists <= R_outer

expect_equal(get_together_sticky(together_inner, together_outer), result)

#Test 9
#NA -- mid -- NA -- mid -- together -- apart
dists <-  c(NA, m,  NA, m,  t, a)
result <- c(NA, F,  NA, T,  T, F)

together_inner <- dists <= R_inner
together_outer <- dists <= R_outer

expect_equal(get_together_sticky(together_inner, together_outer), result)

#Test 10
#NA -- together -- mid -- NA -- mid -- together -- apart
dists <-  c(NA, t, m, NA, m, t)
result <- c(NA, T, T, NA, T, T)

together_inner <- dists <= R_inner
together_outer <- dists <= R_outer

expect_equal(get_together_sticky(together_inner, together_outer), result)

#Test 11
#NA -- together -- mid -- NA -- mid -- together -- apart
dists <-  c(NA, t, m, NA, m, NA)
result <- c(NA, T, T, NA, F, NA)

together_inner <- dists <= R_inner
together_outer <- dists <= R_outer

expect_equal(get_together_sticky(together_inner, together_outer), result)

#Test 11
#apart -- apart -- NA
dists <-  c(a, a, NA)
result <- c(F, F, NA)

together_inner <- dists <= R_inner
together_outer <- dists <= R_outer

expect_equal(get_together_sticky(together_inner, together_outer), result)

