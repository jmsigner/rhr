set.seed(123)
x <- cumsum(rnorm(500))
y <- cumsum(rnorm(500))

## Permutate path by shuffling segments and random adding random angles
p1 <- rhr:::rhrBasePRW(x, y)

