Bs <- rep(100 * (1:50), 3)

law <- matrix(rnorm(200),ncol=2)
colnames(law) <- c("LSAT","GPA")
law <- data.frame(law)

ptm <- proc.time()
library(plyr)
corsize <- ldply(Bs, function(B) {
  cors <- replicate(B, {
    with(law[sample(nrow(law), replace = TRUE), ], cor(LSAT,
                                                       GPA))
  })
  c(B = B, cor = mean(cors))
})
head(corsize, n = 3)
proc.time() - ptm

ptm <- proc.time()
corsize2 = matrix(0, length(Bs), 2)
for (i in 1:length(Bs)) {
  b = Bs[i]
  cor.boot <- numeric(b)
  for (j in 1:b) {
    law.boot <- law[sample(nrow(law), replace = TRUE), ]
    cor.boot[j] = cor(law.boot$LSAT, law.boot$GPA)
  }
  corsize2[i, ] = c(b, mean(cor.boot))
}
colnames(corsize2) = c("B", "cor")
proc.time() - ptm