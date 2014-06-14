# mex <- read.csv('data/mex_clean.csv', stringsAsFactors = F)

# mex$federal_amount


leading.digit <- function(amount) {
  as.numeric(strtrim(as.character(amount), 1))
}

benford <- function(d) {
  log10(d + 1) - log10(d)
}

contractor.error <- function(contractor.df) {
  d <- factor(leading.digit(contractor.df$amount), levels = 1:9)
  count.observed <- table(d)
  p.observed <- count.observed / nrow(contractor.df)
  p.expected <- benford(as.numeric(names(p.observed)))
  count.expected <- round(nrow(contractor.df) * p.expected)
  c(ss.error = sum((p.observed - p.expected)^2),
    chisq.p = chisq.test(p.observed, p = p.expected)$p.value,
   #chisq.p = chisq.test(count.observed, count.expected)$p.value
   #ks.p = ks.test(p.observed, p.expected)$p.value # wrong
    n.contracts = nrow(contractor.df)
  )
}

plot.contractor <- function(contractor.df) {
  d <- factor(leading.digit(contractor.df$amount), levels = 1:9)
  p.observed <- table(d) / nrow(contractor.df)
  p.expected <- benford(1:9)
  plot(1:9, p.expected, xlab = 'Leading digit', ylab = 'Proportion of leading digits',
       main = 'Comparison of leading digits to the Benford distribution', type = 'l',
       bty = 'l')
  lines(1:9, p.observed, col = 2)
}


p <- function() {
  pdf('leading-digits.pdf', width = 11, height = 8.5)
  d_ply(subset(mex, amount > 1), 'contractor_id', plot.contractor)
  dev.off()
}

result <- ddply(subset(mex, amount > 1), 'contractor_id', contractor.error)
p2 <- function() {
  p <- ggplot(result) + aes(x = n.contracts, y = ss.error, label = contractor_id) + geom_text() +
    xlab('Number of contracts by the contractor') +
    ylab("Sum of squared distance from Benford's distribution") +
    ggtitle("Applying Benford's Law to contract amounts")
  ggsave('benford.png', p)
}
