# mex <- read.csv('data/mex_clean.csv', stringsAsFactors = F)

# mex$federal_amount


leading.digit <- function(amount) {
  as.numeric(strtrim(as.character(amount), 1))
}

benford <- function(d) {
  log10(d + 1) - log10(d)
}

contractor.error <- function(contractor.df) {
  d <- leading.digit(contractor.df$amount)
  p.observed <- table(d) / nrow(contractor.df)
  p.expected <- benford(as.numeric(names(p.observed)))
  c(ss.error = sum((p.expected - p.observed)^2))
}

# result <- ddply(subset(mex, amount > 1), 'contractor_id', contractor.error)
print(subset(result, ss.error > 0.9))
