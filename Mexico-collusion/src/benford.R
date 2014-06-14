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
  p <- table(d) / nrow(contractor.df)
  expectation <- benford(names(p))
  sum((p - expectation)^2)
}

result <- ddply(subset(mex, amount > 1), 'contractor_id', contractor.error)
