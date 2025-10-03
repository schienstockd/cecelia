# custom functions
# https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr
.round_any = function(x, accuracy, f=round) {f(x/ accuracy) * accuracy}

# https://stackoverflow.com/a/43195631/13766165
# Check that it doesn't match any non-letter
.checkLettersOnly <- function(x) !grepl("[^A-Za-z]", x)

# Check that it doesn't match any non-number
.checkNumbersOnly <- function(x) !grepl("\\D", x)
