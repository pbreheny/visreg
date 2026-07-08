suppressPackageStartupMessages(library(MASS))

# rlm
fit <- rlm(Ozone ~ ., data = airquality)
visreg(fit, "Wind")

# polr
fit <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
visreg(fit, "Infl", partial = FALSE, rug = FALSE, collapse = TRUE)
p <- visreg(fit, "Infl", partial = FALSE, rug = FALSE, collapse = TRUE, gg = TRUE) |>
  suppressMessages()
print(p) |> expect_silent()
visreg(fit, "Infl", partial = FALSE, rug = FALSE, collapse = TRUE, overlay = TRUE)
visreg(fit, "Infl", type = "contrast", partial = FALSE, rug = FALSE) |> suppressMessages()
