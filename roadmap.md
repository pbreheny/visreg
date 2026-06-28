# visreg 3.0 Roadmap

## 0. Custom geoms — decision: no

Custom geoms work best when computation is observation-level (each row
of data → one mark). visreg’s computation is model-level: it calls
[`predict()`](https://rdrr.io/r/stats/predict.html) on a fitted object
and constructs an entirely new data grid. There is no natural hook for
that in ggplot2’s Stat/Geom system without passing the model object as a
non-standard aesthetic, which violates the grammar.

The architecture already in place — `visreg(plot=FALSE)` returns a clean
object; [`plot()`](https://rdrr.io/r/graphics/plot.default.html) returns
a `gg` object the user can post-process with `+` — is the right answer.
Dropping base R makes that even cleaner since the return value is always
a `gg`.

The most compelling argument for geoms is overlaying multiple model
fits, but the better path there is a cleaner `visregList` +
`autoplot.visregList`, not a custom geom.

## 1. Bug fixes (done)

Three bugs fixed before any refactoring:

**Bug 1 (`weight-char.r` — was the failing test):** When a model has
`weights = col` referencing a data column and the data also contains
character columns (triggering `needsUpdate`), `update(fit, data=f)`
failed because `f` only contains formula variables and the weights
column was missing. Fix: in `setupF()`, scan the call for
`weights`/`offset` arguments that name data columns and copy those
columns into `f` so [`update()`](https://rdrr.io/r/stats/update.html)
can find them.

**Bug 2 (`ggContPlot.R`):** `rug.args` was built with `sides="b"` but
then `point.args` (without `sides`) was passed to `geom_rug` for
`rug=1`. One-character fix.

**Bug 3 (`ggFactorPlot.R`):** `ggplot(pointData, ...)` was called with
`pointData = NULL` (it hadn’t been built yet). Changed to
`ggplot(mapping = ...)` — data is optional in `ggplot()` and all layers
provide their own data anyway.

## 2. Drop base R / lattice entirely

Remove these seven files: `R/factorAxis.R`, `R/factorPlot.R`,
`R/visregPanel.R`, `R/visregFactorPanel.R`, `R/visregPlot.R`,
`R/visregLatticePlot.R`, `R/visregOverlayPlot.R`, `R/toplegend.R`.
Remove lattice from `DESCRIPTION`/`NAMESPACE`.

Simplifications this enables:

- `plot.visreg` loses the `gg` parameter (it is always ggplot2 now).
  Return value is always a `gg` object.
- The `whitespace` parameter existed solely because base R factor plots
  required manual coordinate math. Replace with `position_dodge2` or let
  ggplot2 handle factor spacing natively with `geom_crossbar` /
  `geom_pointrange`.
- `ggFactorPlot`’s internal coordinate trick (`visregGGX`, `visregGGY`)
  can be replaced with idiomatic ggplot2: treat factor levels as
  discrete x values, use `scale_x_discrete`, show confidence intervals
  with `geom_crossbar(aes(x=level, y=fit, ymin=lwr, ymax=upr))`, and
  `geom_jitter` for residuals.
- `ggContPlot` should replace `geom_polygon` (which requires manually
  constructing the ribbon polygon) with
  `geom_ribbon(aes(ymin=lwr, ymax=upr))`.
- `line.par`/`fill.par`/`points.par` parameter names are inconsistent
  with how they are actually called in the README (`line=list(...)`,
  `fill=list(...)`, `points=list(...)`). Standardize on the shorter
  `line=`/`fill=`/`points=` form.

## 3. Naming: strict snake_case

All internal helpers renamed. None are exported, so no NAMESPACE churn.

| Current              | New                                       |
|----------------------|-------------------------------------------|
| `setupF`             | `setup_frame`                             |
| `setupCond`          | `setup_cond`                              |
| `setupD`             | `setup_data`                              |
| `setupX`             | `setup_contrast_data`                     |
| `setupV`             | `build_visreg`                            |
| `setupV2`            | `build_visreg2d`                          |
| `getXY`              | `get_xy`                                  |
| `fillFrame`          | `fill_frame`                              |
| `parseFormula`       | `parse_formula`                           |
| `makeYName`          | `make_y_name`                             |
| `abbrNum`            | `abbr_num`                                |
| `visregPred`         | `visreg_pred`                             |
| `visregResid`        | `visreg_resid`                            |
| `visreg_coef`        | `visreg_coef` (already good)              |
| `printCond`          | `print_cond`                              |
| `collapseVisregList` | keep as `collapse.visregList` (S3 method) |
| `subsetV`            | `subset_visreg`                           |
| `ggContPlot`         | `gg_cont_plot`                            |
| `ggFactorPlot`       | `gg_factor_plot`                          |

Public API parameters (breaking changes are acceptable in 3.0):

- `print.cond` → `print_cond`
- `strip.names` → `strip_names`
- `line.par`/`fill.par`/`points.par` → `line`/`fill`/`points`

Also: `fillFrame` contains an `eval(parse(...))` antipattern that should
be replaced with straightforward list manipulation.

## 4. File reorganization

After removing base R code, consolidate from ~38 files to ~12:

| New file | Contents |
|----|----|
| `visreg.R` | [`visreg()`](https://pbreheny.github.io/visreg/reference/visreg.md) |
| `visreg2d.R` | [`visreg2d()`](https://pbreheny.github.io/visreg/reference/visreg2d.md) |
| `visreg_list.R` | [`visregList()`](https://pbreheny.github.io/visreg/reference/visregList.md), `collapse.visregList()`, `plot.visregList()` |
| `setup_frame.R` | `setup_frame()`, `fill_frame()`, `parse_formula()` |
| `setup_conditions.R` | `setup_cond()`, `print_cond()` |
| `setup_data.R` | `setup_data()`, `setup_contrast_data()`, `get_xy()`, `build_visreg()` |
| `predict.R` | `visreg_pred()`, `visreg_resid()`, `visreg_coef()` |
| `response.R` | `compute_response()` (currently `Response.R`) |
| `terms.R` | `compute_terms()` (currently `Terms.R`) |
| `plot_visreg.R` | [`plot.visreg()`](https://pbreheny.github.io/visreg/reference/plot.visreg.md), `gg_cont_plot()`, `gg_factor_plot()` |
| `plot_visreg2d.R` | [`plot.visreg2d()`](https://pbreheny.github.io/visreg/reference/plot.visreg2d.md) |
| `utils.R` | `abbr_num()`, `make_y_name()`, `pal()`, `subset_visreg()`, [`subset.visreg()`](https://pbreheny.github.io/visreg/reference/subset.visreg.md) |

## 5. Mixed models (lme4)

**What changed:**
[`lme4::predict.merMod`](https://rdrr.io/pkg/lme4/man/predict.merMod.html)
now supports `se.fit=TRUE` (since ~1.1-27), returning marginal
(population-level) standard errors when `re.form=NA`. The old workaround
used fixed-effect coefficient extraction +
[`vcov()`](https://rdrr.io/r/stats/vcov.html) directly.

**Code changes:**

- In `visreg_pred`: when `se.fit=TRUE` for a `merMod` object, call
  `predict(fit, newdata=Data, re.form=NA, se.fit=TRUE)` directly rather
  than computing SEs via the coefficient/vcov route.
- In `Terms()` (contrast type): the `merMod` branch can stay but should
  use `lme4::fixef(fit)` rather than the internal slot `fit@beta`.

**Documentation changes:**

- Default plots show marginal (population-level) predictions and
  confidence bands.
- Pass `re.form=NULL` to plot conditional on estimated random effects
  (CIs not meaningful in that case).
- Pass `re.form = ~ (1|id)` to include a specific subset of random
  effects.

## 6. Tests

Add
[`tinytest::expect_equal`](https://rdrr.io/pkg/tinytest/man/expect_equal.html)
assertions to the existing smoke-test files, especially numerical checks
on `visregFit`/`visregLwr`/`visregUpr` to catch regressions. Add a
specific test for the weights-column bug (the `weight-char.r` case).

## Order of operations

1.  ~~Bug fixes~~ (done)
2.  Drop base R / lattice
3.  snake_case rename
4.  File reorganization
5.  lme4 SE update
6.  Test assertions
