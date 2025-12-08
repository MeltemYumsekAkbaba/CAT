### TECHNICAL DETAILS

This document explains assumptions, core computations, and implementation details of the CAT diagnostic R script included in this project.

### Key assumptions

- Item discrimination (`a`) is set to **1** for every item. If you have per-item discriminations, replace that assignment with your `a` column.
- Item difficulties (`b`) come from a trusted item pool and are numeric.
- Person θ values are pre-estimated outside the script (the script *does not* re-estimate thetas). The recommended estimator for CAT contexts is Warm’s Weighted Maximum Likelihood (WML); the `wide` file in this repository is expected to contain such outputs.
- Timestamps (if available) are parsed with `lubridate::parse_date_time` into UTC; if parsing fails, timestamps default to `NA`.

## Core computations (with formulas)

### 1. Conditional Standard Error of Measurement (CSEM)

- Plotted as `SE` versus $\theta$ (loess smooth).
- Uses `se_f1` reported in the wide file; for skills it uses the respective skill SE columns.

### 2. Marginal reliability

Computed per scale as:

$$
\text{marginal reliability} = 1 - \frac{\mathrm{mean}(SE^2)}{\mathrm{var}(\theta)}
$$

Where the mean and variance are computed across examinees with non-missing values.

### 3. Targeting

- For each session, compute mean administered `b` (mean of `raw_difficulty` for items administered in that session).
- Correlate person $\theta$ (`f1`) with mean administered `b`.
- Fit a linear model `mean_b_admin ~ f1` and report the slope; produce scatterplot + regression line.

### 4. Item information (2PL with $a = 1$)

**Probability u**
