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

**Probability under the logistic 2PL:**

$$
P(\theta) = \frac{1}{1 + e^{-a(\theta - b)}}
$$

**Item information function:**

$$
I(\theta) = a^{2} \cdot P(\theta) \cdot (1 - P(\theta))
$$

For each session, the script computes the average information of the administered items evaluated at the final $\theta$, then compares that average with the maximum information available in the pool at that same $\theta$.  
The ratio `avg_admin_info / max_pool_info` is saved per session.

### 5. Exposure and Gini

- Item exposure rates are computed as:
  - `exposure_rate_attempts = n_administered / n_distinct(session_id)`
  - `exposure_rate_examinees = unique_examinees / n_distinct(user_id)`
- Gini is computed using `DescTools::Gini()` on exposure rates.

### 6. Efficiency summaries

- Final $\theta$, final SE, number of items administered, and time taken per session (difference between last and first `answered_at_parsed`) are computed and saved.

### 7. Growth / repeated attempts

- For users with multiple attempts, the script computes `first_theta`, `last_theta`, and  
  `delta = last_theta - first_theta`, and produces a spaghetti plot for a sample of users.

## Common problems & debugging tips

- **Missing theta/SE columns**: Ensure `f1` and `se_f1` exist in the wide file.
- **Items with a single response category**: If an item has only 0s or only 1s, variance-based functions may fail. Remove such items before running diagnostics.
- **Timestamps not parsed**: If `answered_at` cannot be parsed, time-based diagnostics will be `NA`.
- **Pool–item mismatch**: If response columns in the wide file don't exist in the item pool, targeting and information calculations will be incomplete. Reconcile item IDs between datasets.
