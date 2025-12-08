## CAT Diagnostics — English Language Proficiency Assessment

### Purpose

This project contains analysis code to run computer-adaptive test (CAT) diagnostics for an English language proficiency assessment. The analyses focus on overall and skill-level ability estimation diagnostics (CSEM), targeting, item information, exposure, efficiency, and growth across repeated attempts. The code is written in R and expects pre-computed person θs (WML) and item pool difficulties (b). For θ estimation see "Ability Estimation: English Proficiency Assessment" project folder.

### Quick highlights

* Plots **Conditional Standard Error of Measurement (CSEM)** plots for overall and skill-level θs.
* Computes **marginal reliability** for overall and skill subscales.
* Produces **targeting diagnostics** (mean administered item b vs person θ) with correlation and slope.
* Computes **item information** for administered items at the final theta and compares to pool maximum information.
* Produces **efficiency summaries** (final θ, final SE, number of items, time taken) per attempt.
* Generates **item-level diagnostics**: ICCs, item information curves, pool difficulty distribution.
* Computes **item exposure** metrics and **Gini** coefficients; draws Lorenz-style curves for exposure.
* Performs a **growth analysis** for repeated attempts and outputs summary change scores.


