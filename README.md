# ICO Fundraising Success Prediction (R)

An end-to-end, reproducible R pipeline that predicts whether an Initial Coin Offering (ICO) reaches its fundraising goal. It compares five classical classifiers under a leakage-safe workflow and reports honest, cross-validated performance.

Originally written as MSc coursework (University of Leeds, Business Analytics). This repository is a refactor of that single script into a tested, configurable project.

## Pipeline

1. **Load and validate**: schema check on the raw CSV (fails early with a clear message).
2. **Clean**: normalise messy Yes/No encodings, parse dates, derive ICO and pre-sale durations, URL-presence flags, digital-presence and compliance scores, grouped country.
3. **Leakage control**: `sold_tokens` (only known after the ICO closes) is dropped before modelling.
4. **Split**: stratified 80/20 hold-out.
5. **Preprocess**: median imputation, `log1p` of skewed variables, min-max (KNN) and z-score (Naive Bayes, SVM) scaling, all fitted on training data only.
6. **Model selection**: KNN's `k` is chosen by 10-fold CV on the training set.
7. **Evaluate**: C5.0 decision tree (boosted), KNN, Gaussian Naive Bayes, linear SVM, RBF SVM, against a majority-class baseline. Hold-out metrics (accuracy, sensitivity, specificity, precision, F1, balanced accuracy, AUC), 10-fold CV stability and a train/test overfitting gap.
8. **Report**: 12 figures and 5 tables written to `outputs/`.

## What changed from the coursework script

| Issue in the original | Fix |
|---|---|
| KNN `k` tuned on the test set | Chosen by CV on the training set only |
| Imputation/scaling computed outside CV folds | Refit inside every fold |
| Hold-out and CV used different C5.0 settings | One shared `fit_predict()` for both |
| No baseline | Majority-class baseline in the comparison table |
| No view of overfitting | Train vs test accuracy gap table |
| Hard-coded paths, inline `install.packages` | Config + env vars, `install_dependencies.R` |
| Single 800-line script | Modules in `R/`, one orchestrator |
| No tests | 34 checks, run in CI |

## Quick start

```bash
Rscript install_dependencies.R
Rscript tests/run_tests.R
Rscript run_analysis.R --demo     # synthetic data, to try the pipeline
Rscript run_analysis.R            # real data, see data/README.md
```

Run from the repository root. Paths can be overridden with `ICO_DATA_PATH` and `ICO_OUT_DIR`. All settings (seed, folds, k grid, C5.0 trials) live in `R/config.R`.

## Layout

```
R/config.R        settings
R/utils.R         packages, logging, helpers
R/data_prep.R     load, clean, split
R/preprocess.R    imputation, transforms, scaling
R/models.R        fit_predict, CV folds, KNN tuning
R/evaluation.R    metrics, comparison, baseline
R/plots.R         figures
R/demo_data.R     synthetic data generator
run_analysis.R    orchestrator
tests/            unit checks
```

## Data

The coursework dataset is not redistributable; see [data/README.md](data/README.md). Metrics from `--demo` are meaningless by design.

## Requirements

R >= 4.2 with dplyr, lubridate, caret, C50, class, naivebayes, kernlab, pROC, ggplot2, reshape2, ggrepel.

## Licence

MIT
