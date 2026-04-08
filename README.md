# IC50 Studio

IC50 Studio is an open R Shiny app for dose-response analysis and publication-oriented figure making. It helps you load assay data, fit common sigmoid models, calculate IC50 or EC50 values, compare equations, review fit quality, and export plots and tables.

The app was developed at the **Laboratory of Bioactive Peptides (LPB)**, Faculty of Biochemistry and Biological Sciences, National University of the Littoral (UNL), Santa Fe, Argentina.

## Documentation

- Quick overview: this `README.md`
- Full guide: [USER_MANUAL.md](USER_MANUAL.md)
- In-app guide: the `Instructions` tab

## Main capabilities

- Import `csv`, `tsv`, `txt`, `xls`, and `xlsx` files
- Map your own dose, response, and optional group columns
- Fit `4PL`, `5PL`, `3PL (Hill fixed = 1)`, `3PL (Bottom = 0)`, and `3PL (Top = 100)`
- Report either `IC50` or `EC50`
- Use `Auto-detect`, `Increasing`, or `Decreasing` curve direction
- Fit either `Group means` or `All observations`
- Compare all supported models before running bootstrap uncertainty
- Export curve plots, fit tables, other plots, and other-plot summary tables
- Build `Bar plot`, `Boxplot`, and `Line plot` figures in the `Other Plots` tab
- Add manual annotation labels or automatic significance letters to bar plots

## Installation

Install the required packages once:

```r
install.packages(c("shiny", "bslib", "ggplot2", "DT", "readxl"))
```

## Run the app

Open R or RStudio in this folder and run:

```r
shiny::runApp()
```

## Quick start

1. Launch the app.
2. Click `Use example dataset` or upload your own file.
3. Map the `Concentration or dose column`, `Response column`, and optional `Group or compound column`.
4. Start with `Fit curve using = Group means`.
5. Keep `Potency uncertainty = None` during exploration.
6. If you are unsure which equation fits best, enable `Compare all models first (no bootstrap)`.
7. Click `Run analysis`.
8. Review `Curve Plot`, `Fit Results`, and `Model Comparison`.
9. Only after the fit looks right, enable bootstrap uncertainty for final reporting.
10. Use the `Other Plots` tab if you want publication-style assay figures that do not require curve fitting.

## Input data expectations

For dose-response fitting, your file should contain:

- one numeric dose or concentration column
- one numeric response column
- one optional grouping column such as `compound`, `sample`, `treatment`, or `peptide`

Important behavior:

- Negative doses are removed.
- Zero-dose rows are kept for preview and linear-axis plotting, but excluded from the actual curve fit.
- Each fitted group needs at least `4` distinct positive dose values.
- The app does not assume a fixed concentration unit. You can use `nM`, `uM`, `ug/mL`, `mg/mL`, or any other unit as long as the dose column is numeric.
- If you choose `Normalize using manual 0% and 100% controls`, the app applies `100 * (Y - control_0) / (control_100 - control_0)`.

An example file is included at [example_dose_response.csv](example_dose_response.csv).

## Dose-response highlights

- `IC50` uses a fixed response target of `50`.
- `EC50` uses the half-max effect between the fitted bottom and fitted top.
- `Auto-detect` direction chooses the better fit between increasing and decreasing directions when needed.
- `Weighting = 1 / SD^2 from means` is only meaningful with `Fit curve using = Group means`.
- If you switch to `All observations`, the app resets weighting to `None`.
- `95% CI`, `+/- SD`, and `+/- SEM` are bootstrap-based uncertainty estimates for the fitted potency value.

## Other Plots module

Use the `Other Plots` tab for figures that are common in inhibitor, hemolysis, viability, or other bioassay workflows.

Supported plot types:

- `Bar plot`
- `Boxplot`
- `Line plot`

Useful details:

- The module uses the same uploaded dataset as the curve-fitting workflow, but has its own `X`, `Y`, `Series / color`, `Facet`, and `Optional annotation` mapping.
- `Bar plot` and `Line plot` can summarize replicates with `Mean` or `Median`.
- Available error bars are `SEM`, `SD`, `95% CI`, `IQR`, or `None`.
- `Boxplot` is best for raw replicate distributions, not pre-summarized means.
- `Bar plot` supports either manual annotation labels or automatic significance letters.

Automatic letters in bar plots:

- Methods: `ANOVA + Tukey HSD` or `Kruskal-Wallis + pairwise Wilcoxon (Holm)`
- Default comparison scope: `Within each x group in each facet`
- Recommended use: independent biological replicates, not only technical repeats
- Important limitation: the lettering is a one-factor comparison inside the selected scope and does not replace a full factorial statistical analysis

## Reading the results

The `Fit Results` table includes a `reporting_status` column to help decide whether a numeric potency value should be reported:

- `Report numeric potency value`
- `Do not report numeric value (target not reached)`
- `Do not report numeric value (extrapolated)`
- `Numeric value shown; review fit`
- `No fit available`

The `fit_reason` and `reporting_note` columns explain why a row needs caution.

## Model comparison

When `Compare all models first (no bootstrap)` is enabled, the app ranks supported equations by:

1. how many groups give reportable numeric potency values
2. how many groups fit successfully
3. how many groups still need review
4. median `R-squared`
5. simpler models when the earlier criteria are tied

This makes it easier to choose a practical model before spending time on bootstrap uncertainty.

## Exports

You can export:

- the main curve plot
- the fit-results CSV
- the other-plot figure
- the other-plot summary CSV

The export settings panel controls file name, format, units, width, height, and DPI for both plotting modules.

## Troubleshooting

- If a curve looks wrong, first check the mapped columns and `Curve direction`.
- If many groups are flagged as `Top far above data` or `Bottom far below data`, try a simpler model or expand the tested concentration range.
- If the app says the target was not reached, do not force a numeric IC50 or EC50. Report that the effect was not reached within the tested range.
- If you want a boxplot, make sure your rows are raw replicate values. If your file already contains means plus SD or SEM, use a bar plot instead.

## Contact

| | |
|---|---|
| **Dr. Ivan Sanchis** | sanchisivan@fbcb.unl.edu.ar<br>sanchisivan@gmail.com |

Laboratory of Bioactive Peptides (LPB)  
Faculty of Biochemistry and Biological Sciences (FBCB)  
National University of the Littoral (UNL)  
Santa Fe, Argentina

Repository: [github.com/sanchisivan/ic50-studio](https://github.com/sanchisivan/ic50-studio)

## License

MIT License. See [LICENSE](LICENSE) for the full text.
