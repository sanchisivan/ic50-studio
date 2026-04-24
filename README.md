# IC50 Studio

[![DOI](https://zenodo.org/badge/1203267905.svg)](https://doi.org/10.5281/zenodo.19609975)

Zenodo DOI: `10.5281/zenodo.19609975`

IC50 Studio is an open R Shiny app for dose-response analysis and publication-oriented figure making. It helps you load assay data, fit common sigmoid models, calculate IC50 or EC50 values, compare equations, review fit quality, and export plots and tables.

The app was developed at the **Laboratory of Bioactive Peptides (LPB)**, Faculty of Biochemistry and Biological Sciences, National University of the Littoral (UNL), Santa Fe, Argentina.

## Documentation

- Quick overview: this `README.md`
- Full guide: [USER_MANUAL.md](USER_MANUAL.md)
- In-app guide: the `Instructions` tab

## Main capabilities

- Import `csv`, `tsv`, `txt`, `xls`, and `xlsx` files
- Paste tab-separated or comma-separated tables copied from Excel
- Map your own dose, response, and optional group columns
- Preview imported columns, suggested mapping, and read diagnostics before fitting
- Validate raw data with warnings plus a quick scatter preview before running the fit
- Accept either linear concentration columns or already `log10`-transformed concentration columns
- Fit `4PL`, `5PL`, `3PL (Hill fixed = 1)`, `3PL (Bottom = 0)`, and `3PL (Top = 100)`
- Report either `IC50` or `EC50`
- Use `Auto-detect`, `Increasing`, or `Decreasing` curve direction
- Fit either `Group means` or `All observations`
- Normalize responses with min/max scaling, manual `0%` and `100%` controls, or zero-dose controls per group
- Compare all supported models before running bootstrap uncertainty
- Choose automatic, measured-dose, or custom x-axis breaks and show either concentration or `log10(concentration)` axis labels
- Control curve error-bar cap width independently from the x-value spacing
- Export curve plots, fit tables, other plots, and other-plot summary tables
- Build `Bar plot`, `Boxplot`, and `Line plot` figures in the `Other Plots` tab
- Add manual annotation labels or automatic significance letters to bar plots
- Edit uploaded or pasted data directly inside the app with the spreadsheet-style `Table Editor`
- Keep the same compound or series colors across different plots with fixed label-to-color assignments
- Keep the same compound point shapes across curve plots with fixed label-to-shape assignments

## Installation

Install the required packages once:

```r
install.packages(c("shiny", "bslib", "ggplot2", "DT", "readxl"))
```

Optional but recommended for the best in-app table-editing experience:

```r
install.packages("rhandsontable")
```

## Run the app

Open R or RStudio in this folder and run:

```r
shiny::runApp()
```

## Quick start

1. Launch the app.
2. Click `Use example dataset`, upload your own file, or use the `Paste data` tab to paste a table copied from Excel.
3. Review the automatic `Import preview` window to confirm how the table was read and what column mapping was suggested.
4. If needed, open the `Table Editor` tab to correct cells, paste full columns or table blocks, add rows, or remove rows before fitting.
5. Map the `Concentration or dose column`, `Response column`, and optional `Group or compound column`.
6. Set `Uploaded concentration values` so the app knows whether your file stores linear concentrations or already `log10`-transformed values.
7. Open `Data Preview` and check the validation warnings, raw-data preview, and scatter plot before fitting.
8. Start with `Fit curve using = Group means`.
9. Keep `Potency uncertainty = None` during exploration.
10. If you are unsure which equation fits best, enable `Compare all models first (no bootstrap)`.
11. Click `Run analysis`.
12. Review `Curve Plot`, `Fit Results`, and `Model Comparison`.
13. Only after the fit looks right, enable bootstrap uncertainty for final reporting.
14. If the same compounds or conditions should keep the same color across multiple figures, enter them in `Plot styling options > Fixed colors for specific series / compounds`.
15. If the same compounds should keep the same point shape in curve plots, enter them in `Plot styling options > Fixed point shapes for specific series / compounds`.
16. Use the `Other Plots` tab if you want publication-style assay figures that do not require curve fitting.

## Input data expectations

For dose-response fitting, your imported table should contain:

- one numeric dose or concentration column
- one numeric response column
- one optional grouping column such as `compound`, `sample`, `treatment`, or `peptide`

You can supply that table by uploading a supported file or by pasting a tab-separated or comma-separated block with a header row into the `Paste data` tab.
The built-in example dataset includes a `replicate` column, but that column is optional in your own data. The app can still summarize repeated rows that share the same group and concentration even when no replicate label column is present.

Important behavior:

- Negative doses are removed.
- The app accepts either linear concentrations such as `0.01`, `0.1`, `1`, or `10`, or already `log10`-transformed values such as `-2`, `-1`, `0`, or `1`. Use `Uploaded concentration values` to match your file.
- Zero-dose rows in linear-concentration files are kept in the preview and can be used for `Normalize to zero-dose control (per group)`. By default they stay out of the nonlinear fit, but you can turn on `Include zero-dose rows in fitting` to replace concentration `0` with a small positive surrogate one log10 decade below the minimum positive concentration.
- Each fitted group needs at least `4` distinct dose values used for fitting after any log10-to-linear conversion. By default the app counts only positive doses; when `Include zero-dose rows in fitting` is enabled, concentration `0` can count as one of those levels.
- Before fitting, the app warns when a mapped dose or response column contains non-numeric or missing values, or when a group has fewer than `4` distinct positive doses.
- The app does not assume a fixed concentration unit. You can use `nM`, `uM`, `ug/mL`, `mg/mL`, or any other unit as long as the dose column is numeric.
- `IC50` and `EC50` are always reported in linear concentration units, even when the uploaded concentration column is already log10-transformed.
- If you choose `Normalize using manual 0% and 100% controls`, the app applies `100 * (Y - control_0) / (control_100 - control_0)`.
- If you choose `Normalize to zero-dose control (per group)`, the app applies `100 * Y / mean(Y at concentration 0)` when the zero-dose mean is non-zero. If that mean is `0`, the app keeps the zero-dose response as the `0%` baseline and maps the strongest observed response away from it to `100%`.

An example file is included at [example_dose_response.csv](example_dose_response.csv).

## Import preview and validation

- After each file upload, Excel-sheet change, or pasted-data parse, the app opens an `Import preview` window automatically.
- The import preview shows the detected separator when relevant, the first rows as read, suggested column mapping, variable names, and read diagnostics such as suspicious headers or non-numeric mapped columns.
- The `Open import preview` button reopens that window at any time.
- The `Table Editor` tab lets you edit the imported table directly inside the app. With `rhandsontable` installed, you can paste full columns or rectangular blocks copied from Excel or another spreadsheet.
- The `Data Preview` tab now includes a pre-fit validation block with warning messages, the first `10` raw rows, and a simple raw scatter plot with a `log10` x-axis so you can sanity-check the data shape before fitting.

## Dose-response highlights

- `IC50` uses a fixed response target of `50`.
- `EC50` uses the half-max effect between the fitted bottom and fitted top.
- `Auto-detect` direction chooses the better fit between increasing and decreasing directions when needed.
- `Weighting = 1 / SD^2 from means` is only meaningful with `Fit curve using = Group means`.
- If you switch to `All observations`, the app resets weighting to `None`.
- `95% CI`, `+/- SD`, and `+/- SEM` are bootstrap-based uncertainty estimates for the fitted potency value.

## Plot controls

- `Use log10 concentration axis` changes the x-axis spacing, not the underlying reported concentration units.
- `Log10 axis labels` can show either the concentration values or the displayed `log10(concentration)` values.
- `X-axis breaks` can be `Automatic`, `Measured concentrations`, or `Custom`.
- In `Custom` mode, x breaks and x limits follow the same scale shown on the axis.
- `Error bar cap width (% of x-axis span)` controls the visual cap width so it stays consistent across x-axis scales.

## Choosing a dose-response model

| Model | Parameters | Typical use | Main caution |
|---|---|---|---|
| `4PL` | bottom, top, midpoint, Hill slope | Best general starting point for inhibitor, viability, and activation-style curves | Assumes a symmetric sigmoid around the midpoint |
| `5PL` | bottom, top, midpoint, Hill slope, asymmetry | Useful for clearly asymmetric curves, especially in immunoassay-style calibration or other skewed bioassays | Easier to overfit when there are few concentrations or weak plateau coverage |
| `3PL (Hill fixed = 1)` | bottom, top, midpoint | Simple Langmuir-like shape when you want a fixed slope across groups | Can miss genuinely steep or shallow transitions |
| `3PL (Bottom = 0)` | top, midpoint, Hill slope | Useful when the biology strongly supports a true zero baseline | Wrong if the lower plateau is not actually near zero |
| `3PL (Top = 100)` | bottom, midpoint, Hill slope | Useful for well-normalized percent-response assays with a meaningful 100% upper bound | Wrong if the upper plateau is not truly constrained to 100 |

See [USER_MANUAL.md](USER_MANUAL.md) for detailed model explanations, Hill-slope interpretation, and scientific references.

## Other Plots module

Use the `Other Plots` tab for figures that are common in inhibitor, hemolysis, viability, or other bioassay workflows.

Supported plot types:

- `Bar plot`
- `Boxplot`
- `Line plot`

Useful details:

- The module uses the same uploaded dataset as the curve-fitting workflow, but has its own `X`, `Y`, `Series / color`, `Facet`, and `Optional annotation` mapping.
- The `Fixed colors for specific series / compounds` setting also applies here, so repeated labels can keep the same color across curve plots and other plots.
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
- If imported columns look wrong, reopen `Import preview` and confirm the separator, header row, and suggested mapping.
- If `Data Preview` warns that a group has fewer than `4` unique doses, expand the tested range or do not expect a stable sigmoidal fit for that group.
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
