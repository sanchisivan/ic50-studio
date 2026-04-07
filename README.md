# IC50 Studio

IC50 Studio is an R Shiny app for loading dose-response data, fitting common sigmoid models, calculating IC50 or EC50 values, comparing equations, and exporting publication-ready plots.

It is designed for people who normally work in spreadsheet-based curve-fitting tools but want an open, editable workflow in R.

Developed at the **Laboratory of Bioactive Peptides (LPB)**, Faculty of Biochemistry and Biological Sciences, National University of the Littoral (UNL), Santa Fe, Argentina.

## Features

- Import `csv`, `tsv`, `txt`, `xls`, and `xlsx`
- Map your own concentration/dose, response, and grouping columns
- Normalize responses either from the observed range or from manual 100% and 0% assay controls
- Fit `4PL`, `5PL`, `3PL (Hill fixed = 1)`, `3PL (Bottom = 0)`, and `3PL (Top = 100)`
- Use `Auto-detect`, `Increasing`, or `Decreasing` curve direction
- Fit either `Group means` or `All observations`
- Compare all models without bootstrap and get a suggested equation
- Report IC50 or EC50 with `None`, `95% CI`, `+/- SD`, or `+/- SEM`
- Export plots and results tables
- Flag non-reportable IC50 values such as curves that never reach 50%
- Show short fit-quality reasons such as `Top far above data` or `50% not reached`
- Show an analysis summary window after fitting when some groups need review

## Installation

Install the required R packages once:

```r
install.packages(c("shiny", "bslib", "ggplot2", "DT", "readxl"))
```

## Run the app

Open R or RStudio in this folder and run:

```r
shiny::runApp()
```

## Expected data format

Your input file should contain:

- one numeric concentration or dose column
- one numeric response column
- one optional grouping column such as `compound`, `sample`, or `treatment`

Notes:

- Concentration values must be greater than zero for standard log-scale fitting.
- The app does not assume a fixed concentration unit.
- You can use `nM`, `uM`, `ug/mL`, `mg/mL`, or any other unit as long as the concentration column is numeric.
- Write the exact unit you want in the x-axis title.
- If you want absolute-style normalization, you can enter manual `100%` and `0%` control responses. The app uses `100 * (Y - control_0) / (control_100 - control_0)`.

An example file is included at [example_dose_response.csv](example_dose_response.csv).

## Recommended workflow

1. Upload your file.
2. Map the concentration, response, and optional group columns.
3. Leave `Curve direction` on `Auto-detect` unless you need to force it.
4. Start with `Potency uncertainty = None` for speed.
5. If you are unsure which model to use, enable `Compare all models first (no bootstrap)`.
6. Click `Run analysis`.
7. Review the suggested model and the fit table.
8. Only after the fit looks good, turn on bootstrap uncertainty for final reporting.
9. Adjust plot styling and export.

## How to choose a model

- `4PL`: good general starting point when both lower and upper plateaus are visible
- `3PL (Hill fixed = 1)`: useful when you want a simpler fixed-slope model
- `3PL (Bottom = 0)`: useful when the response should start near 0
- `3PL (Top = 100)`: useful when the response should approach 100
- `5PL`: useful when the curve is asymmetric

If you are unsure, use the model comparison option first.

## Interpreting the fit table

The app reports a `reporting_status` column to make it clearer what should and should not be reported:

- `Report numeric potency value`: the fit looks acceptable for numeric reporting
- `Do not report numeric value (target not reached)`: the observed data never reach the target response for the selected metric, so report this as above or below the tested range instead of giving a numeric value
- `Do not report numeric value (extrapolated)`: the fitted value falls outside the tested concentration range
- `Numeric value shown; review fit`: the fit exists, but the curve shape or plateau behavior needs caution
- `No fit available`: the curve could not be fit

The table also includes a `fit_reason` column with a short explanation:

- `Top far above data`: the fitted upper plateau is much higher than the observed points, so the model is extrapolating strongly
- `Bottom far below data`: the fitted lower plateau drops well below the observed low-response points
- `50% not reached`: used in IC50 mode when the observed responses never cross the 50% level
- `Half-max effect not reached`: used in EC50 mode when the observed responses do not span the fitted half-max effect level
- `Value outside tested range`: the fitted potency value lies outside the tested concentration range
- `Flat or invalid fitted range`: the fitted curve collapsed to an implausible response span

In many practical cases, a visually smooth `4PL` can still be flagged if the fitted top or bottom is unrealistic. When this happens, try a simpler model such as `3PL (Hill fixed = 1)` or expand the concentration range.

## Analysis summary window

After each run, the app can show a short summary window if any groups need attention. This summary helps you quickly see:

- how many groups are reportable
- how many groups did not reach 50%
- how many groups are extrapolated
- how many groups need manual review
- the most common `fit_reason` values in that run

Use the popup as a quick warning, then check the full `Fit Results` table for the final interpretation.

## Potency uncertainty

Potency uncertainty is optional and uses bootstrap resampling.

- `95% CI` is usually the best choice for publication
- `+/- SD` and `+/- SEM` are derived from the bootstrap distribution of the fitted potency value
- Keep uncertainty set to `None` during exploration for speed
- Around `50` bootstrap iterations is useful for a quick preview
- Around `100 to 200` bootstrap iterations is a better starting point for final reporting

## Plotting notes

- Percent-like data will default to a `0 to 100` y-axis
- The default y-axis includes `50` and `100` as reference values
- You can hide the plot title and subtitle with checkboxes
- Styling options, axis controls, and export settings are grouped in dropdown sections to keep the main panel shorter

## Troubleshooting

- If the fit looks flat or wrong, first check the selected group column and curve direction.
- If the app reports that 50% was not reached, expand the concentration range instead of forcing a numeric IC50.
- If many groups are marked `Top far above data`, the selected model is probably too flexible for the available plateau information.
- If you are working with inhibition data, consider `Invert as 100 - response`.
- If different compounds behave very differently, use the model comparison option before enabling bootstrap.

## Included help

The app also includes an `Instructions` tab with a built-in quick guide, model advice, uncertainty notes, and troubleshooting tips, plus an `About` tab with project and contact information.

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
