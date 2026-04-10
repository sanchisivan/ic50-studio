# IC50 Studio User Manual

This manual explains how to use IC50 Studio in a practical, step-by-step way. It is intended for researchers working with dose-response, inhibition, activation, hemolysis, viability, and related bioassay datasets who want a transparent workflow in R without having to code every figure and fit manually.

If you only want the short version, read [README.md](README.md). If you want the built-in quick guide, open the `Instructions` tab inside the app.

## Contents

1. [What the app does](#1-what-the-app-does)
2. [Installation and launch](#2-installation-and-launch)
3. [Interface overview](#3-interface-overview)
4. [Preparing your data](#4-preparing-your-data)
5. [Recommended dose-response workflow](#5-recommended-dose-response-workflow)
6. [Analysis settings reference](#6-analysis-settings-reference)
7. [Understanding the dose-response models](#7-understanding-the-dose-response-models)
8. [Understanding the output tabs](#8-understanding-the-output-tabs)
9. [Curve-plot customization and export](#9-curve-plot-customization-and-export)
10. [Other Plots module](#10-other-plots-module)
11. [Statistical guidance and interpretation](#11-statistical-guidance-and-interpretation)
12. [Example workflows](#12-example-workflows)
13. [Troubleshooting](#13-troubleshooting)
14. [Good-practice checklist](#14-good-practice-checklist)
15. [Contact and license](#15-contact-and-license)
16. [References](#16-references)

## 1. What the app does

IC50 Studio has two main jobs:

- fit dose-response curves and calculate IC50 or EC50 values
- build publication-oriented assay figures that do not require a sigmoid fit

### Dose-response analysis features

- import `csv`, `tsv`, `txt`, `xls`, and `xlsx`
- map your own dose, response, and group columns
- normalize or transform responses before fitting
- fit `4PL`, `5PL`, `3PL (Hill fixed = 1)`, `3PL (Bottom = 0)`, and `3PL (Top = 100)`
- work in `IC50` or `EC50` mode
- use either `Group means` or `All observations`
- compare all available models before bootstrap uncertainty
- flag non-reportable potency values such as target not reached or extrapolated
- export plots and full results tables

### Other plotting features

- grouped `Bar plot`
- replicate-distribution `Boxplot`
- summary `Line plot`
- manual annotation labels from your file
- automatic significance letters for bar plots
- shared export settings with the curve plot tab

### What the app does not try to be

IC50 Studio is meant to be practical and transparent, not a full replacement for every statistical workflow. In particular:

- automatic significance letters are available only for bar plots
- automatic letters are one-factor comparisons inside the selected comparison scope
- the app does not replace a full factorial model when your design has interactions such as `compound x concentration`
- users should still decide whether their replicates are independent biological observations or only technical repeats

## 2. Installation and launch

Install the required packages once:

```r
install.packages(c("shiny", "bslib", "ggplot2", "DT", "readxl"))
```

Open R or RStudio in the project folder and run:

```r
shiny::runApp()
```

When the app opens, you can either:

- upload your own file
- click `Use example dataset`

The built-in example dataset contains:

- `Compound A`
- `Compound B`
- `8` concentrations per compound
- `3` replicates per concentration

This example is useful because it works for both dose-response fitting and the `Other Plots` module.

## 3. Interface overview

The app is divided into a left sidebar for controls and a main area with tabs for plots, tables, and guidance.

### Sidebar sections

#### `Data and mapping`

Use this section to:

- upload a file
- choose an Excel sheet when needed
- map the dose, response, and group columns

#### `Analysis settings`

This section controls:

- response scaling
- fitting mode
- response transformation
- curve direction
- model choice
- potency metric
- model comparison
- weighting
- uncertainty
- curve resolution

#### `Curve plot quick controls`

This section controls:

- plot title
- plotting preset
- log10 x-axis
- curve extension toward zero
- guide lines for potency and 50% response

#### `Plot styling options`

This section controls the appearance of the main curve plot:

- axis titles
- plot style
- grid lines
- palette
- legend position and legend content
- raw-point visibility
- SD error bars
- faceting
- font size
- point size
- line width

#### `Other plots module`

This section contains the controls for the `Other Plots` tab:

- plot type
- other-plot column mapping
- summary statistic
- error bars
- raw-point overlay
- annotation mode
- automatic letters options
- x-axis log scaling for numeric x values
- title and axis labels

#### `Curve plot axis breaks and limits`

Use this section to enter custom:

- x breaks
- y breaks
- x limits
- y limits

These fields accept comma-, semicolon-, or space-separated numbers. Axis limits must contain exactly two numbers in increasing order.

#### `Export settings (all plots)`

This section controls export for both plotting modules:

- file name
- file format
- units
- width
- height
- DPI

### Main tabs

#### `Curve Plot`

Shows the main fitted dose-response figure and the export buttons for:

- the plot file
- the fit table CSV

#### `Fit Results`

Shows the main results table with:

- reported potency values
- fit reliability
- fit warnings
- R-squared
- fit status

#### `Model Comparison`

Shows the no-bootstrap comparison across all supported models when enabled.

#### `Data Preview`

Shows:

- the imported raw data
- the prepared summary used for grouped fitting
- the other-plot summary table

#### `Other Plots`

Shows:

- notes for the selected other-plot workflow
- the selected bar, box, or line figure
- export buttons for the other plot and its summary table

#### `Instructions`

Contains a built-in quick guide inside the app.

#### `About`

Contains the project description, affiliation, contact information, repository link, and license note.

## 4. Preparing your data

### Supported file formats

IC50 Studio accepts:

- `CSV`
- `TSV`
- `TXT`
- `XLS`
- `XLSX`

If you upload an Excel workbook, the app lets you choose the sheet.

### Minimum structure for dose-response fitting

For curve fitting, your dataset should include:

- one numeric dose or concentration column
- one numeric response column
- one optional group column

Examples of good group columns:

- `compound`
- `sample`
- `treatment`
- `peptide`
- `variant`

If you do not choose a group column, the app treats the dataset as one single series.

### Recommended raw-data layout

The best input format is one row per observation. Example:

```text
compound,concentration,response,replicate
Compound A,0.01,99,R1
Compound A,0.01,101,R2
Compound A,0.01,98,R3
Compound A,0.03,97,R1
...
```

This layout works well because the app can:

- calculate group means and SD
- fit curves from either summarized means or all observations
- compute raw-point overlays in plots
- calculate automatic bar-plot letters from the raw replicate rows

### What happens during preparation

For the dose-response workflow:

- non-numeric dose or response rows are removed
- rows with missing group labels are removed
- negative doses are removed
- zero-dose rows are kept for preview and linear plots
- zero-dose rows are excluded from the actual fit unless you enable `Include zero-dose rows in fitting`, which replaces concentration `0` with a small positive surrogate for log-style fitting

### Important rule for fitting

Each fitted group needs at least `4` distinct dose values used for fitting.

By default the app counts only positive doses. If you enable `Include zero-dose rows in fitting`, concentration `0` can count as one of those levels.

If a group has fewer than `4` distinct fitted doses:

- it stays visible in previews
- it is counted in diagnostic notes
- it is not fit successfully

### Units

The app does not assume a fixed concentration unit. You can use:

- `nM`
- `uM`
- `ug/mL`
- `mg/mL`
- any other unit

The only requirement is that the dose column itself must be numeric. Add the unit you want in the x-axis title.

### Zero-dose controls

Zero-dose rows are often useful as assay controls or baseline observations. The app handles them in a practical way:

- they remain in the imported data preview
- they remain in the summarized data preview
- they can appear in the `Other Plots` module
- they can appear in linear-axis curve displays
- by default they are excluded from the mathematical fit itself
- they can be included in the mathematical fit when `Include zero-dose rows in fitting` is enabled for linear concentration data, using a small positive surrogate value
- they cannot be shown at x = 0 on a log axis because log-scale plots cannot display zero, even when they are included in the fit

### Raw replicates versus summarized means

The app works best when your dataset contains raw replicates. If you only have a table of means plus SD or SEM:

- the dose-response fitter can still work if the dose-response structure is present
- the `Other Plots` module can still make bar or line plots from those summarized values
- boxplots are not appropriate, because a boxplot needs raw replicate values
- automatic bar-plot letters are not appropriate unless the rows being compared are actual independent observations

## 5. Recommended dose-response workflow

This is the safest default workflow for most users.

### Step 1. Load data

Use either:

- `Upload data`
- `Use example dataset`

If you load Excel, choose the correct sheet.

### Step 2. Check column mapping

Review:

- `Concentration or dose column`
- `Response column`
- `Group or compound column`

The app guesses sensible defaults from column names, but you should always confirm the mapping.

### Step 3. Start with conservative settings

Recommended starting values:

- `Response scaling = Raw values`
- `Fit curve using = Group means`
- `Response transform = As entered`
- `Curve direction = Auto-detect`
- `Model = 4PL`
- `Potency metric = IC50`
- `Potency uncertainty = None`

### Step 4. Decide whether the response needs transformation

Examples:

- If higher measured response means weaker inhibition, use `Invert as 100 - response`.
- If the data are already in the desired biological direction, keep `As entered`.
- Use `Mirror around min/max` only when you specifically want to reflect the response around the observed range.

### Step 5. Decide whether to normalize

Use normalization when your raw response units vary or when you want a percent-like scale.

Useful choices:

- `Raw values` if the data are already on a meaningful scale
- `Normalize 0 to 100 (min to max)` when the lowest response should become 0 and the highest should become 100
- `Normalize 100 to 0 (max to min)` when the highest response should become 0 and the lowest should become 100
- `Normalize using manual 0% and 100% controls` when you want assay-control-based scaling

### Step 6. Run a fast first fit

Before spending time on bootstrap uncertainty:

- keep `Potency uncertainty = None`
- optionally enable `Compare all models first (no bootstrap)`
- click `Run analysis`

### Step 7. Review the fit

Check:

- the visual shape of the fitted curves
- whether the chosen model matches the biology
- the `reporting_status` column
- the `fit_reason` column
- the `Model Comparison` tab if comparison was enabled

### Step 8. Only then enable uncertainty

For final reporting:

- use `95% CI` in most publication contexts
- use `+/- SD` or `+/- SEM` only if you specifically want those bootstrap summaries
- increase bootstrap iterations from the quick-preview value

A practical rule:

- `50` iterations for a quick preview
- `100` to `200` iterations as a better starting point for final reporting

### Step 9. Customize the figure

After the fit looks correct:

- adjust titles, labels, and style
- decide whether to use log10 x-axis
- set the export size
- export the plot and CSV

## 6. Analysis settings reference

This section explains every control in `Analysis settings`.

### `Response scaling`

#### `Raw values`

No scaling is applied. Use this when your response values are already in the units you want to interpret.

#### `Normalize 0 to 100 (min to max)`

The lowest observed response in each group becomes `0` and the highest becomes `100`.

Use this when:

- different groups are on similar biology but different raw scales
- you want a standard percent-like range

#### `Normalize 100 to 0 (max to min)`

This is the reverse version of min-max normalization. The highest observed response becomes `0` and the lowest becomes `100`.

Use this when:

- your assay is naturally decreasing and you want a percent-inhibition style scale
- you want the visual direction to match an inhibition interpretation

#### `Normalize using manual 0% and 100% controls`

This mode uses the values you enter in:

- `100% control response`
- `0% control response`

Formula:

```text
100 * (Y - control_0) / (control_100 - control_0)
```

Use this when:

- you have assay controls that define the absolute 0% and 100% states
- you want GraphPad-like control normalization

### `Fit curve using`

#### `Group means`

The app first summarizes replicates at each dose and then fits the curve to those means.

Advantages:

- stable and easy to interpret
- compatible with `Weighting = 1 / SD^2 from means`
- often a good default for bioassay datasets with repeated measurements per dose

#### `All observations`

The app fits the curve directly to all observation rows.

Advantages:

- keeps every measured value in the fit
- useful when you want raw-observation fitting

Tradeoffs:

- weighting from the summarized SD table is not available
- groups with unequal replicate counts can influence the fit differently

When you select `All observations`, the app resets weighting to `None`.

### `Response transform`

#### `As entered`

Uses the values exactly as they are after any selected scaling.

#### `Invert as 100 - response`

Transforms the response as:

```text
100 - response
```

Useful for:

- inhibition-style assays
- viability or activity readouts where the measured value moves in the opposite direction from the effect you want to fit

#### `Mirror around min/max`

Reflects the response around the observed range.

Use this only when you specifically want a mirrored version of the data and understand how that changes the biological meaning.

### `Curve direction`

#### `Auto-detect`

This is the recommended default. The app evaluates the shape and keeps the direction that fits better when needed.

#### `Increasing`

Use this when the biological response rises with increasing dose.

#### `Decreasing`

Use this when the biological response falls with increasing dose.

### `Model`

Supported equations:

- `4PL`
- `5PL`
- `3PL (Hill fixed = 1)`
- `3PL (Bottom = 0)`
- `3PL (Top = 100)`

More guidance is provided in Section 7.

### `Potency metric`

#### `IC50`

The app solves for the dose where the response equals `50`.

This is most natural when your response scale is already interpreted around a fixed 0 to 100 effect scale.

#### `EC50`

The app solves for the half-max effect between the fitted bottom and fitted top.

This is useful when:

- the assay is activation-like
- the dynamic range is not centered on an absolute 50% response

### `Compare all models first (no bootstrap)`

When enabled, the app fits all supported models without bootstrap uncertainty and ranks them by practical reportability. This helps you choose a model before running slower uncertainty estimation.

### `Weighting`

#### `None`

Every summarized point contributes equally.

#### `1 / SD^2 from means`

Uses the SD from the summarized replicate table to give lower weight to noisier dose levels.

Important:

- this only makes sense with `Fit curve using = Group means`
- it is unavailable in practice when fitting `All observations`

### `Potency uncertainty`

Available choices:

- `95% CI`
- `+/- SD`
- `+/- SEM`
- `None`

These values come from bootstrap resampling of the fitted potency value, not from the raw SD at each dose.

### `Potency decimal places`

Controls the displayed decimal places for:

- potency values
- bootstrap summaries linked to potency

### `Bootstrap iterations`

Controls how many bootstrap resamples are used when uncertainty is enabled.

Higher values:

- are slower
- give more stable uncertainty summaries

### `Curve resolution`

Controls how many points are used to draw the fitted curve. Higher values make the line smoother but are usually unnecessary beyond a moderate range.

## 7. Understanding the dose-response models

### Hill equation and Hill coefficient

The supported curve equations in IC50 Studio are Hill-type logistic models (Hill, 1910; Weiss, 1997; Goutelle et al., 2008). In this family of models, the midpoint parameter controls where the transition happens on the dose axis, while the Hill coefficient controls how steeply the curve changes around that midpoint.

Practical interpretation:

- a larger Hill slope gives a sharper transition
- a smaller Hill slope gives a broader transition
- in real bioassays, the Hill coefficient is usually best treated as a descriptive steepness term, not as a literal count of binding sites (Weiss, 1997; Sebaugh, 2011)

For classic inhibitor workflows, the Hill framework is useful because it provides a stable empirical way to summarize potency and response shape even when the full biochemical mechanism is more complicated than a single-site binding scheme. For a modern general overview of linear and nonlinear regression in quantitative bioassays, see Jarantow et al. (2023).

### Quick comparison

| Model | Free parameters | Best use case | Main caution |
|---|---|---|---|
| `4PL` | bottom, top, midpoint, Hill slope | Best default for most inhibitor, viability, and activation-like datasets | Assumes a symmetric sigmoid |
| `5PL` | bottom, top, midpoint, Hill slope, asymmetry | Useful when the curve is reproducibly asymmetric | Can overfit sparse data and unstable plateaus |
| `3PL (Hill fixed = 1)` | bottom, top, midpoint | Useful when you want a simple fixed-slope fit | Can miss real steepness differences |
| `3PL (Bottom = 0)` | top, midpoint, Hill slope | Useful when a true zero baseline is biologically justified | Misleading if the baseline is not actually zero |
| `3PL (Top = 100)` | bottom, midpoint, Hill slope | Useful for well-normalized percent-scale assays | Misleading if the upper plateau is not truly 100 |

### `4PL`

Best default starting point when:

- you expect a sigmoidal relationship
- both the lower and upper plateaus are reasonably represented

Why it is the usual default:

- it matches the standard symmetric Hill-type dose-response shape used in many inhibitor and viability workflows
- it is flexible enough for most datasets without adding an asymmetry parameter that may be poorly constrained
- it is usually the safest first model for enzyme inhibitor IC50 analysis unless the data clearly show reproducible asymmetry

This is the standard practical starting point in many assay-analysis guides and ligand-binding workflows (Sebaugh, 2011; Xiang et al., 2018). Recent assay papers still use `4PL` as a routine calibration model, including competitive ELISA validation work (Garg et al., 2022). A recent protocol on generating drug-resistant cell lines also treats `4PL` as a suitable default for `IC50` estimation when the full sigmoid is represented by the data (Kadomoto et al., 2025).

### `5PL`

Useful when:

- the curve is asymmetric
- one side of the sigmoid looks stretched relative to the other

Tradeoff:

- more flexibility
- greater chance of unstable or hard-to-interpret plateau behavior if the dataset is sparse

Why and when to use it:

- `5PL` adds an asymmetry term, so the midpoint and the reported `IC50` or `EC50` are no longer forced into the same symmetric geometry as in `4PL`
- this is especially useful when the assay response is systematically skewed rather than symmetric
- the scientific support for `5PL` is strongest in asymmetric assay settings such as immunoassay calibration and other bioassays with visibly skewed sigmoids

Methodological work comparing `4PL` and `5PL` shows that the extra asymmetry parameter is useful when asymmetry is real, but it is not automatically better in every dataset because the bias-variance tradeoff can still favor `4PL` in some situations (Gottschalk and Dunn, 2005; Cumberland et al., 2015). More recent assay-oriented examples include asymmetric immunoassay design work (Hyun et al., 2020) and quantitative ELISA studies that explicitly use `5PL` standard curves (Vernet et al., 2021; Carducci et al., 2024).

For enzyme inhibitors:

- `5PL` can still be useful if your inhibitor curve is clearly asymmetric and that pattern is reproducible across repeats or related compounds
- it should usually be treated as a specialist option rather than the default for standard enzyme inhibitor datasets
- if the asymmetry disappears when you improve concentration spacing or replicate quality, the extra parameter was probably not justified
- when in doubt, start with `4PL`, then use model comparison and visual inspection to decide whether `5PL` is really improving the fit in a meaningful way

### `3PL (Hill fixed = 1)`

Useful when:

- you want a simpler fit
- you prefer a fixed slope for consistency across groups
- the data do not justify a fully flexible Hill slope

Reduced-parameter sigmoid models are most useful when the data do not support a fully unconstrained fit or when you want a more stable comparison across related groups (Sebaugh, 2011). In practical assay work, reduced-parameter fits are commonly recommended when a full sigmoid is not obtained or when some curve features should be constrained rather than estimated freely (Kadomoto et al., 2025).

### `3PL (Bottom = 0)`

Useful when:

- the response should approach zero at one end
- the biology strongly supports a zero baseline

This is best treated as a constrained pragmatic fit, not a separate mechanistic model. It is most appropriate when the lower plateau is known from the assay design or normalization scheme rather than weakly inferred from sparse data (Sebaugh, 2011; Kadomoto et al., 2025).

### `3PL (Top = 100)`

Useful when:

- the response should approach 100 at one end
- you are working on a normalized percent-like scale with a meaningful upper bound

As with `3PL (Bottom = 0)`, this is a constrained practical model. It is most useful when the upper plateau really is anchored by assay design, control-based normalization, or a clear biological maximum rather than only by wishful interpretation of the fitted curve (Sebaugh, 2011; Kadomoto et al., 2025).

### Practical model choice advice

If you are unsure:

1. start with `4PL`
2. enable `Compare all models first (no bootstrap)`
3. inspect `reporting_status`, `fit_reason`, and curve realism
4. use a simpler model if the more flexible one is producing unrealistic plateaus

What to look for when deciding between `4PL` and `5PL`:

- choose `4PL` if the curve shape looks approximately symmetric and the plateaus are biologically reasonable
- consider `5PL` if one side of the transition is consistently more stretched or compressed than the other
- do not choose `5PL` only because it gives a slightly better `R-squared`
- be cautious with `5PL` when you have few concentration levels or weak coverage of one plateau

Choosing between `4PL` and `5PL` should not rely only on a slightly better fit statistic, because `5PL` can lose robustness when asymmetry is weak or the data are too sparse to support the extra parameter (Cumberland et al., 2015). Full article details for the citations used in this section are listed in Section 16.

## 8. Understanding the output tabs

### `Curve Plot`

This tab shows the fitted figure. Depending on your settings it can include:

- raw points
- summarized points
- SD error bars
- fitted curves
- potency guide lines
- a 50% reference line
- faceting by group

### Log-scale note

If `Use log10 concentration axis` is enabled:

- the x-axis becomes logarithmic
- zero-dose rows cannot be placed at x = 0
- curve extension toward zero is drawn as extra decades below the minimum positive dose, not at literal zero

### `Fit Results`

This is the main interpretation table.

Common visible columns include:

- `group`
- reported potency value
- `reporting_note`
- `reporting_status`
- `fit_reliability`
- `fit_reason`
- `suggested_direction`
- `r_squared`
- `n_points`
- `fit_status`

The exported CSV contains additional columns beyond the most prominent visible ones.

### `reporting_status`

#### `Report numeric potency value`

The app considers the potency value suitable to report numerically.

#### `Do not report numeric value (target not reached)`

The observed data do not reach the target needed for the selected potency metric. In these cases, report that the effect was not reached within the tested range instead of forcing a number.

#### `Do not report numeric value (extrapolated)`

The fitted potency lies outside the tested concentration range.

#### `Numeric value shown; review fit`

The app could calculate a value, but something about the fit still needs human judgment.

#### `No fit available`

The model could not be fit for that group.

### `fit_reason`

This column gives the short reason for caution. Common examples:

- `Top far above data`
- `Bottom far below data`
- `50% not reached`
- `Half-max effect not reached`
- `Value outside tested range`
- `Flat or invalid fitted range`

### `reporting_note`

This column gives a longer export-friendly explanation.

### `r_squared`

Useful as one fit-quality clue, but it should not be the only criterion. A visually smooth curve can still be biologically implausible if the plateaus are unrealistic.

### `Model Comparison`

This tab is populated when `Compare all models first (no bootstrap)` is enabled.

The suggested model is chosen by prioritizing:

1. more groups with reportable numeric potency values
2. more successful fits
3. fewer groups needing review
4. higher median `R-squared`
5. fewer parameters when the earlier criteria are tied

This makes the recommendation practical rather than purely mathematical.

### `Data Preview`

This tab helps you audit what the app is actually using.

#### `Imported data`

Shows the raw uploaded table or the example dataset.

#### `Prepared summary`

Shows the summarized table used for grouped fitting. This is particularly useful when you are using:

- `Fit curve using = Group means`
- `Weighting = 1 / SD^2 from means`

#### `Other-plots summary`

Shows the summary table used by the `Other Plots` tab. This can include:

- summary statistics
- error-bar components
- annotation columns
- displayed label metadata

### Analysis summary popup

After a run, the app can show a summary popup when some groups need attention. It is useful for a quick scan of:

- how many groups are fully reportable
- how many did not reach the target
- how many are extrapolated
- how many need review
- the most common fit reasons

Use it as a warning system, not as the final interpretation.

## 9. Curve-plot customization and export

### `Curve plot quick controls`

#### `Plot title`

Use a short descriptive title, or uncheck `Show title` if you prefer a cleaner final figure.

#### `Plot preset`

Presets quickly coordinate style choices. If you want fine control afterward, switch settings manually.

#### `Use log10 concentration axis`

Recommended for many dose-response figures because it spreads low and high concentrations more naturally.

#### `Include zero-dose rows in fitting`

Useful when:

- your linear-concentration dataset includes control rows at `x = 0`
- the zero-dose baseline should help anchor the fitted top or bottom, similar to GraphPad's include-zero option

The app replaces concentration `0` with a small positive surrogate one log10 decade below the minimum positive concentration. On log-scale plots, those rows can influence the fit and appear at that surrogate position, but they still cannot be drawn at literal `x = 0`.

#### `Extend fitted curve toward zero-dose baseline`

Useful when:

- you did not measure a literal zero point
- you want the curve to visually continue toward the baseline

On a log axis, this is implemented as extra decades below the lowest positive dose.

#### `Extra log10 decades below the minimum dose`

Controls how far the extrapolated left tail is drawn on a log axis when curve extension is enabled.

#### `Show IC50 guide lines`

Adds visual guide lines to the potency estimate when appropriate. This now starts turned off by default so the main curve panel opens in a cleaner state.

#### `Show 50% reference line`

Useful in `IC50` mode or whenever the audience expects to see the 50% reference.

### `Plot styling options`

Important controls include:

- `X-axis title`
- `Y-axis title`
- `Plot style`
- `Plot grid lines`
- `Palette`
- `Legend position`
- `Legend content`
- `Show legend title`
- `Legend title`
- `Legend title size`
- `Legend numeric decimals`
- `Background`
- `Use different point shapes by group`
- `Show raw points`
- `Show SD error bars`
- `Facet by group`
- `Base font size`
- `Point size`
- `Curve line width`

### `Curve plot axis breaks and limits`

Use these fields when you need journal-style control over the axes.

Examples:

- `Custom x breaks`: `0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30`
- `Custom y breaks`: `0, 25, 50, 75, 100`
- `X-axis limits`: `0.01, 30`
- `Y-axis limits`: `0, 100`

### Export settings

These settings apply to both plot modules.

Recommended defaults for publication-oriented raster export:

- format: `PNG` or `TIFF`
- width: choose based on target figure layout
- height: choose based on target figure layout
- DPI: `600`

Use `PDF` or `SVG` when you want vector output.

## 10. Other Plots module

The `Other Plots` tab is designed for figures that do not require fitting a sigmoid curve.

Typical uses:

- grouped bar plots for peptides, compounds, or treatments
- boxplots for replicate distributions across groups
- line plots for time-course or concentration-course summaries

The module uses the same uploaded dataset as the curve-fitting workflow, but it has its own mapping and plot-specific controls.

### Column mapping

The `Other plot column mapping` section includes:

- `X column`
- `Y column`
- `Series / color column`
- `Facet column`
- `Optional annotation column`

### How to think about the mapping

The fastest way to choose a good mapping is to decide what each visual dimension should mean.

Typical grouped-bar example:

- `X column = peptide`
- `Y column = percent hemolysis`
- `Series / color column = concentration`
- `Facet column = None`
- `Optional annotation column = letters`, if you already have them

Typical time-course line example:

- `X column = time`
- `Y column = response`
- `Series / color column = treatment`

### Plot type: `Bar plot`

Use a bar plot when your goal is to compare summarized group values, usually means or medians, across conditions.

This is a good choice for:

- inhibitor summaries at several concentrations
- hemolysis screening figures
- grouped treatment comparisons

Relevant controls:

- `Summary value`
- `Error bars`
- `Overlay raw points`
- `Label mode`

#### `Summary value`

Choices:

- `Mean`
- `Median`

Use:

- `Mean` for most standard assay summaries
- `Median` when you want a more robust center against outliers

#### `Error bars`

Choices:

- `SEM`
- `SD`
- `95% CI`
- `IQR`
- `None`

Use:

- `SEM` when you want a mean-precision style summary
- `SD` when you want to show spread around the mean
- `95% CI` when you want interval-style uncertainty
- `IQR` when using median-centered summaries

#### `Overlay raw points`

Recommended in most cases, because it shows how many replicates you actually have and how variable they are.

### Plot type: `Boxplot`

Use a boxplot when you want to emphasize the distribution of raw replicate values within each group.

A boxplot makes sense when each point is an independent observation, for example:

- different donors
- different animals
- different biological experiments
- different independently prepared cultures

A boxplot is usually not a good choice when the points are only technical repeats from the same sample.

#### When boxplots are weak

Be careful with boxplots when:

- you only have `3` replicates per group
- the rows are already summarized means
- the observations are not independent biological units

In those cases:

- a bar plot with raw-point overlay is often clearer
- a point plot or line plot may also be more honest

#### Important limitation

Automatic significance letters are currently available only for bar plots. For boxplots, you can still use:

- `Label mode = Annotation column`
- `Letter / annotation size`

### Plot type: `Line plot`

Use a line plot when x has an order that matters, such as:

- time
- concentration
- dose sequence
- day

The line plot uses summarized values plus optional error bars and can also overlay raw points.

#### `Use log10 x-axis when the selected x column is numeric`

Use this when:

- the x variable spans orders of magnitude
- the x variable is numeric

Do not use it when x is categorical.

### Label modes

The `Label mode` control offers:

- `None`
- `Annotation column`
- `Auto significance letters (bar plot)`

#### `None`

No labels are drawn.

#### `Annotation column`

Use this when your dataset already contains labels such as:

- `a`
- `b`
- `ab`
- `ns`
- short comments

The app places the annotation text above the relevant bar, box, or line summary.

#### `Auto significance letters (bar plot)`

Use this when:

- you want the app to calculate compact-letter displays automatically
- you are working with raw replicate rows
- your comparison question is one-factor within a chosen scope

### Automatic significance letters in detail

When `Label mode = Auto significance letters (bar plot)`, the app activates these controls:

- `Letter test`
- `Compare bars`
- `Letter alpha`
- `Letter / annotation size`

#### `Letter test`

Choices:

- `ANOVA + Tukey HSD`
- `Kruskal-Wallis + pairwise Wilcoxon (Holm)`

Use `ANOVA + Tukey HSD` when:

- the groups are reasonably compatible with parametric assumptions
- you want the classic compact-letter approach after one-way ANOVA

Use `Kruskal-Wallis + pairwise Wilcoxon (Holm)` when:

- the data are more naturally handled with a nonparametric one-factor comparison
- you still want compact-letter output

#### `Compare bars`

Choices:

- `Across all bars in each facet`
- `Within each series in each facet`
- `Within each x group in each facet`

This setting is critical because it defines the statistical question.

##### `Within each x group in each facet`

This is the default and is usually the right choice for grouped bar plots such as `peptide x concentration`.

Example question:

- "Within peptide Y3W7, which concentrations differ from each other?"

Use this when:

- x is the biological item of interest
- series is concentration or condition
- you want letters within each x category

##### `Within each series in each facet`

Example question:

- "At concentration 25 uM, which peptides differ from each other?"

Use this when:

- series is the fixed condition
- x contains the groups you want to compare inside that condition

##### `Across all bars in each facet`

This pools all displayed bars in the facet into one lettering system.

Use this only when you truly want one global one-factor comparison across the whole displayed set. It is visually convenient, but the interpretation can become too broad for factorial designs.

#### `Letter alpha`

This is the significance threshold used when building the compact-letter display. The default `0.05` is a sensible starting point.

#### `Letter / annotation size`

Controls the size of:

- manual annotation labels
- automatic significance letters

### What automatic letters mean

Bars that share a letter are not significantly different under the selected method and scope. Bars with different letters are interpreted as different under that same method and scope.

Important caution:

- the meaning depends entirely on the selected comparison scope
- the letters are not a replacement for a full statistical methods section
- the app currently exports label metadata, but not a full pairwise p-value table

### Recommended usage for hemolysis-style figures

For a plot like `peptide x concentration -> percent hemolysis`:

- `Other plot type = Bar plot`
- `X column = peptide`
- `Y column = percent hemolysis`
- `Series / color column = concentration`
- `Label mode = Auto significance letters (bar plot)` or `Annotation column`
- `Compare bars = Within each x group in each facet`

This compares concentrations within each peptide, which is usually the intended question in that visual layout.

### When boxplot is more appropriate than bar plot

Choose `Boxplot` when:

- your goal is to show replicate spread or distribution
- each row is an independent biological observation
- you want the audience to see variability, quartiles, and outliers

Choose `Bar plot` when:

- your goal is to show a summarized value such as mean response
- the figure is mainly comparative and publication-styled
- you already have or want compact-letter annotations

### Facets

Use `Facet column` when you want separate small panels by:

- assay
- day
- cell line
- donor
- experiment batch

This helps avoid overcrowding one large panel.

### Other-plot summary export

The `Download other-plot summary CSV` button exports the summarized table behind the figure. This is useful for:

- checking the values that were actually plotted
- reusing the summary in another workflow
- keeping a record of annotation mode and label metadata

## 11. Statistical guidance and interpretation

### Biological replicates versus technical replicates

This distinction matters for both boxplots and automatic letters.

#### Independent biological replicates

Examples:

- different donors
- different animals
- separate cultures prepared independently
- separate experiments performed on different days as independent repeats

These are generally suitable for:

- boxplots
- bar-plot significance letters

#### Technical repeats

Examples:

- repeated reads of the same well
- triplicate pipettings from the same prepared sample
- repeated instrument measurements of the same biological unit

These are usually not enough by themselves for strong inferential claims. In those cases:

- raw-point overlays are still useful
- descriptive plots are fine
- statistical lettering may overstate evidence if interpreted as biological significance

### When the comparison question does and does not make sense

A plot can be visually attractive but statistically mismatched if the lettering does not correspond to the scientific question.

Examples:

- If your bars are grouped by compound and colored by concentration, comparing `within each x group` answers whether concentrations differ within a compound.
- Comparing `within each series` answers whether compounds differ at a fixed concentration.
- Comparing `across all bars` answers a broader question that may not match a factorial design.

Choose the scope based on the question you want the figure to answer, not just on which version looks familiar.

### When not to report a numeric IC50 or EC50

Do not report a numeric potency value when the app flags:

- `Do not report numeric value (target not reached)`
- `Do not report numeric value (extrapolated)`

In those cases, it is usually better to write something like:

- "IC50 not reached within the tested range"
- "EC50 estimated outside the tested concentration range"

### Why a nice-looking curve can still be unreliable

A smooth fitted line can still be misleading if:

- the top is far above the measured data
- the bottom is far below the measured data
- the target response is never actually crossed
- the fitted value lies outside the tested range

That is why the app shows `reporting_status`, `fit_reason`, and `reporting_note` instead of only showing a number.

## 12. Example workflows

### Workflow A. Standard inhibitor IC50 analysis

Use this when you have a standard inhibition dataset across several concentrations.

Recommended settings:

- `Response scaling = Raw values` or a suitable normalization mode
- `Fit curve using = Group means`
- `Response transform = Invert as 100 - response` if needed
- `Curve direction = Increasing` if the transformed biology increases with dose, otherwise `Auto-detect`
- `Model = 4PL`
- `Potency metric = IC50`
- `Potency uncertainty = None` for the first pass

Interpretation note:

- for mechanistic enzyme-inhibitor interpretation, remember that the fitted `IC50` is assay-dependent and is not automatically the same as `Ki` (Cheng and Prusoff, 1973)

Then:

1. run the analysis
2. inspect model comparison if enabled
3. confirm the fit quality
4. re-run with `95% CI` for final reporting

### Workflow B. EC50 analysis for activation-like data

Use this when the signal rises with increasing dose and the biologically relevant parameter is the half-max effect.

Recommended settings:

- `Potency metric = EC50`
- `Curve direction = Increasing` or `Auto-detect`
- `Model = 4PL` as a starting point

Remember that `EC50` is based on the fitted half-max effect between bottom and top, not a fixed response of `50`.

### Workflow C. Publication bar plot with letters

Use this for figures like grouped hemolysis or inhibitor comparison charts.

Recommended settings:

- `Other plot type = Bar plot`
- `X column = peptide or compound`
- `Y column = measured response`
- `Series / color column = concentration or treatment`
- `Summary value = Mean`
- `Error bars = SEM` or `SD`
- `Overlay raw points = TRUE`

Then choose either:

- `Label mode = Annotation column` if you already have letters
- `Label mode = Auto significance letters (bar plot)` if you want the app to calculate them

For many grouped bioassay figures:

- `Compare bars = Within each x group in each facet`

### Workflow D. Boxplot for independent experiment distributions

Use this only if your rows are actual independent observations.

Recommended settings:

- `Other plot type = Boxplot`
- `X column = treatment`
- `Y column = response`
- `Series / color column = None` or another grouping variable when needed
- `Overlay raw points = TRUE`

If you want statistical labels on a boxplot:

- calculate them elsewhere
- store them in a column
- use `Label mode = Annotation column`

## 13. Troubleshooting

### The app says there are not enough doses

Cause:

- one or more groups have fewer than `4` distinct positive doses

Fix:

- check the mapped group column
- confirm the dose column is numeric
- confirm doses were not accidentally treated as text
- add more positive dose levels if the dataset is too sparse

### The fit direction looks wrong

Fix:

- check whether the response should be transformed first
- try `Invert as 100 - response` for inhibition-style data
- try forcing `Increasing` or `Decreasing` if the automatic choice is not biologically appropriate

### The app reports `50% not reached`

Meaning:

- in `IC50` mode, the observed response never crosses 50 in the fitted direction

What to do:

- expand the tested concentration range
- do not report a numeric IC50

### The app reports `Half-max effect not reached`

Meaning:

- in `EC50` mode, the data do not span the fitted half-max effect level sufficiently

What to do:

- improve the dynamic range
- expand the concentration range
- do not force a numeric value

### The fit looks smooth but the table says review

This usually means the curve shape is mathematically smooth but biologically suspicious, for example:

- `Top far above data`
- `Bottom far below data`
- `Value outside tested range`

Try:

- a simpler model
- a broader dose range
- re-checking normalization or transformation

### My zero-dose control is missing from the curve plot

If the x-axis is log10:

- zero cannot be plotted directly

If you need to see zero-dose rows:

- use a linear axis
- check the `Data Preview` tables
- use the `Other Plots` tab if the goal is a descriptive figure rather than a curve fit

If `Include zero-dose rows in fitting` is enabled, the zero-dose rows can affect the fitted parameters and appear at the surrogate log-scale position, even though the plot still cannot draw a point at literal `x = 0`.

### Weighting is not staying enabled

Cause:

- `Weighting = 1 / SD^2 from means` only works with `Fit curve using = Group means`

If you switch to `All observations`, the app resets weighting to `None`.

### The bar-plot letters do not answer my intended question

Cause:

- the `Compare bars` scope is set to the wrong comparison frame

Fix:

- use `Within each x group in each facet` to compare series within each x category
- use `Within each series in each facet` to compare x categories within each series
- use `Across all bars in each facet` only when a global one-factor comparison is truly intended

### The boxplot does not look informative

Likely reasons:

- too few replicates
- replicates are technical rather than biological
- the file contains summarized means instead of raw data

Try:

- keeping `Overlay raw points = TRUE`
- switching to `Bar plot`
- using a line plot if x has an order

## 14. Good-practice checklist

Before final reporting, check the following:

- the mapped dose and response columns are correct
- the group column really reflects the intended biological grouping
- the response transformation matches the biology
- the chosen model is not producing unrealistic plateaus
- `reporting_status` supports numeric reporting when you plan to quote a potency value
- bootstrap uncertainty is enabled only after the fit looks reasonable
- zero-dose handling on the figure matches the message you want the plot to convey
- boxplots are used only for raw independent observations
- automatic letters are used only when the comparison scope matches the scientific question
- figure export size and DPI match the target journal or presentation format

## 15. Contact and license

**Dr. Ivan Sanchis**  
Institutional email: sanchisivan@fbcb.unl.edu.ar  
Personal email: sanchisivan@gmail.com

Laboratory of Bioactive Peptides (LPB)  
Faculty of Biochemistry and Biological Sciences (FBCB)  
National University of the Littoral (UNL)  
Santa Fe, Argentina

Repository: [github.com/sanchisivan/ic50-studio](https://github.com/sanchisivan/ic50-studio)

IC50 Studio is distributed under the MIT License. See [LICENSE](LICENSE) for the full license text.

## 16. References

Carducci M, Massai L, Lari E, Semplici B, Aruta MG, De Simone D, Piu P, Montomoli E, Berlanda Scorza F, Grappi S, Iturriza-Gómara M, Canals R, Rondini S, Rossi O. Qualification of an enzyme-linked immunosorbent assay for quantification of anti-Vi IgG in human sera. Front Immunol. 2024;15:1466869. [PubMed](https://pubmed.ncbi.nlm.nih.gov/39478859/)

Cheng Y, Prusoff WH. Relationship between the inhibition constant (Ki) and the concentration of inhibitor which causes 50 per cent inhibition (I50) of an enzymatic reaction. Biochem Pharmacol. 1973;22(23):3099-3108. [DOI](https://doi.org/10.1016/0006-2952(73)90196-2)

Cumberland WG, Xu X, Breitbart E, Parvin CA. Nonlinear calibration model choice between the four and five-parameter logistic models. J Biopharm Stat. 2015;25(5):972-983. [PubMed](https://pubmed.ncbi.nlm.nih.gov/24918306/)

Gottschalk PG, Dunn JR. The five-parameter logistic: a characterization and comparison with the four-parameter logistic. Anal Biochem. 2005;343(1):54-65. [PubMed](https://pubmed.ncbi.nlm.nih.gov/15953581/)

Garg K, Villavicencio-Aguilar F, Solano-Rivera F, Gilbert L. Analytical Validation of a Direct Competitive ELISA for Multiple Mycotoxin Detection in Human Serum. Toxins (Basel). 2022;14(11):727. [PubMed](https://pubmed.ncbi.nlm.nih.gov/36355977/)

Goutelle S, Maurin M, Rougier F, Barbaut X, Bourguignon L, Ducher M, Maire P. The Hill equation: a review of its capabilities in pharmacological modelling. Fundam Clin Pharmacol. 2008;22(6):633-648. [DOI](https://doi.org/10.1111/j.1472-8206.2008.00633.x)

Hill AV. The possible effects of the aggregation of the molecules of haemoglobin on its dissociation curves. J Physiol. 1910;40:iv-vii.

Hyun SW, Wong WK, Yang Y. Optimal designs for asymmetric sigmoidal response curves in bioassays and immunoassays. Stat Methods Med Res. 2020;29(2):421-436. [Publisher](https://journals.sagepub.com/doi/full/10.1177/0962280219832631)

Jarantow SW, Pisors ED, Chiu ML. Introduction to the Use of Linear and Nonlinear Regression Analysis in Quantitative Biological Assays. Curr Protoc. 2023;3(6):e801. [PubMed](https://pubmed.ncbi.nlm.nih.gov/37358238/)

Kadomoto S, Shelley G, Mizokami A, Keller ET. Development of Drug-resistant Cell Lines for Experimental Procedures. J Vis Exp. 2025;(222):e68957. [PubMed](https://pubmed.ncbi.nlm.nih.gov/40889269/)

Sebaugh JL. Guidelines for accurate EC50/IC50 estimation. Pharm Stat. 2011;10(2):128-134. [PubMed](https://pubmed.ncbi.nlm.nih.gov/22328315/)

Vernet R, Charrier E, Grogg J, Mach N. A Quantitative ELISA Protocol for Detection of Specific Human IgG against the SARS-CoV-2 Spike Protein. Vaccines (Basel). 2021;9(7):770. [PubMed](https://pubmed.ncbi.nlm.nih.gov/34358186/)

Weiss JN. The Hill equation revisited: uses and misuses. FASEB J. 1997;11(11):835-841. [PubMed](https://pubmed.ncbi.nlm.nih.gov/9285481/)

Xiang Y, Donley J, Seletskaia E, Shingare S, Kamerud J, Gorovits B. A Simple Approach to Determine a Curve Fitting Model with a Correct Weighting Function for Calibration Curves in Quantitative Ligand Binding Assays. AAPS J. 2018;20(3):45. [PubMed](https://pubmed.ncbi.nlm.nih.gov/29536273/)
