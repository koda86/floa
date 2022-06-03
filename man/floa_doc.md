# Documentation Github repo koda86/floa

https://github.com/koda86/floa

This repository accompanies this publication: DOI


The paper compares three methods for constructing continuous prediction intervals:

- POINT: Pointwise Limits of Agreement by Bland & Altman (1999, 2007)

- ROISLIEN:Pointwise Limits of Agreement by Røislien et al. (2012)

- BOOT: Bootstrapped functional prediction bands (Lenhoff et al., 1999; Olshen, Biden, Wyatt, & Sutherland, 1989; Sutherland et al., 1988)



### Main script

The parent script is 'main.R'. Here, all subscripts are loaded and called. To run 'main.R', a number of folder directories and function parameters have to be pre-specified:

**1. Working directories**

- dir.script: Path to directory in which the R scripts are stored.
- dir.data  : Path to directory in which the data set is stored.

**2. Data set**

The precast sample data sets are provided as long data formats. When using own data sets, please make sure to provide the same data format.

For the scripts to run out of the box, data objects need to have five columns:

1. device (character, has to be named "ONE" and "TWO")
2. subjectID (integer, 1 to number of subjects)
3. strideID (integer, 1 to number strides, should be balanced between measurement systems)
4. value (numeric, actual measurement data)
5. frame (integer, e.g. 0 to 100, curves should have the same length)

**3. Function parameters:**

- n.boot: The number of bootstrap iterations (default = 400)
(- n.rep: The number of repeated calculations in the uncertainty estimation (default = 100))


#### Example data sets

Data sets are generated in 'simulate_data.R' and stored in .../R/examples.

The provided data consists of four (3 synthetic and 1 real-world) data sets given as both .txt (ASCII) and .rds (binary) files:

- 'smooth_realistic': Simulated curves with Gaussian error model

- 'non_gaussian'    : Simulated curves with non-Gaussian errors and heteroskedasticity

- 'shift.rds'       : Simulated curves curves with Gaussian error model and phase shift in x-axis direction

- 'imu_mc'          : Real-world hip joint angle curves


### Functions in main()

If you are interested in reproducing the results in the 

In main.R, the following functions are called:

- plot_loa(): Returns a plot of (differently colored) prediction bands vs. the original difference curves.

- coverage_loocv(): Leave-one (curve) out method to estimate the coverage probability

- estimate_uncertainty_loa(): Estimates the uncertainty in different methods across 'n.rep' repeated calculations

<!---
### Flowchart

```{=html}
<div id="htmlwidget-f84af788624b61ab0729" style="width:672px;height:480px;" class="grViz html-widget"></div>
<script type="application/json" data-for="htmlwidget-f84af788624b61ab0729">{"x":{"diagram":"digraph flowchart {\n\n      # node definitions with substituted label text\n      node [fontname = Helvetica, shape = rectangle]\n      \n      tab1 [label = \"Time normalize curves of IMU - MC\"]\n      tab2 [label = \"Create difference curves IMU - MC\"]\n      tab3 [label = \"Convert difference curves to function data objects using Fourier series\"]\n      tab4 [label = \"floa_rcb \"]\n      tab5 [label = \"FLOAboot_2SD \"]\n      tab6 [label = \"floa_point\"]\n      tab7 [label = \"First stage:\n draw_clusters\n All strides from 11 subjects (randomly permuted)\n are drawn with replacement\"]\n      tab8 [label = \"Functional mean for each cluster\"]\n      tab9 [label = \"Second stage:\n Repeat the first stage 1000 times\"]\n      tab10 [label = \"Quantile\"]\n\n      # edge definitions with the node IDs\n      tab1 -> tab2\n      tab2 -> tab3\n      tab3 -> tab4\n      tab3 -> tab5\n      tab3 -> tab6\n      tab4 -> tab7\n      tab7 -> tab8\n      tab8 -> tab9\n      tab9 -> tab10\n      }","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
```
--->
