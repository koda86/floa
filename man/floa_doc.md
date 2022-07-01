# Documentation Github repo koda86/floa

### How to use the directory

There are two ways to use the code in this repository. You can either:

**1. Fork and clone the repository**

- If you haven't done that before, these are good starting points:
  - https://github.com/rstats-tln/fork-and-clone-repo
  - https://docs.github.com/en/get-started/quickstart/fork-a-repo

- Another excellent place for learning git and GitHub is http://happygitwithr.com.

or

**2. Download the repository**

- Download the repository as a zip file and work locally

---

If you aim to reproduce the results of the paper, please open the parent script 'main.R' and read the instructions in the section **Main script** below.

If you want run subscripts only (e.g. to calculate prediction bands from your own data using one of the three methods), make sure to organize your data in long data format with five columns named:

1. device (character, has to be named "ONE" and "TWO")
2. subjectID (integer, 1 to number of subjects)
3. strideID (integer, 1 to number strides, should be balanced between measurement systems)
4. value (numeric, actual measurement data)
5. frame (integer, e.g. 0 to 100, curves should have the same length)

### Main script

In 'main.R', all subscripts are loaded and called. To run the script, a number of folder directories and function parameters have to be pre-specified:

**1. Working directories**

- dir.script: Path to directory in which the R scripts are stored.
- dir.data : Path to directory in which the data set is stored.

**2. Data set**

Make sure that a data set with the required (long) format is assigned to a variable named 'data'.

**3. Function parameters:**

- n.boot: The number of bootstrap iterations (default = 400)

- (n.rep: The number of repeated calculations in the uncertainty estimation (default = 100))

In 'main.R', it is possible to choose between four predefined data sets ("smooth_realistic", "non_gaussian", "shift", "imu_MC"). Of cause, you may also your own data. Just make sure that a data set with the required (long) format is assigned to a variable named 'data'.

#### Example data sets

Data sets are generated in 'simulate_data.R' and stored in .../R/examples.

The provided data consists of four (3 synthetic and 1 real-world) data sets given as both .txt (ASCII) and .rds (binary) files:

- 'smooth_realistic': Simulated curves with Gaussian error model

- 'non_gaussian'    : Simulated curves with non-Gaussian errors and heteroskedasticity

- 'shift.rds'       : Simulated curves curves with Gaussian error model and phase shift in x-axis direction

- 'imu_mc'          : Real-world hip joint angle curves


### Functions

In 'main.R', the following functions are called:

- plot_loa(): Returns a plot of (differently colored) prediction bands vs. the original difference curves.

- coverage_loocv(): Leave-one (curve) out method to estimate the coverage probability

- estimate_uncertainty_loa(): Estimates the uncertainty in different methods across 'n.rep' repeated calculations

Within these functions, other functions are nested. If your aim is to calculate prediction bands in one of the methods from the paper (POINT, ROISLIEN, BOOT), use one of the the follwing three scripts: 

- floa_point.R(): Pointwise continuous Limits of Agreement according to Bland & Altman (1999) (POINT)

- floa_roislien.R(): Functional limits of agreement according to Roislien et al. (2012) (ROISLIEN)

- floa_boot.R(): Implementation of the method described in Lenhoff et al. (1999) (BOOT)
  - Requires other function arguments besides 'data':
    - test

Other nested functions are:

- pick_subwise_curves(): Select a single random stride from every subject in data

- points_within_limits.R(): Calculate coverage (points of a single curve within the band limits)

- coverage_curves.R(): Calculates the proportion of bands that contain a specified proportion of curve points


<!---
### Flowchart

```{=html}
<div id="htmlwidget-f84af788624b61ab0729" style="width:672px;height:480px;" class="grViz html-widget"></div>
<script type="application/json" data-for="htmlwidget-f84af788624b61ab0729">{"x":{"diagram":"digraph flowchart {\n\n      # node definitions with substituted label text\n      node [fontname = Helvetica, shape = rectangle]\n      \n      tab1 [label = \"Time normalize curves of IMU - MC\"]\n      tab2 [label = \"Create difference curves IMU - MC\"]\n      tab3 [label = \"Convert difference curves to function data objects using Fourier series\"]\n      tab4 [label = \"floa_rcb \"]\n      tab5 [label = \"FLOAboot_2SD \"]\n      tab6 [label = \"floa_point\"]\n      tab7 [label = \"First stage:\n draw_clusters\n All strides from 11 subjects (randomly permuted)\n are drawn with replacement\"]\n      tab8 [label = \"Functional mean for each cluster\"]\n      tab9 [label = \"Second stage:\n Repeat the first stage 1000 times\"]\n      tab10 [label = \"Quantile\"]\n\n      # edge definitions with the node IDs\n      tab1 -> tab2\n      tab2 -> tab3\n      tab3 -> tab4\n      tab3 -> tab5\n      tab3 -> tab6\n      tab4 -> tab7\n      tab7 -> tab8\n      tab8 -> tab9\n      tab9 -> tab10\n      }","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
```
--->
