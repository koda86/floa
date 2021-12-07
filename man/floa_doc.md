# Documentation Github repo koda86/floa

https://github.com/koda86/floa

**Author:** Daniel Koska

This repository accompanies this publication: DOI

Please cite: Koska, D., Oriwol, D. & Maiwald, C. (2022) Methodische Aspekte der Konstruktion von Pradiktionsbändern in Methodenvergleiche.


The paper presents for three methods for constructing continuous prediction intervals (bands):

- POINT: Punktweise Limits of Agreement (Bland & Altman, 1999, 2007)

- ROISLIEN: Limits of Agreement according to Røislien et al. (2012)

- BOOT: Bootstrapped functional prediction bands (Lenhoff et al., 1999; Olshen, Biden, Wyatt, & Sutherland, 1989; Sutherland et al., 1988)


### Structure

#### Data sets

Data sets are generated in 'script examples.R' and stored in .../R/examples

Contains 4 data sets (3 synthetic and 1 real-world) given as both .txt (ASCII) or .rds (binary) files:

- 'smooth_realistic': Simulated curves with Gaussian error model

- 'non_gaussian': Simulated curves with non-Gaussian errors and heteroskedasticity

- 'shift.rds': Simulated curves curves with Gaussian error model and phase shift in x-axis direction

- 'imu_mc': Real-world hip joint angle curves

Each file contains long data format with 6 columns: row numer, "device", "subjectID", "strideID", "value", "frame".


### Flowchart


```{=html}
<div id="htmlwidget-f84af788624b61ab0729" style="width:672px;height:480px;" class="grViz html-widget"></div>
<script type="application/json" data-for="htmlwidget-f84af788624b61ab0729">{"x":{"diagram":"digraph flowchart {\n\n      # node definitions with substituted label text\n      node [fontname = Helvetica, shape = rectangle]\n      \n      tab1 [label = \"Time normalize curves of IMU - MC\"]\n      tab2 [label = \"Create difference curves IMU - MC\"]\n      tab3 [label = \"Convert difference curves to function data objects using Fourier series\"]\n      tab4 [label = \"floa_rcb \"]\n      tab5 [label = \"FLOAboot_2SD \"]\n      tab6 [label = \"floa_point\"]\n      tab7 [label = \"First stage:\n draw_clusters\n All strides from 11 subjects (randomly permuted)\n are drawn with replacement\"]\n      tab8 [label = \"Functional mean for each cluster\"]\n      tab9 [label = \"Second stage:\n Repeat the first stage 1000 times\"]\n      tab10 [label = \"Quantile\"]\n\n      # edge definitions with the node IDs\n      tab1 -> tab2\n      tab2 -> tab3\n      tab3 -> tab4\n      tab3 -> tab5\n      tab3 -> tab6\n      tab4 -> tab7\n      tab7 -> tab8\n      tab8 -> tab9\n      tab9 -> tab10\n      }","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
```
