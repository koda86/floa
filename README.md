# floa - **f**unctional **l**imits **o**f **a**greement

This repository provides code to calculate continuous prediction intervals (prediction bands) using several methods and supplements the following publication:

Koska, D., Oriwol, D., Maiwald, C. (2023) Comparison of statistical models for characterizing continuous differences between two biomechanical measurement systems. Journal of Biomechanics. DOI: https://doi.org/10.1016/j.jbiomech.2023.111506

For a detailled description please see the documentation (floa_doc.md) in floa/man.

## Availability as R package
The functions in this repository are now available as a readily installable R package under https://github.com/koda86/FunBootBand. Calculation methods are basically unchanged, but the scripts have been combined into a single function (`band()`) in the package. See the 'FunBootBand' repository for more details and instructions.

### Error note
In the first release of `floa`, the calculation of confidence bands (as opposed) to prediction bands was not correct. This has been fixed in the new [FunBootBand](https://github.com/koda86/FunBootBand) package. The code in this, repo, however, will not be changed.

### Citation
If you make use of any of the code or datasets, please cite the paper and/or the Github repository.

### Issues
Please report software bugs or other problems by searching existing issues or creating a new issue [here](https://github.com/koda86/floa/issues).

### Contributing
If you find issues or bugs feel free to send me an E-Mail (daniel.koska@hsw.tu-chemnitz.de). If you want to fix them yourself, please do, and submit a pull request so it can be reviewed and merged.

### Licence
BSD-2-Clause License
