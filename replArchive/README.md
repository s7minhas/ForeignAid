## Replication instructions for the paper

The replication archive is organized into two directories (note that these files are also available on Github at [https://github/s7minhas/foreignAid/](https://github.com/s7minhas/foreignAid)):

- **main**: contains the data files and scripts necessary to reproduce the main results in the paper
- **appendix**: contains the data files and scripts necessary to reproduce the results in the appendix

Replicating the figures and tables in the **main** text will take only a few minutes on a standard laptop if the provided `.rda` files are used.  

#### Setup information

All of the analyses reported in the manuscript and the appendix are run with the following specification (further information on packages used in the analysis is included at the end of the README): 

```
R version 3.6.0 (2019-04-26)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Mojave 10.14.4

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

loaded via a namespace (and not attached):
[1] compiler_3.6.0
```

#### Running the scripts

Scripts should be run in the following order (each script assumes that the working directory matches the directory in which the script is stored, if you need to modify change the paths specified in **setup.R**): 

- **figure1.R**: Descriptive chart of aid flows between the US and Iran. Resulting figure stored in `main/floats/figure1.pdf`.
- **figure2.R**: Generates visualization of estimated latent space relations across the ally, IGO, and UN voting variables. Resulting figures stored in `main/floats/figure2_[ally, igo, un].jpg`.
- **figure3.R**: Runs main set of models discussed in the paper and stores model object in `main/mods/mainMod.rda`. Also generates a summary of the results in a coefficient plot, which is saved to `main/floats/figure3.pdf`.
- **figure4.R**: Conducts and visualizes a substantive effects analysis of the interaction between strategic distance and number of disasters on aid flows. Resulting figure is stored in `main/floats/figure4.pdf`
- **figure5_7.R**: Runs models on aid flows using varying lags of strategic distance and number of disasters on aid flows and stores the model objects in `main/mods/mainMod_lag3.rda` and `main/mods/mainMod_lag5.rda`. A separate visualization is produced for each dependent variable. Results are stored in `main/floats/figure[5, 6, 7].pdf`.

We have also included all the analysis necessary to generate the results in the appendix. The appendix are organized in a similar format as those in the `main` directory. 

#### R package build notes

Below we provide the version of each of the libraries that our project relies on (each library was built using R 3.6.0). 

|                 |                |                   |                |
|:----------------|:---------------|:------------------|:---------------|
|cshapes: 0.6     |devtools: 2.0.2 |doParallel: 1.0.14 |dplyr: 0.8.0.1  |
|foreach: 1.4.4   |ggplot2: 3.1.1  |grid: 3.6.0        |gridExtra: 2.3  |
|latex2exp: 0.4.0 |lme4: 1.1-21    |MASS: 7.3-51.4     |reshape2: 1.4.3 |
|xtable: 1.8-4 |    |     | |

If you find any errors or have any further questions, please address them to me via email at minhassh@msu.edu.