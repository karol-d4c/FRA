# Fractional Response Analysis (FRA) - R package
R- package FRA is designed to performs fractional response analysi of single-cell responses as presented in the manuscript Nienałtowski et al. "Fractional response analysis reveals logarithmic cytokine responses in cellular populations" Submitted (2020).
 A comprehensive documentation is available in directory [`XXXXXXXX.pdf`](https://github.com/sysbiosig/XXXXXXX).
 ## Setup
 ## Requirements - Hardware
  + A 32 or 64 bit processor (recommended: 64bit)
  + 1GHz processor (recommended: multicore for a comprehensive analysis)
  + 2GB MB RAM (recommended: 4GB+, depends on the size of experimental data)

## Requirements - Software
The main software requirement is the installation of the R environment (version: >= 3.2), which can be downloaded from [R project website](https://www.r-project.org) and is distributed for all common operating systems.  We tested the package in  R environment installed on Windows 7, 10; Mac OS X 10.11 - 10.13 and Ubuntu 18.04 with no significant differences in the performance. The use of a dedicated Integrated development environment (IDE), e.g. [RStudio](https://www.rstudio.com) is recommended. 

Apart from a base installation of R, SLEMI requires the following R packages:

1. for installation 

  + devtools
  
2. for estimation
  
  + nnet
  + doParallel (if parallel computation are needed)
  
3. for visualisation

  + ggplot2
  + ggthemes
  + grDevices
  + viridis
  
  
4. for data handling

  + data.table
  + reshape2
  + dplyr
  + foreach

Each of the above packages can be installed by executing 
  
> `install.packages("name_of_a_package")`

in the R console. 

Importantly, during installation availability of the above packages will be verified and missing packages will be automatically installed.
 ### Installation

The package can be directly installed from [GitHub](https://github.com/sysbiosig/FRA).  For installation, open RStudio (or base R) and run following commands in the R console

> `# install.packages("devtools") # run if not installed`
 
> `library(devtools)`
 
> `install_github("sysbiosig/FRA")`
 
All packages that are required will be installed or updated automatically.

 
 ## Basic usage
 
 The FRA package provides their functionalities with three main functions:

1. `FRA()`-  fractional response analysis  performed for heterogeneous, multivariate, and dynamic measurements. Function computes: (i) the fractional response curve that quantifies fractions of cells that exhibit different responses to a change in dose, or any other experimental conditionand and (ii) the cell-to-cell heterogeneity, i.e.fraction of cells exposed to one dose that exhibits responses in the range characteristic for other doses.

2. `plotHeterogeneityPieCharts()` - visualises the cell-to-cell heterogeneity structure using table of pie charts. Each pie chart describes the fraction of cells exposed to one dose (rows)  that expibits responses typical for either of the doses (columns).

3. `plotFRC()`  visualises the fractional response curve (FRC) and the cell-to-cell heterogenity. FRC is represented as a line, whereas heterogeneity is represented as  colour band.

Morevoer, package contains examplary datasets, that were used in the publication:

1. `data.fra.cytof`

2. `data.fra.ps1`

3. `data.fra.ps3`

4. `data.fra.nfkb`
 

 ### Preaparing data
 
 The function `FRA()` takes `data` in the form of the object `data.frame` with a specifc structure of rows and columns. 
Responses $y^i_j$ are assumed to be measured for a finite set of stimuli levels $x_1,x_2,\ldots,x_m$. The responses $y^i_j$ can be multidimensional.

### 
## Fractional response analysis
In order to perform fractional response analysis of single-cell data call 
> model <-FRA(
  data,
  signal = "signal",
  response = "response",
  sample = "sample",
  bootstrap.number = 0,
  bootstrap.sample_size = 1000,
  parallel_cores = 1,
  lr_maxit = 1000,
  MaxNWts = 5000,
  ...
)

The required arguments are:

* `data` - a data.frame or data.table object in a wide format that describe response (might be multidimmensional) of the samples to the signal (now only one dimmensional); data.frame data consists columns of names defined by sample, signal (optional), and response; each row represents a response of one sample to the input signal; column signal define the input signal; columns response define the multidimmensional (optional) response to the input signal; column sample specify identifaction of sample; if sample is not defined then sample is identified by row number; 
* `signal` - character, specify name of the column that represents the input signal; 	
* `response` vector of characters, that specify names of the columns that represents the output response;
* `sample`	- character (optional), specify name of the column that consists identifiaction of sample;
* `parallel_cores` - specify number of cores used for computations, `default = 1`
* `bootstrap.number` (`default = 1`) - numeric, `bootstrap.number >= 1`, specify nymber of bootstrap samples used for estimation SCRC and cell-to-cell heterogeneity. It is crucial to choose this value carefully, as it induce estimator accuracy. The proper value depends on data dimmensions and density distribution. The practice indicates that the higher number of bootstrap samples are required to obtain satisfying level of the accuracy of the cell-to-cell heterogeneity estimator. The `bootstrap.number = 1` denotes that one bootstrap sampling is performed to guarantee equipotence between number of cells for each dose, that is assumed in method;
*  `bootstrap.sample_size` - numeric, size of the bootstrap sample;
* `lr_maxit` (`default = 1000`) - a maximum number of iterations of fitting step of logistic regression algorithm in `nnet` function. If a warning regarding lack of convergence of logistic model occurs, should be set to a larger value (possible if data is more complex or of a very high dimension); 
* `MaxNWts` (`default = 5000`) - a maximum number of parameters in logistic regression model. A limit is set to prevent accidental over-loading the memory. It should be set to a larger value in case of exceptionally high dimension of the output data or very high number of input values. In principle, logistic model requires fitting $(m-1)\cdot(d+1)$ parameters, where $m$ is the number of unique input values and $d$ is the dimension of the output.

The function returns the `FRAModel` object that contains among others
* `frc` - a `data.frame` that describe fractional response curve; contains two columns `dose` and `frc`
* `heterogeneity` - a `data.frame` that describes cell-to-cell heterogeneity, i.e., fraction of cells exposed to one dose (rows) that exhibits responses in the range characteristic for other doses (columns).

 ### Run
 In order to estimate channel capacity, using basic logistic regression model, call
> `capacity_logreg_main(dataRaw, signal, response, output_path)`
where: 
* `dataRaw` is a data.frame with experimental data as described above
* `signal` is a character indicating the name of column in `dataRaw` with the input (X)
* `response` is a character vector indicating names of columns in `dataRaw` with output (Y) variables
* `output_path` is a character with the directory, to which results of the estimation should be saved
 ### Results
 The function `capacity_logreg_main` returns a list, whose main elements are
 * cc - channel capacity estimate (in bits)
* p_opt - numeric bector with the optimal input distribution
* model - `nnet` object describing fitted logistic regression model
 For convenience of further analysis, this list is saved in `output_path` directory in a file `output.rds`. In addition to that, a set of exploratory graphs are created to visualise obtained estimates.
 ## Examples
 Additional examples of using package with some background on information theory is given in [`paper/TestingProcedures.pdf`](https://github.com/sysbiosig/SLEMI/blob/master/paper/TestingProcedures.pdf) and implemented in script [`paper/testing_procedures.R`](https://github.com/sysbiosig/SLEMI/blob/master/paper/testing_procedures.R). Codes used in publication are accessible from [`paper/paper_MP.R`](https://github.com/sysbiosig/SLEMI/blob/master/paper/paper_MP.R) and [`paper/paper_SI.R`](https://github.com/sysbiosig/SLEMI/blob/master/paper/paper_SI.R) respectively.
 ### Datasets
 In the manuscript describing methodological aspects of our algorithm we present the analysis of information transmission in NfkB pathway upn the stimulation of TNF-$\alpha$. Experimental data from this experiment in the form of single-cell time series are attached to the package as a data.frame object and can be accessed using `data_nfkb` variable.
 Each row of `data_nfkb` represents a single observation of a cell. Column 'signal' indicates the level of TNF-$\alpha$ stimulation for a given cell, while columns 'response_T', gives the normalised ratio of nuclear and cytoplasmic transcription factor as described in Supplementary Methods of the corresponding publication. 
 ## Other functionalities
 ### Additional paramters
 Apart from required arguments, the function `capacity_logreg_main` has also other parameters than can be used to tune the activity of the algorithm. These are
 * `model_out` (`default=TRUE`) - logical, specify if `nnet` model object should be saved into output file
* `graphs` (`default=TRUE`) - logical, controls creating diagnostic plots in the output directory.
* `plot_width` (`default = 6`) - numeric, the basic width of created plots 
* `plot_height` (`default = 4`) - numeric, the basic height of created plots
* `scale` (`default = TRUE`) - logical, value indicating if the columns of `dataRaw` are to be centered and scaled, what is usually recommended for the purpose of stability of numerical computations. From a purely theoretical perspective, such transformation does not influence the value of channel capacity.
* `lr_maxit` (`default = 1000`) - (argumnet of `nnet` package) a maximum number of iterations of optimisation step in logistic regression algorithm. Set to higher value if your data is more complex or of high dimension.
* `MaxNWts` (`default = 5000`) - (argumnet of `nnet` package) a maximum number of paramters in logistic regression model. Set to higher value if you data has many dimensions or input has many states.
 ### Diagnostic procedures
 We implemented two diagnostic procedures to control the performance of channel capacity estimation and to measure uncertainity due to finite sample size and model over-fitting. These include:
 1. Bootstrap test - capacity is re-calculated using $x$% of data, sampled from original dataset without replacement. After repeating procedure $n$ times, its standard deviation can be treated as an error of original estimate.
2. Over-fitting test - original data is divided into Training and Testing datasets. Then, logistic regression is estimated using $x$% of data (training dataset) and integrals of channel capacity are calculated via Monte Carlo using remaining $(1-x)$% of data (testing dataset). It is repeated $n$ times.
 In order to use those procedures, user must provide additional arguments to function `logreg_capacity_main()`, i.e.
 * testing (default=FALSE) - a logical value that turn on/off testing mode,
* TestingSeed (default= 1234) - the seed for the random number generator used to sample original dataset,
* testing_cores (default= 4) - a number of cores to use (via `doParallel` package) in parallel computing, 
* boot_num (default= 40) - a number of repetitions of the bootstrap , 
* boot_prob (default= 0.8) - a fraction of initial observations to  use in the bootstrap, 
* traintest_num (default= 40) - a number of repetitions of the overfitting test,
* partition_trainfrac (default= 0.6) - a fraction of initial observations to use as a training dataset in the overfitting test
 ## Support
 Please mail t.jetka at gmail.com in case of any bugs, problems and questions regarding package or inquiries regarding information theory.
 ## Reference
 Please cite
> Jetka T, Nienałtowski K, Winarski T, Błoński S, Komorowski M (2019) Information-theoretic analysis of multivariate single-cell signaling responses. PLOS Computational Biology 15(7): e1007132. https://doi.org/10.1371/journal.pcbi.1007132
 ## Licence
 SLEMI is released under the GNU licence and is freely available. A comprehensive documentation is available in directory [`vignette/SLEMI_vignette.pdf`](https://github.com/sysbiosig/SLEMI/blob/master/vignette/SLEMI_vignette.pdf).
