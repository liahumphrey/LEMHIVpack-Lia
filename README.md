
<!-- README.md is generated from README.Rmd. Please edit that file -->

LEMHIVpack
==========

<!-- <img src='man/figures/logo.jpg' align="right" height="139" /> -->

[`LEMHIVpack`](https://github.com/HERU-LEM/LEMHIVpack) package and
github repository contains all model functions, code modules and input
data to run our dynamic compartmental simulation model for the localized
HIV microepidemics in Atlanta, Baltimore, Los Angeles, Miami, NYC,
Seattle. Please see this [slide
deck](https://drive.google.com/file/d/1nlkr-qv3nzUvypzGiQ9TD-h6rk-KsI4f/view),
the *Articles* section of this site, as well as the references below for
more details.

Preliminaries
=============

-   Install
    [RStudio](https://www.rstudio.com/products/rstudio/download/)
-   Install `devtools`

<!-- -->

    install.packages("devtools")

-   [Clone github
    repository](https://docs.github.com/en/enterprise/2.13/user/articles/cloning-a-repository)
    -   Cloning [this](https://github.com/HERU-LEM/LEMHIVpack) github
        repository will give users access to all necessary code and data
        to run the model and conduct the analysis.

Usage and installation
======================

1.  Open R Studio and load project file `LEMHIVpack.Rproj` from the main
    folder of the LEMHIVpack github repository.

2.  Download and install LEMHIVpack package from
    [GitHub](https://github.com) using:

<!-- -->

    devtools::install_github("HERU-LEM/LEMHIVpack")

R code modules
==============

The remaining R scripts to run the model and conduct the analysis are
organized into three sections:

1.  [Setup](https://github.com/HERU-LEM/LEMHIVpack/tree/master/01_Setup)
    contains R scripts for core modules. Users can modify these files as
    necessary (e.g. adjusting analysis time-horizon, intervention
    sustainment period, discount rate etc.), but do not need to run
    these scripts prior to running the model, as they will be called by
    modules in the next two steps.

2.  [Run
    model](https://github.com/HERU-LEM/LEMHIVpack/tree/master/02_Run_model)
    contains R scripts to run the model and produce raw outputs (costs,
    QALYs, HIV incidence) for single interventions and combinations
    (deterministic and PSA), which will be used in subsequent analyses.

3.  [Analysis](https://github.com/HERU-LEM/LEMHIVpack/tree/master/03_Analysis)
    contains R scripts to use raw model outputs generated in
    **R/02\_Run\_model** to conduct the primary analyses:
    cost-effectiveness analysis (CEA), HIV incidence reduction between
    different implementation scenarios and the status-quo, disaggregated
    cost breakdown by component (implementation, medical care, PrEP,
    ART, MOUD), determining optimal combination implementation strategy
    (OCIS), generating health production functions, and other
    visualizations of results.

Background material
===================

For more detail on the evidence synthesis, model development, and
analysis, please see the following manuscripts:

-   Krebs, E., Enns, B., Wang, L., Zang, X., Panagiotoglou, D., Del Rio,
    C., Dombrowski, J., Feaster, D. J., Golden, M., Granich, R.,
    Marshall, B., Mehta, S. H., Metsch, L., Schackman, B. R.,
    Strathdee, S. A., Nosyk, B., & localized HIV modeling study group
    (2019). [“Developing a dynamic HIV transmission model for 6 U.S.
    cities: An evidence
    synthesis”](https://dx.plos.org/10.1371/journal.pone.0217559). PloS
    one, 14(5), e0217559.
    <a href="https://doi.org/10.1371/journal.pone.0217559" class="uri">https://doi.org/10.1371/journal.pone.0217559</a>

-   Zang, X., Krebs, E., Min, J. E., Pandya, A., Marshall, B.,
    Schackman, B. R., Behrends, C. N., Feaster, D. J., Nosyk, B., &
    Localized HIV Modeling Study Group (2020). [“Development and
    Calibration of a Dynamic HIV Transmission Model for 6 US
    Cities”](https://journals.sagepub.com/doi/10.1177/0272989X19889356).
    Medical decision making : an international journal of the Society
    for Medical Decision Making, 40(1), 3–16.
    <a href="https://doi.org/10.1177/0272989X19889356" class="uri">https://doi.org/10.1177/0272989X19889356</a>

-   Krebs, E., Zang, X., Enns, B., Min, J. E., Behrends, C. N., Del Rio,
    C., Dombrowski, J. C., Feaster, D. J., Gebo, K. A., Golden, M.,
    Marshall, B., Metsch, L. R., Schackman, B. R., Shoptaw, S.,
    Strathdee, S. A., Nosyk, B., & Localized Economic Modeling Study
    Group (2020). [“The impact of localized implementation: determining
    the cost-effectiveness of HIV prevention and care interventions
    across six United States
    cities”](https://journals.lww.com/aidsonline/Abstract/2020/03010/The_impact_of_localized_implementation_.12.aspx).
    AIDS (London, England), 34(3), 447–458.
    <a href="https://doi.org/10.1097/QAD.0000000000002455" class="uri">https://doi.org/10.1097/QAD.0000000000002455</a>

-   Nosyk, B., Zang, X., Krebs, E., Enns, B., Min, J. E., Behrends, C.
    N., Del Rio, C., Dombrowski, J. C., Feaster, D. J., Golden, M.,
    Marshall, B., Mehta, S. H., Metsch, L. R., Pandya, A., Schackman, B.
    R., Shoptaw, S., Strathdee, S. A., & Localized HIV Modeling Study
    Group (2020). [“Ending the HIV epidemic in the USA: an economic
    modelling study in six
    cities”](https://www.thelancet.com/journals/lanhiv/article/PIIS2352-3018(20)30033-3/fulltext).
    The lancet. HIV, 7(7), e491–e503.
    <a href="https://doi.org/10.1016/S2352-3018(20)30033-3" class="uri">https://doi.org/10.1016/S2352-3018(20)30033-3</a>
