
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mverse

<!-- badges: start -->

<!-- badges: end -->

*mverse* is an extension to multiverse package (Sarma et al. 2021) which
allows users create explorable multiverse analysis (Steegen et al. 2016)
in R. This extension provides user friendly abstraction and a set of
examples for researchers, educators, and students in statistics.

## Installation

<!-- You can install the released version of mverse from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("mverse") -->

<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mverseanalysis/mverse")
```

## Example: Hurricane Names and Gender-based Expectations

This is an example which demonstrating a full analysis using `mverse`
using the `hurricane` dataset included in the package. The data set
comes from a study by Jung et al. (2014) and contains the following
information about hurricanes that landed on the U.S. between 1950 and
2021:

  - Femininity rating on hurricane names (1:very masculine; 11: very
    feminine)
  - Total fatality counts
  - Total damage dollar amount normalized to 2013 USD
  - Maximum wind speed
  - Minimum pressure
  - Year

Below are the first few lines of the dataset. See `?hurricane` for
details on the dataset.

    #>       Name Year alldeaths  MasFem Minpressure_Updated_2014 HighestWindSpeed
    #> 1     Easy 1950         2 5.40625                      960              125
    #> 2     King 1950         4 1.59375                      955              134
    #> 3     Able 1952         3 2.96875                      985              125
    #> 4  Barbara 1953         1 8.62500                      987               75
    #> 5 Florence 1953         0 7.87500                      985              115
    #> 6    Carol 1954        60 8.53125                      960              115
    #>    NDAM Category MinPressure_before Elapsed.Yrs Source
    #> 1  2380        3                958          63    MWR
    #> 2  7220        4                955          63    MWR
    #> 3   210        1                985          61    MWR
    #> 4    78        1                987          60    MWR
    #> 5    21        1                985          60    MWR
    #> 6 24962        3                960          59    MWR

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Inspecting the data reveals that 8 of the top 10 hurricanes that caused
most fatalities had female names while only 6 of the top 10 hurricanes
with the most financial damage had female names. Jung et al. (2014)
hypothesized that this gap was due to people underestimating the
severity of a hurricane when it’s named with a female name. Jung used
the data to investigate whether hurricanes with *feminine* names led to
*more fatalities given equal strength* because their names didn’t
motivate as much preparedness as hurricanes with *masculine* names.

To illustrate a multiverse analysis using `mverse`, we will consider a
Poisson regression model that models the relationship between the
femininity of a hurricane’s name and the total fatalities it caused,
while controlling for the strength of the hurricane. We will expand the
multiverse based on the following two decision points:

1.  Are there any outliers that we should remove from analysis? If so,
    which ones should we exclude?
2.  Which variable best captures the strength of a hurricane?

In `mverse`, such decision points are called *branches* and the
individual paths we can take at each branch are called *options*.

### Branches for Outliers

Upon inspecting the distributions of the fatalities, financial damage
amounts, maximum wind speeds, and minimum pressures, we may choose to
exclude

  - Katrina, 2005 only;
  - Katrina, 2005 and Audrey, 1957;
  - none.

We can use `filter_branch` to declare the four options, or branches, of
defining the outliers.

### Mutate Branch: Branches for Defining a Variable

To control for the strength of a hurricane, we may use one of

  - Total damage dollar amount normalized to 2013 USD;
  - Maximum wind speed; and
  - Minimum pressure

from the data set. We can define a new variable `hurricane_strength`
using `mutate_branch` such that we create a multiverse that investigates
all three options.

### GLM mverse: Fit a GLM Model

To fit a Poisson regression model, we can use `glm_mverse` in the
`mverse` library. `glm_mverse` runs `glm` method across the multiverse.
`glm_mverse` needs to pass the model specification using a formula and
the likelihood family to `glm` in each universe. We can specify the
formula and the family using `formula_branch` and `family_branch`.

Once we have all branches defined, we can add them to the `mverse`
object using `add_***_branch` methods and fit the model with
`glm_mverse`.

### Summary and Specification Curve: Inspect Results

After completing the analysis, we can extract the results using
`summary` method. The method returns a `tibble` with branching options,
estimates, 95% confidence intervals for all regression terms across the
multiverse. Below, we focus on the main effect on *femininity*.

We can also inspect the result graphically using `spec_curve` method.
The method builds a specification curve analysis (Simonsohn, Simmons,
and Nelson 2020) for the specified term. By default, the universes are
sorted by the estimate of interest specified by `var="femininity"`.

The method also allows sorting the universes by whether p-value \< 0.05
or by branch options.

## References

<div id="refs" class="references">

<div id="ref-hurricane">

Jung, Kiju, Sharon Shavitt, Madhu Viswanathan, and Joseph M. Hilbe.
2014. “Female Hurricanes Are Deadlier Than Male Hurricanes” 111 (24):
8782–7. <https://doi.org/10.1073/pnas.1402786111>.

</div>

<div id="ref-multiverseR">

Sarma, Abhraneel, Alex Kale, Michael Moon, Nathan Taback, Fanny
Chevalier, Jessica Hullman, and Matthew Kay. 2021. “Multiverse:
Multiplexing Alternative Data Analyses in R Notebooks (Version 0.5.0).”
<https://github.com/MUCollective/multiverse>.

</div>

<div id="ref-speccurve">

Simonsohn, Uri, Joseph P. Simmons, and Leif D. Nelson. 2020.
“Specification Curve Analsysis” 4 (July): 1208–14.
<https://doi.org/10.1038/s41562-020-0912-z>.

</div>

<div id="ref-multiverse">

Steegen, Sara, Francis Tuerlinckx, Andrew Gelman, and Wolf Vanpaemel.
2016. “Increasing Transparency Through a Multiverse Analysis” 11 (5):
702–12. <https://doi.org/10.1177/1745691616658637>.

</div>

</div>
