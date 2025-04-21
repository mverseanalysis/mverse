## v0.2.0

+  The package was archived due to an error caused by a dependency update.
+  This update fixes the error plus implements an updated method for plotting.
+  `ttest_mverse()` was updated to `t_test_mverse()`
+  The update also includes other minor updates such as updated CITATION using
`bibentry()`, updated reference for `soccer`, updated `WORDLIST`, etc.

### Local `devtools::check(remote = TRUE, manual = TRUE)` results

── R CMD check results ───────────────────────────────────────────────────────────────────────────────────── mverse 0.2.0 ────
Duration: 1m 8s

❯ checking CRAN incoming feasibility ... [3s/11s] NOTE
  Maintainer: ‘Michael Jongho Moon <michael.moon@utoronto.ca>’
  
  New submission
  
  Package was archived on CRAN
  
  Possibly misspelled words in DESCRIPTION:
    Hullman (21:59)
    Sarma (21:6)
    Taback (21:34)
    explorable (22:71)
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2024-11-02 as issues were not corrected
      in time.

❯ checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

### `rhub::rhub_check()` results

- Status: OK on all runs
- Results available at https://github.com/mverseanalysis/mverse/actions/runs/14580602416

### `devtools::check_win_devel()` results

* checking CRAN incoming feasibility ... NOTE
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Michael Jongho Moon <michael.moon@utoronto.ca>'

New submission

Package was archived on CRAN

Possibly misspelled words in DESCRIPTION:
  Hullman (21:59)
  Sarma (21:6)
  Taback (21:34)
  explorable (22:71)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2024-11-02 as issues were not corrected
    in time.

Status: 1 NOTE


## v.0.1.0

------------------------------------------------------

This is the first CRAN release.

### Local `devtools:check()` result

── R CMD check results ───────────────────────────────────────────── mverse 0.1.0 ────
Duration: 37.9s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

### RHub `devtools::check_rhub()` result

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Michael Jongho Moon <michael.moon@mail.utoronto.ca>'

New submission

Possibly misspelled words in DESCRIPTION:
  Hullman (21:59)
  Multiverse (3:13)
  Sarma (21:6)
  Taback (21:34)
  explorable (22:71)
  multiverse (23:5, 28:5)

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1073/pnas.1402786111
    From: man/hurricane.Rd
          inst/doc/mverse_intro_glmmodelling.html
    Status: 503
    Message: Service Unavailable
    
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

### Winbuilder `devtools::check_win_devel()` result

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Michael Jongho Moon <michael.moon@mail.utoronto.ca>'

New submission

Possibly misspelled words in DESCRIPTION:
  Hullman (21:59)
  Sarma (21:6)
  Taback (21:34)
  explorable (22:71)

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1073/pnas.1402786111
    From: man/hurricane.Rd
          inst/doc/mverse_intro_glmmodelling.html
    Status: 503
    Message: Service Unavailable

