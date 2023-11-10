---
editor_options:
  markdown:
    wrap: 72
    canonical: true
---

Check out the project's [website](nestanyol.github.io/AdvancedR3/).

# AdvancedR3: Reproducible research in R workshop

This project collects all the steps done during the course, also
available on: <https://r-cubed-advanced.rostools.org/>

# Brief description of folder and file contents

The following folders contain:

-   `data/`:
-   `doc/`:
-   `R/`:

# Installing project R package dependencies

If dependencies have been managed by using
`usethis::use_package("packagename")` through the `DESCRIPTION` file,
installing dependencies is as easy as opening the `AdvancedR3.Rproj`
file and running this command in the console:

```         
# install.packages("remotes")
remotes::install_deps()
```

You'll need to have remotes installed for this to work.

# Resource

For more information on this folder and file workflow and setup, check
out the [prodigenr](https://rostools.github.io/prodigenr) online
documentation.
