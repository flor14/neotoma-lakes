
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Neotoma Lakes App <img src="www/neotomalakes_logo.png" align="right" height="250" />

<!-- badges: start -->

[![test-coverage](https://github.com/flor14/neotoma-lakes/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/flor14/neotoma-lakes/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/flor14/neotoma-lakes/branch/main/graph/badge.svg)](https://codecov.io/gh/flor14/neotoma-lakes)
<!-- badges: end -->

The Neotoma Paleoecology Database (“Neotoma”) is more than a database!
Neotoma is a database, a software ecosystem, and a community.

Neotoma provides an underlying cyberinfrastructure that enables the
development of common software tools for data ingest, discovery,
display, analysis, and distribution, while giving domain scientists
control over critical taxonomic and other data quality issues.

## What is Neotoma Lakes app?

The Neotoma Lakes app is an R-Shiny application designed to assist in
improving lake entries within the Neotoma database.

1.  Begin by entering the SiteID: ![](www/siteid_screen.png)

2.  2.  Water bodies from the [Hydrolake
        DB](https://wp.geog.mcgill.ca/hydrolab/hydrolakes/) near the
        site will be displayed in blue. Clicking on these water bodies
        allows you to access information within the HYDROLAKE DB
        associated with each lake on the right panel. If one of these
        lakes represents better the Neotoma DB Site, you can submit it
        along with comments. ![](www/hydrolakes_screen.png)

3.  If you cannot find a polygon representing your water body in the
    Hydrolakes DB, you have the option to create your own polygon. In
    doing so, you’ll be able to view the information about the polygon
    in the right panel and submitting it to improe the database.
    ![](www/create_poly_screen.png)

## Do you want to contribute?

- Join the [Neotoma
  Community](https://www.neotomadb.org/about/join-the-neotoma-community)
