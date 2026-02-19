
<!-- README.md is generated from README.Rmd. Please edit that file --> 

# Neotoma Lakes App <img src="www/neotomalakes_logo.png" align="right" height="200" />

<!-- badges: start -->

[![check-app](https://github.com/flor14/neotoma-lakes/actions/workflows/check-app.yaml/badge.svg)](https://github.com/flor14/neotoma-lakes/actions/workflows/check-app.yaml)
[![codecov](https://codecov.io/gh/flor14/neotoma-lakes/branch/main/graph/badge.svg)](https://codecov.io/gh/flor14/neotoma-lakes)
<!-- badges: end -->

The [Neotoma Paleoecology Database
(“Neotoma”)](https://www.neotomadb.org/) is more than a database!
Neotoma is a database, a software ecosystem, and a community.

Neotoma provides an underlying cyberinfrastructure that enables the
development of common software tools for data ingest, discovery,
display, analysis, and distribution, while giving domain scientists
control over critical taxonomic and other data quality issues.

## What is Neotoma Lakes app?

The Neotoma Lakes app is an R-Shiny application designed to assist in
improving lake entries within the Neotoma database.

1.  Begin by entering the SiteID: ![](www/siteid_screen.png)
2.  Water bodies from the [Hydrolakes
    DB](https://wp.geog.mcgill.ca/hydrolab/hydrolakes/) near the site
    will be displayed in blue. Clicking on these water bodies allows you
    to access information within the Hydrolakes DB associated with each
    lake on the right panel. If one of these lakes represents better the
    Neotoma DB Site, you can submit it along with comments.
    ![](www/hydrolakes_screen.png)
3.  If you cannot find a polygon representing your water body in the
    Hydrolakes DB, you have the option to create your own polygon. In
    doing so, you’ll be able to view the information about the polygon
    in the right panel and submitting it to improe the database.
    ![](www/create_poly_screen.png)

## App Access and Local Deployment

#### Accessing the App

The application can be accessed through the link available in the
‘About’ section of this GitHub repository.

#### Running the App Locally

To run the app on your local machine using Docker, follow these steps:

1 - Clone or Download the Repository Example using the terminal:

    git clone git@github.com:flor14/neotoma-lakes.git

2 - Install Docker Desktop

Install [Docker
Desktop](https://www.docker.com/products/docker-desktop/) if you haven’t
done so yet and open it.

3 - Building the App

Navigate to the folder where the project is located. Execute the
`docker build` command to generate the Docker image:

    cd neotomalakes
    docker build -t neotomalakesapp .

This step can take some minutes. Then, start the app with this command:

    docker run --rm -p 3838:3838 neotomalakesapp

**Note:** If you are using a Mac M1/M2, please add the argument
`--platform linux/amd64` to the last command.

The app will now be accessible locally at `http://localhost:3838`.

## Community guidelines

Report Issues:

- Questions, feedback, bug reports: please open an issue in the issue
  tracker of the project
  [here](https://github.com/flor14/neotoma-lakes/issues).

Contribution to the software:

- Please open an issue in the issue tracker of the project that
  describes the changes you would like to make to the software and open
  a pull request with the changes. The description of the pull request
  must reference the corresponding issue.

- Join the [Neotoma
  Community](https://www.neotomadb.org/about/join-the-neotoma-community)
