 <!-- badges: start -->
  [![R build status](https://github.com/mattmalcher/lift.tracker/workflows/R-CMD-check/badge.svg)](https://github.com/mattmalcher/lift.tracker/actions)
  <!-- badges: end -->

# lift.tracker
A graphical representation of how often the lift in south city court is broken. 

# Where are the results?
Live at: **https://mattmalcher.github.io/lift.tracker/**

# Why are you doing this?
Because carrying your bike up 4 floors is a pain.

# How do I add data?
Fill out the form at: https://forms.gle/895QEMSCkqaR9vrr7

This should update within 3 hours (current action schedule)


# How does this work?
* google form to get data
* google sheet to collect data
* R for data vis
    * googlesheets4
    * ggplot2 - see [this vignette](https://cran.r-project.org/web/packages/sugrrants/vignettes/frame-calendar.html)
* github actions to update - see [here](https://github.com/mattmalcher/lift.tracker/blob/main/.github/workflows/cal_1.yaml)
* github pages for hosting
