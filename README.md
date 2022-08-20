Brookfield Institute Data Visualization Style Guide
================

## Introduction

This package, and associated data visualization style guide lays out the
style elements for data visualization used at the Brookfield Institute.

## Colours

At the Brookfiled Institute, we use three primary colours: Magenta:
`#dd347a` , Cyan: `#8ad4df` ,and Yellow: `#faaf19`

## Installation

As the fonts used by the Brookfield Institute in data visualization is
licensed only for our use - you will notice that those font files are
missing from this public repository. To download the package binary, go
to the Policy Team Resources folder on our Google Drive and you will be
able to find it here. This public version is available for reference
purposes.

Installing the package is fairly straightforward, except for a fact that
if you are working on a Mac, you will have to switch your RStudio
graphical device to Ragg. This ensures that fonts are rendered
correctly.

## Update logs

Current Version: 1.0.0

New in Version 1.0.0

-   Completely revamped font support - uses showtext now instead of
    extrafont
-   Added option to add BIIE logo to generated graphics
-   Updated graph styles to match revamped Brookfield style and font
    guide as of AUgust 2022
-   Note that for Caption to work properly, R graphical device has to be
    set to RAGG

New in Version 0.3.0

-   Add new plot.area.bf Function
-   Add options for ordering of column
-   Fixes some irregularities in input name for plot.scatter.bf
