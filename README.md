Brookfield Institute Data Visualization Style Guide
================

![brookfield_logo](inst/extdata/big_icon.png)

## Introduction

This package, and associated data visualization style guide lays out the
style elements for data visualization used at the Brookfield Institute.

## Anatomy of a graphic at the Brookfield Institute

![sample_image](inst/extdata/graph_anatomy.svg)

## Available graph types in the current version of the package

| Graph Type             | Function                 | Use Cases                                                                          |
|------------------------|--------------------------|------------------------------------------------------------------------------------|
| **Bar graph**          | `plot.column.bf()`       | Comparing single metric across different groups                                    |
| **Scatter graph**      | `plot.scatter.bf()`      | Plot relationship between two metrics across observations                          |
| **Waffle chart**       | `plot.waffle.bf()`       | Represent the shares of different groups in a population                           |
| **Line chart**         | `plot.line.bf()`         | Compare change in metric across multiple period (usually) for different groups     |
| **Pyramid chart**      | `plot.pyramid.bf()`      | Commonly used to represent age pyramid and similar data                            |
| **Mekko chart**        | `plot.mekko.bf()`        | A variant of a bar graph where the width of each bar represents a different metric |
| **Arrow Change chart** | `plot.change.arrow.bf()` | Show change in a metric across two time periods across different groups            |
| **Area chart**         | `plot.area.bf()`         | Used commonly to show how shares of different groups shift over time               |
| **Province map**       | `plot.map.prov.bf()`     | Maps out data at the provincial level                                              |
| **ER map**             | `plot.map.er.bf()`       | Maps out data at the Economic Region level                                         |
| **CMA map**            | `plot.map.cma.bf()`      | Maps out data at the Census Metropolitan Area/Census Agglomeration level           |
| **CSD map**            | `plot.map.csd.bf()`      | Maps out data at the Census Subdivision level                                      |

## Colours

At the Brookfiled Institute, we use three primary colours:

- Magenta: ![\#dd347a](https://via.placeholder.com/15/dd347a/dd347a.png)
  `#dd347a`
- Cyan: ![\#8ad4df](https://via.placeholder.com/15/8ad4df/8ad4df.png)
  `#8ad4df`
- Indigo: ![\#14365D](https://via.placeholder.com/15/14365D/14365D.png)
  `#14365d`

Magenta ![\#dd347a](https://via.placeholder.com/15/dd347a/dd347a.png) is
often used as a highlight colour, in highlighting important data points,
and is not recommended to use as the base colour of comparison.

In addition, we use the following secondary colours:

- Yellow: ![\#faaf19](https://via.placeholder.com/15/faaf19/faaf19.png)
  `#faaf19`
- Green: ![\#3C7D78](https://via.placeholder.com/15/3C7D78/3C7D78.png)
  `#3c7d78`
- Pink: ![\#FFA5B9](https://via.placeholder.com/15/FFA5B9/FFA5B9.png)
  `#ffa5b9`
- Brown: ![\#C37546](https://via.placeholder.com/15/C37546/C37546.png)
  `#c37546`
- Ecru: ![\#EBE6DE](https://via.placeholder.com/15/EBE6DE/EBE6DE.png)
  `#ebe6de`
- Grey: ![\#626466](https://via.placeholder.com/15/626466/626466.png)
  `#626466`

Any graphic will use at least one colour from the primary palette. The
secondary palette should not be used without any colours from the
primary palette. Programmatically, the function that will generate
colours is `set.colours()`. You can specify in a vector the specific
combination of colours you’d like in the order they appear, and pass it
in the `colours` argument in most graphing functions.

    set.colours(3, categorical.choice = c("magenta","ecru","green"))

### Discrete Colour combinations

The following gives suggested colour combinations for specific number of
colours.

#### 2 colours

One from the primary palette + one from secondary. For example:

- Indigo ![\#14365D](https://via.placeholder.com/15/14365D/14365D.png)
  `#14365d` + Green
  ![\#3C7D78](https://via.placeholder.com/15/3C7D78/3C7D78.png)
  `#3c7d78`
- Indigo ![\#14365D](https://via.placeholder.com/15/14365D/14365D.png)
  `#14365d` + Ecru
  ![\#EBE6DE](https://via.placeholder.com/15/EBE6DE/EBE6DE.png)
  `#ebe6de`
- Magenta ![\#dd347a](https://via.placeholder.com/15/dd347a/dd347a.png)
  `#dd347a` + Grey
  ![\#626466](https://via.placeholder.com/15/626466/626466.png)
  `#626466`
- Magenta ![\#dd347a](https://via.placeholder.com/15/dd347a/dd347a.png)
  `#dd347a` + Pink
  ![\#FFA5B9](https://via.placeholder.com/15/FFA5B9/FFA5B9.png)
  `#ffa5b9`
- Cyan ![\#8ad4df](https://via.placeholder.com/15/8ad4df/8ad4df.png)
  `#8ad4df` + Green
  ![\#3C7D78](https://via.placeholder.com/15/3C7D78/3C7D78.png)
  `#3c7d78`
- Cyan ![\#8ad4df](https://via.placeholder.com/15/8ad4df/8ad4df.png)
  `#8ad4df` + Pink
  ![\#FFA5B9](https://via.placeholder.com/15/FFA5B9/FFA5B9.png)
  `#ffa5b9`

#### 3 colours

One primary colour + two secondary colours. For example:

- Indigo ![\#14365D](https://via.placeholder.com/15/14365D/14365D.png)
  `#14365d` + Green
  ![\#3C7D78](https://via.placeholder.com/15/3C7D78/3C7D78.png)
  `#3c7d78` + Pink
  ![\#FFA5B9](https://via.placeholder.com/15/FFA5B9/FFA5B9.png)
  `#ffa5b9`
- Indigo ![\#14365D](https://via.placeholder.com/15/14365D/14365D.png)
  `#14365d` + Brown
  ![\#C37546](https://via.placeholder.com/15/C37546/C37546.png)
  `#c37546` + Ecru
  ![\#EBE6DE](https://via.placeholder.com/15/EBE6DE/EBE6DE.png)
  `#ebe6de`
- Cyan ![\#8ad4df](https://via.placeholder.com/15/8ad4df/8ad4df.png)
  `#8ad4df` + Ecru
  ![\#EBE6DE](https://via.placeholder.com/15/EBE6DE/EBE6DE.png)
  `#ebe6de` + Grey
  ![\#626466](https://via.placeholder.com/15/626466/626466.png)
  `#626466`
- Magenta ![\#dd347a](https://via.placeholder.com/15/dd347a/dd347a.png)
  `#dd347a` + Ecru
  ![\#EBE6DE](https://via.placeholder.com/15/EBE6DE/EBE6DE.png)
  `#ebe6de` + Green
  ![\#3C7D78](https://via.placeholder.com/15/3C7D78/3C7D78.png)
  `#3c7d78`

#### 4+ colours

When you have more than 4 colours, you are able to use more than one
primary colour, and include gradient as appropriate. Note that you
should never exceed more than 7 separate colours.

- Indigo ![\#14365D](https://via.placeholder.com/15/14365D/14365D.png)
  `#14365d` + Brown
  ![\#C37546](https://via.placeholder.com/15/C37546/C37546.png)
  `#c37546` + Pink
  ![\#FFA5B9](https://via.placeholder.com/15/FFA5B9/FFA5B9.png)
  `#ffa5b9` + Yellow
  ![\#faaf19](https://via.placeholder.com/15/faaf19/faaf19.png)
  `#faaf19`
- Magenta ![\#dd347a](https://via.placeholder.com/15/dd347a/dd347a.png)
  `#dd347a` + Pink
  ![\#FFA5B9](https://via.placeholder.com/15/FFA5B9/FFA5B9.png)
  `#ffa5b9` + Cyan
  ![\#8ad4df](https://via.placeholder.com/15/8ad4df/8ad4df.png)
  `#8ad4df` + Indigo
  ![\#14365D](https://via.placeholder.com/15/14365D/14365D.png)
  `#14365d`

### Colour gradients

In addition to discrete colours, the style guide (and the package),
provide the following gradients that can be used. Gradients are
especially useful in representing data that has a monotonic range
interpretation (for example in heatmaps). Programmatically, you just
need to specify in `set.colours()` how many colours you’re looking for,
and from which gradient you’d like the colours to come from:

    set.colours(5, type = "gradient", gradient.choice="indigo")

In our official style guide, it is recommended that when gradients are
used, the following three are prioritized:

- Indigo: ![\#14365D](https://via.placeholder.com/15/14365D/14365D.png)
  `#14365d`
  ![\#284469](https://via.placeholder.com/15/284469/284469.png)
  `#284469`
  ![\#4f6885](https://via.placeholder.com/15/4F6885/4F6885.png)
  `#4f6885`
  ![\#4c6282](https://via.placeholder.com/15/4c6282/4c6282.png)
  `#4c6282`
  ![\#6f829b](https://via.placeholder.com/15/6f829b/6f829b.png)
  `#6f829b`
  ![\#8a9bae](https://via.placeholder.com/15/8a9bae/8a9bae.png)
  `#8a9bae`
  ![\#8192a8](https://via.placeholder.com/15/8192a8/8192a8.png)
  `#8192a8`
  ![\#94a3b5](https://via.placeholder.com/15/94a3b5/94a3b5.png)
  `#94a3b5`
- Green: ![\#3C7D78](https://via.placeholder.com/15/3C7D78/3C7D78.png)
  `#3c7d78`
  ![\#528b86](https://via.placeholder.com/15/528b86/528b86.png)
  `#528b86`
  ![\#6D9E9A](https://via.placeholder.com/15/6D9E9A/6D9E9A.png)
  `#6d9e9a`
  ![\#679894](https://via.placeholder.com/15/679894/679894.png)
  `#679894`
  ![\#9EBEBC](https://via.placeholder.com/15/7ca6a2/7ca6a2.png)
  `#7ca6a2`
  ![\#9ebebc](https://via.placeholder.com/15/9ebebc/9ebebc.png)
  `#9ebebc`
  ![\#a5c2bf](https://via.placeholder.com/15/a5c2bf/a5c2bf.png)
  `#a5c2bf`
  ![\#CEDFDD](https://via.placeholder.com/15/CEDFDD/CEDFDD.png)
  `#CEDFDD`
- Grey: ![\#626466](https://via.placeholder.com/15/626466/626466.png)
  `#626466`
  ![\#7D8890](https://via.placeholder.com/15/7D8890/7D8890.png)
  `#7d8890`
  ![\#8A949B](https://via.placeholder.com/15/8A949B/8A949B.png)
  `#8a949b`
  ![\#939598](https://via.placeholder.com/15/939598/939598.png)
  `#939598`
  ![\#9a9fa3](https://via.placeholder.com/15/9a9fa3/9a9fa3.png)
  `#9a9fa3`
  ![\#a4acb1](https://via.placeholder.com/15/a4acb1/a4acb1.png)
  `#a4acb1`
  ![\#626466](https://via.placeholder.com/15/a5abaf/a5abaf.png)
  `#a5abaf`
  ![\#B1B8BC](https://via.placeholder.com/15/B1B8BC/B1B8BC.png)
  `#b1b8bc`

However, the package provides the following additional gradients for
used when appropriate:

- Magenta: ![\#DD347A](https://via.placeholder.com/15/DD347A/DD347A.png)
  `#dd347a`
  ![\#E04686](https://via.placeholder.com/15/E04686/E04686.png)
  `#e04686`
  ![\#E35892](https://via.placeholder.com/15/E35892/E35892.png)
  `#e35892`
  ![\#E66B9E](https://via.placeholder.com/15/E66B9E/E66B9E.png)
  `#e66b9e`
  ![\#E97DAA](https://via.placeholder.com/15/E97DAA/E97DAA.png)
  `#e97daa`
  ![\#EC90B6](https://via.placeholder.com/15/EC90B6/EC90B6.png)
  `#ec90b6`
  ![\#EFA2C2](https://via.placeholder.com/15/EFA2C2/EFA2C2.png)
  `#efa2c2`
  ![\#F2B5CE](https://via.placeholder.com/15/F2B5CE/F2B5CE.png)
  `#f2b5ce`
- Cyan: ![\#8AD4DF](https://via.placeholder.com/15/8AD4DF/8AD4DF.png)
  `#8ad4df`
  ![\#94D7E1](https://via.placeholder.com/15/94D7E1/94D7E1.png)
  `#94d7e1`
  ![\#9FDBE4](https://via.placeholder.com/15/9FDBE4/9FDBE4.png)
  `#9fdbe4`
  ![\#A9DFE7](https://via.placeholder.com/15/A9DFE7/A9DFE7.png)
  `#a9dfe7`
  ![\#B4E3EA](https://via.placeholder.com/15/B4E3EA/B4E3EA.png)
  `#b4e3ea`
  ![\#BFE7ED](https://via.placeholder.com/15/BFE7ED/BFE7ED.png)
  `#bfe7ed`
  ![\#C9EBF0](https://via.placeholder.com/15/C9EBF0/C9EBF0.png)
  `#c9ebf0`
  ![\#D4EFF3](https://via.placeholder.com/15/D4EFF3/D4EFF3.png)
  `#d4eff3`
- Yellow: ![\#FFC800](https://via.placeholder.com/15/FFC800/FFC800.png)
  `#ffc800`
  ![\#FFCD17](https://via.placeholder.com/15/FFCD17/FFCD17.png)
  `#ffcd17`
  ![\#FFD22E](https://via.placeholder.com/15/FFD22E/FFD22E.png)
  `#ffd22e`
  ![\#FFD745](https://via.placeholder.com/15/FFD745/FFD745.png)
  `#ffd745`
  ![\#FFDC5C](https://via.placeholder.com/15/FFDC5C/FFDC5C.png)
  `#ffdc5c`
  ![\#FFE173](https://via.placeholder.com/15/FFE173/FFE173.png)
  `#ffe173`
  ![\#FFE68B](https://via.placeholder.com/15/FFE68B/FFE68B.png)
  `#ffe68b`
  ![\#FFEBA2](https://via.placeholder.com/15/FFEBA2/FFEBA2.png)
  `#ffeba2`
- Pink: ![\#FFA5B9](https://via.placeholder.com/15/FFA5B9/FFA5B9.png)
  `#ffa5b9`
  ![\#ffafc1](https://via.placeholder.com/15/ffafc1/ffafc1.png)
  `#ffafc1`
  ![\#ffb9c9](https://via.placeholder.com/15/ffb9c9/ffb9c9.png)
  `#ffb9c9`
  ![\#ffc3d0](https://via.placeholder.com/15/ffc3d0/ffc3d0.png)
  `#ffc3d0`
  ![\#ffccd8](https://via.placeholder.com/15/ffccd8/ffccd8.png)
  `#ffccd8`
  ![\#ffd6df](https://via.placeholder.com/15/ffd6df/ffd6df.png)
  `#ffd6df`
  ![\#ffdfe7](https://via.placeholder.com/15/ffdfe7/ffdfe7.png)
  `#ffdfe7`
  ![\#ffe9ee](https://via.placeholder.com/15/ffe9ee/ffe9ee.png)
  `#ffe9ee`
- Brown: ![\#F7941E](https://via.placeholder.com/15/F7941E/F7941E.png)
  `#f7941e`
  ![\#F79D32](https://via.placeholder.com/15/F79D32/F79D32.png)
  `#f79d32`
  ![\#F8A746](https://via.placeholder.com/15/F8A746/F8A746.png)
  `#f8a746`
  ![\#F9B15B](https://via.placeholder.com/15/F9B15B/F9B15B.png)
  `#f9b15b`
  ![\#F9BA6F](https://via.placeholder.com/15/F9BA6F/F9BA6F.png)
  `#f9ba6f`
  ![\#FAC484](https://via.placeholder.com/15/FAC484/FAC484.png)
  `#fac484`
  ![\#FBCE98](https://via.placeholder.com/15/FBCE98/FBCE98.png)
  `#fbce98`
  ![\#FCD8AD](https://via.placeholder.com/15/FCD8AD/FCD8AD.png)
  `#fcd8ad`

## Fonts

The Brookfield Institute styleguide uses 2 main family of fonts,
Pressura, and Rooney. The following general conventions will determine
the correct font usage when making non-standard graphics. In general,
default to showing numbers using Pressura (except for cases when the
numbers are integrated inside a text).

- **Figure numbering**: Use Pressura Bold typeface, 13pt
- **Figure title**: Use RooneySans Regular typeface, 11pt
- **Axis titles**: Use RooneySans Regular typeface, 11pt
- **Axis labels**: Use Pressura Light typeface, 10pt
- **Legend title**: Use RooneySans Light typeface, 11pt
- **Legend label**: Use RooneySans Light typeface, 10pt
- **Annotation**: Use RooneySans Regular typeface, appropriate type
  size.

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

Current Version: 1.2.0 (updated November 29th 2022)

New in Version 1.2.0

- Added option in plot.line.bf to fit loess line instead of just
  connecting dots

New in Version 1.1.0 (November 4th 2022)

- Added 2021 Census geographies (namely for CMA and CSD). Mapping
  functions default to 2021 geographies.
- Changed pacakge structure slightly, splitting each function into its
  own R file.

New in Version 1.0.1

- Fixed a small bug with plot.column.bf() displaying Rooney font when it
  should be showing Pressura font.
- Ensured every external package reference is updated.

New in Version 1.0.0

- Completely revamped font support - uses showtext now instead of
  extrafont
- Added option to add BIIE logo to generated graphics
- Updated graph styles to match revamped Brookfield style and font guide
  as of August 2022
- Note that for Caption to work properly, R graphical device has to be
  set to RAGG

New in Version 0.3.0

- Add new plot.area.bf Function
- Add options for ordering of column
- Fixes some irregularities in input name for plot.scatter.bf
