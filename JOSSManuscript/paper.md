---
title: 'stRat stat: a R-based software for the digitization of stratigraphic sections and assembly of large-stratigraphic datasets'
tags:
- R
- geology
- sedimentology
- stratigraphy
- reservoir modeling
authors:
- name: Daniel S. Coutts
- orcid: 0000-0002-2345-2865
- affiliation: 1
affiliations:
- Name: Department of Geoscience, University of Calgary
Index: 1
date: 12 January 2020
bibliography: paper.bib
---

# Introduction

Stratigraphic sections are a main tool used by sedimentologists, stratigraphers, and basin analysists communicate the nature of the sedimentary record to others. Stratigraphic sections (i.e., measured sections and core logs) are used to describe bedding thickness, grain size, occurrence of sedimentary structures, or the facies within a sedimentary succession or lithesome ‘[@Macauley2013; @Wang2019]’. They are commonly composed of an x-axis that is used to describe the grain size (i.e., sediment diameter) of the sediment and a y-axis is used to describe the thickness of the drawn interval. The thickness of an individual stratigraphic section ranges from meters, described at a mm- to cm- scale, to hundreds of meters, described at a m- to Dm-scale. Detailed stratigraphic sections (i.e., mm- to cm-scale data) may be used in the prediction or simulation of subsurface reservoirs, while large-scale sections (i.e., m- to Dm-scale data) may be used to describe or model the history of an entire sedimentary basin.
Stratigraphic sections are predominantly interpreted by visually comparing the grain size, thickness, or character of beds or facies qualitatively; however, there are statistical measures that are used to determine sedimentary environment ‘[@Felletti2010]’, interpret bed thickness trends ‘[@Sylvester2007]’, or stratigraphic order of facies ‘[@Li2018]’. As such, these useful statistics require numerical inputs for stratigraphic characteristics. Available software has largely focused on the creation of uniform stratigraphic sections for communication in journal publications ‘[@Zervas2009; @Wolniewicz2014]’ rather than the analysis the stratigraphic sections ‘[@Lewis2014; @Salles2018]’. As stratigraphic data is used in numerous digital applications (e.g., reservoir modelling, numerical modelling of sedimentary systems, prediction of subsurface facies), an effective way to translate these data types into numerical format is required.
stRat stat is an open-source software written in R that allows for the digitization, discretization, and analysis of a hand-drawn or digitally-produced stratigraphic section for statistical analysis or input in to machine learning algorithms. stRat stat leverages the vectorized nature of R operations, used to summarize large stratigraphic libraries, as well as widely-adopted packages (e.g., Shiny) that allow for user interaction. The goal of stRat stat is to provide platform for geoscientists to derive quantitative metrics from previously and newly collected stratigraphic data.

# Description of stRat stat

stRat stat is an R Shiny application that transforms visual stratigraphic sections into a digital format so that they can be leveraged in computational and statistical analysis. stRat stat contains three main functionalities. First, users digitize a stratigraphic section, demarcating bed boundaries, grain size divisions, sedimentary structures, facies, architectural elements, and stratigraphic intervals. This allows for the software to discretize the section into a numerical format. Secondly, users can summarize and join additional discrete measurements (e.g., core measurements) and continuous measurements (e.g., petrophysical borehole measurements) that are associated with the stratigraphic section. Lastly, multiple sections can be joined together to create a database for the analysis and comparison of numerous stratigraphic sections at once. 
The digitization and discretization of desired stratigraphic sections is done through a user interface that utilizes the R Shiny package, where users hand-select points on an uploaded image of a selected stratigraphic section. As stratigraphic sections vary significantly (e.g., both the positive direction and scale of the x-axis cannot be guaranteed for all users), the user-driven input is required. The pixel locations of points picked by users are recorded and later converted into thickness measurements (y-axis) and grain size (x-axis) once the discretization process is run. Key statistics can be computed for each bed (e.g., bed thickness, min/mean/max grain size), and other statistics are computed for facies, elements, and element sets/stratigraphic intervals (e.g., net:gross, min/mean/max grain size, amalgamation ratio, thickness, number of beds, etc.). The final output of this process is a single .csv file that communicates the different recorded parameters (columns) as a function of thickness (rows). 
The addition and summarization of discrete measurements (e.g., x-ray fluorescence, core permeability) and continuous measurements (e.g., borehole petrophysical logs) is key for large-scale characterization of the sedimentary record. The location of discrete measurements are rounded to the nearest discretized thickness value and joined. Continuous data are first interpolated to match the scale of digitized section and then joined. In both cases, input measurements can be summarized (e.g., min/mean/max, standard deviation) at the different hierarchical levels (i.e., bed, facies, element, element set/interval) to provide insight into variability of the measurements at these scales. 
stRat stat is novel in three ways: 1) it allows for stratigraphic sections from any literature source to be discretized and used in quantitative applications; 2) it provides a basic platform that allows for future, case specific, statistical operations to be added (e.g., proportion of sedimentary structures within a facies); and 3) it allows for geoscientists without computational background to transform these data in an interactive format. Together, this software allows for an increased user-group to access, quantify, and interpret hundreds to thousands of kilometers of stratigraphic sections that are available in peer-review journal publications, academic theses, or subsurface core logs. 

# Acknowledgements

Insight into functional programming was provided by Anton Biryukov, and insight into the uses and requirements of stratigraphic data were provided by Sarah Southern and Zane Jobe.

# References
