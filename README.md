# Research title: Using Pseudo-Colour and Changing Coordinates Can Support ECG Interpretation

We show that visualising an electrocardiograph (ECG) using Polar coordinates, and applying a horizontal pseudo-colour filter over the signal, significantly helps lay-people to detect QT-interval increases. Here is the link for the research paper: xxxxx

## Getting Started

This repository will get you a copy of the R-scripts and the ECG signals on your local machine for development and testing purposes. 

### Prerequisites

You need to install R and RStudio software to run the scripts.

```
https://www.rstudio.com/products/rstudio/download/
```

### Installing

You need also to install some R packages including  

```
(pracma)
(readr)
(tibble)
(signal)
(readxl)
(dplyr)
(tidyr)
(ggplot2)
(ggcyto)
```

### Images folder

The images folder containes full size image (PNG type) showing examples of normal
(QT < 430), borderline(QT > 440 and < 470), prolonged (QT > 470 and < 500) and very prolonged (QT > 500). 
The image name are coded as the value of QT-interval in millisconds and the type of the visualisation technique, where 'C' indicates Cartesian, 'CC' indicates Cartesian with Colour, 'P' indicates Polar and 'PC' indicates Polar with Colour.

## R-scripts folder

The R-scripts used to create the visualisation techniques are provided for each QT exmaple, along with the excels files of the raw ECG signals. 

## Built With
RStudio software version 1.1.447.

## Authors

**Alaa Alahmadi** - 

See also the list of [co-authors](xxxx) who participated in this research.


