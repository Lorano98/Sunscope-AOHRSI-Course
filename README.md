<img src="https://github.com/user-attachments/assets/94a0aa81-9109-4294-8d94-6d7f8c421dd8" width="300" align="right">

# Sunscope-AOHRSI-Course-

A repository for detecting solar panels on rooftops on high resolution imagery in R and QGIS.

## Motivation
In view of climate change, phasing out conventional energy sources is one of the most important issues of our time. Germany's climate targets are clearly formulated: greenhouse gas emissions are to be reduced by at least 65% by 2030, by at least 88% by 2040 and net greenhouse gas neutrality is to be achieved by 2045. One effective way for private households to do their bit is to install solar panels on the roofs of their homes. We want to investigate the extent to which this option is already being utilised and how much potential there is for improvement. To do this, we want to use In view of climate change, phasing out conventional energy sources is one of the most important issues of our time. Germany's climate targets are clearly formulated: greenhouse gas emissions are to be reduced by at least 65% by 2030, by at least 88% by 2040 and net greenhouse gas neutrality is to be achieved by 2045. One effective way for private households to do their bit is to install solar panels on the roofs of their homes. We want to investigate the extent to which this option is already being utilised and how much potential there is for improvement. To do this, we want to use deep learning to automatically detect solar panels on satellite imagesdeep learning to automatically detect solar panels on satellite images.

## Goal
* Detecting solar panels on rooftops
* In an easy way:
  * Minimal human resources
  * Small amount of training data
  * Estimating the missed potential concerning the nationwide coverage in solarpanels
* In future -> Finding potential rooftops for solar panels

## Workflow

![image](https://github.com/user-attachments/assets/7571980f-13cd-4470-bece-e1396a60a2af)

## DIY

1. Get your own tif file depicting some solar panels.

2. To make the running of the R code easier, you can follow this [tutorial](https://github.com/DaChro/ogh_summer_school_2020) to install the enivornment yourself or the docker image. Then you can just run the script with your own training data!

3. In the second step you can download the QGIS Model deep_learning_post_processing from the visualization folder and apply it to your results from the R Script.

4. If you want to train the model yourself you can go to the above link and follow the tutorial.

## Data Sources
- Trainingsdata:  
  Maxar solar panel dataset HD  
  https://blog.maxar.com/tech-and-tradecraft/2022/maxars-hd-technology-provides-measurable-improvements-in-machine-learning-applications
- Predictiondata:  
  Digital Orthophoto from Geoportal NRW  
  https://www.geoportal.nrw
- Building outline data:  
  Stadt Münster  
  https://opendata.stadt-muenster.de/dataset/geb%C3%A4udeumrisse
