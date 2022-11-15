# Bayesian Velocity Models for Dynamic Horse Racing Valuation

### Canadian Pharaoh

### Tyrel Stokes, Kimberly Kroetch, Gurashish Bagga, Liam Welsh, Brendan Kumagai

### 2022 Big Data Derby

This repository contains all code used for our 2022 Big Data Derby submission along with visualizations used in our paper.

## Repository Directory

### Data

The data in this Github contains just the necessary files needed to run our code from scratch that we obtained from outside of the Big Data Derby "Data" page on Kaggle. This includes horse and jockey IDs from the NYRA website, finishing placements for the horses, and the track outlines that we obtained through manual data entry from Google Earth.

Run `data_preparation_pipeline.R` to obtain the data used in our model and visualizations.

### Code

Our code is broken down into four sections:

- Data Preparation: Required data preparation for our modelling and visualization.

- Stan Models: The scripts to fit our Bayesian velocity models and run model simulations with Stan.

- Model Pipeline: The scripts to run these models directly from R.

- Data Visualization: A pipeline to create the visuals for our writeup


### Figures

A folder containing the visuals from our writeup.
