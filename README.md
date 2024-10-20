# Geography 418 Lab 3 Tutorial - Sam Whitten

---
title: "Spatial Autocorrelation Analysis of Household Income and French Language Knowledge in Cape Breton Using R"
date: "2024-10-08"
output: pdf_document
---
#output: 

#    number_sections: false
#    fig_caption: true
#    global_numbering: true 


## Introduction

Introduction
Welcome to this tutorial on conducting spatial autocorrelation analysis using R. Spatial autocorrelation is an essential concept in spatial analysis. It investigates whether spatial patterns—such as household income levels or language proficiency—are randomly distributed or show signs of clustering. By understanding and identifying spatial autocorrelation, geographers and spatial analysts can gauge how similar or different geographic areas are compared to their neighbours. The data used in this analysis is sourced from the Canadian Census, focusing specifically on household income and French language knowledge for the Cape Breton region.

In this tutorial, you will learn to analyze spatial autocorrelation in household income data from the Canadian Census, concentrating on Cape Breton as our area of focus. We will utilize various R tools and libraries to perform global and local spatial autocorrelation tests and visualize the results for deeper insights into spatial patterns. This guide is aimed at university students and researchers new to spatial statistics or R programming.

Objectives
We will cover the following key areas:
1.	Data Preparation and Cleaning: Loading the data, cleaning it, and ensuring the geographic and census data are correctly merged.
2.	Exploring Spatial Patterns: Visualizing income and French knowledge distributions across Cape Breton.
3.	Global and Local Spatial Autocorrelation Analysis: Applying Moran's I and Local Indicators of Spatial Association (LISA) to detect clustering patterns.
4.	Interpreting the Results: Understanding what the results mean for the spatial distribution of household income and French language knowledge.
This document will walk you through each step, including the required R code, explanations of the concepts, and visual outputs to help solidify your understanding.

Concept of Libraries
To begin with, we need to load the appropriate R packages for the analysis. R packages, often called libraries, are collections of functions and datasets developed by the R community. They help perform specialized tasks more efficiently, especially for spatial data.
The following libraries are required for this tutorial:
•	sf: For reading and processing spatial data.
•	tmap: For visualizing spatial data.
•	spdep: For calculating spatial autocorrelation metrics like Moran's I.
•	raster, shinyjs, e1071: For additional geospatial analysis, user interface tools, and calculating descriptive statistics respectively.
We begin by installing and loading these libraries:
The installation step ensures that all necessary libraries are available while the library() calls load them in this tutorial.




```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
#Install packages if not already installed:


library("knitr")
library("sf")
library("tmap")
library("spdep")
library("raster")
library("shinyjs")
library("e1071")


#Load in libraries:


```

The installation step ensures that all necessary libraries are available, while the library() calls load them for use in this tutorial.

Data Preparation and Cleaning
The dataset used in this tutorial contains household income data and census information for the Cape Breton region. We will start by reading the shapefile containing geographic boundaries and the CSV file containing census variables. After merging these datasets, we will filter the data to include only relevant columns and focus on the target city, Cape Breton.

Data preparation involves renaming columns, cleaning unnecessary rows, and calculating any new variables required for our analysis. For example, we convert French language knowledge into a percentage for easier interpretation and mapping. This step-by-step process ensures the data is well organized for our subsequent analyses.



```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}

csv <- read.csv("/Users/swhitten/Downloads/Assignment3_Data/ucgsJQnBVLvP_data.csv") 

shp <- st_read("/Users/swhitten/Downloads/Assignment3_Data/lda_000a16a_e.shp")


```

```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}

#New column names
cols <- c("GEO UID", "Province code", "Province name", "CD code",
          "CD name", "DA name", "Population", "Land area", 
          "Median total income", "Income Sample Size", "French Knowledge", 
          "Language Sample Size")


# Apply correct column names to the dataframe
colnames(csv) <- cols  # If you already have updated names for the columns

# Add a column to count the number of characters in GEO UID
csv$len <- nchar(as.character(csv$`GEO UID`))

# Remove rows where GEO UID has fewer than 8 characters
csv_clean <- subset(csv, csv$len == 8)

# Merge the spatial and aspatial data using the correct column names
census_DAs <- merge(shp, csv_clean, by.x = "DAUID", by.y = "GEO UID", all.x = TRUE)

# Subset for the city of Kamloops (adjust this based on your data structure)
Municp <- subset(census_DAs, census_DAs$CMANAME == "Cape Breton")

# Convert French Knowledge to a rate
Municp$PercFrench <- (Municp$`French Knowledge` / Municp$`Language Sample Size`) * 100




```

This step-by-step process ensures that the data is well-organized for our subsequent analyses.

Including Plots
To better understand the data, we create visual representations of the median total income and the percentage of French knowledge in Cape Breton.
We use the tmap package to visualize the spatial patterns:
These maps provide a visualization of the median total income and percentage of French knowledge across the Cape Breton region. They help us understand whether areas with higher income or French language knowledge tend to cluster together spatially.


```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
# Remove rows with NA values for Median total income and French Knowledge
Income_noNA <- Municp[which(!is.na(Municp$`Median total income`)),]
French_noNA <- Municp[which(!is.na(Municp$`French Knowledge`)),]

```

We use the tmap package to visualize the spatial patterns:




```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
# Calculate descriptive stats for Income
meanIncome <- mean(Municp$`Median total income`, na.rm = TRUE)
stdevIncome <- sd(Municp$`Median total income`, na.rm = TRUE)
skewIncome <- e1071::skewness(Municp$`Median total income`, na.rm = TRUE)

# Calculate descriptive stats for French Knowledge
meanFrench <- mean(Municp$PercFrench, na.rm = TRUE)
stdevFrench <- sd(Municp$PercFrench, na.rm = TRUE)
skewFrench <- e1071::skewness(Municp$PercFrench, na.rm = TRUE)

# Create a dataframe to display the statistics
data <- data.frame(
  Variable = c("Income", "French Language"),
  Mean = c(round(meanIncome, 2), round(meanFrench, 2)),
  StandardDeviation = c(round(stdevIncome, 2), round(stdevFrench, 2)),
  Skewness = c(round(skewIncome, 2), round(skewFrench, 2))
)

# Display the table
knitr::kable(data, caption = "Descriptive statistics for selected census variables")
```

We create maps with the tmap package, known for its strength in visualizing spatial data in R. The tm_shape() function defines geographic boundaries, specifically the Cape Breton census dissemination areas, while tm_polygons() visualizes certain attributes such as median total income and French language proficiency. Various colours indicate different values within regions, and we utilize the tm_layout() function to refine legends, titles, and layout for improved readability. Displaying multiple maps side by side allows for comparative visual insights, highlighting differences in the spatial distribution of these attributes.

```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Sydney census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)."}
# Adjust the maps and layout
map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median Total Income (CAD)", 
              style = "jenks", 
              palette = "BuGn", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("right", "top"),
            title.size = 1.8,
            legend.title.size = 1.5,
            legend.text.size = 0.8,
            legend.frame = FALSE,
            outer.margins = c(0.05, 0.05, 0.05, 0.05))  # Add margins for better spacing

map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "French Knowledge (%)", 
              style = "jenks", 
              palette = "BuGn", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("right", "top"),
            title.size = 1.8,
            legend.title.size = 1.5,
            legend.text.size = 0.8,
            legend.frame = FALSE,
            outer.margins = c(0.05, 0.05, 0.05, 0.05))  # Add margins for better spacing

# Print maps side by side
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```
<img width="697" alt="Screenshot 2024-10-20 at 10 18 17 AM" src="https://github.com/user-attachments/assets/d4be29a0-fb97-4468-8e2a-a35505ce912a">


These maps provide a visualization of the median total income and percentage of French knowledge across the Cape Breton region. They help us understand whether areas with higher income or higher French language knowledge tend to cluster together spatially.


Global Spatial Autocorrelation with Moran's I
Moran's I is a global measure of spatial autocorrelation, which tests whether similar values are clustered across the entire study area. A positive Moran's I value indicates clustering of similar values (either high or low), while a negative value suggests a checkerboard pattern where neighbouring areas differ. A value near zero indicates a random spatial distribution.
Here's how we perform and interpret the Moran's I analysis for median total income and French knowledge:
A statistically significant positive value of Moran's is found in the analysis. I would suggest that high-income areas cluster together in Cape Breton. In contrast, negative values might indicate that high-income regions are following to low-income areas. Similarly, Moran's I for French knowledge would help identify clustering patterns for knowledge of French.


Here's how we perform and interpret the Moran's I analysis for median total income and French knowledge:

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}

# For Queen's contiguity-based neighbors on Income data
Income.nb <- poly2nb(Income_noNA)  # Create neighbors list
Income_centroids <- st_centroid(Income_noNA)
Income.net <- nb2lines(Income.nb, coords = st_coordinates(Income_centroids))

# For Rook's contiguity-based neighbors on Income data
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords = st_coordinates(Income_centroids))

# Set CRS for neighbors' networks (using sp)
proj4string(Income.net) <- CRS(st_crs(Income_noNA)$proj4string)
proj4string(Income.net2) <- CRS(st_crs(Income_noNA)$proj4string)

# For Queen's contiguity-based neighbors on French data
French.nb <- poly2nb(French_noNA)
French_centroids <- st_centroid(French_noNA)
French.net <- nb2lines(French.nb, coords = st_coordinates(French_centroids))

# For Rook's contiguity-based neighbors on French data
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords = st_coordinates(French_centroids))

# Set CRS for French networks
proj4string(French.net) <- CRS(st_crs(French_noNA)$proj4string)
proj4string(French.net2) <- CRS(st_crs(French_noNA)$proj4string)


# Create weights matrix for Income data (Queen's neighbors)
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

# Calculate Moran's I for Income
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

# Extract Moran's I results
mIIncome <- miIncome$estimate[[1]]

```

<img width="697" alt="Screenshot 2024-10-20 at 10 18 26 AM" src="https://github.com/user-attachments/assets/62ec160b-95de-4c3d-ae4b-9ec5d5e5c4be">


In the analysis, a statistically significant positive value of Moran's I would suggest that high-income areas cluster together in Cape Breton, while negative values might indicate that high-income areas are located next to low-income areas. Similarly, the Moran's I for French knowledge would help identify clustering patterns for knowledge of French.

Visual Interpretation of Moran's I
Moran's I scatter plots visually represent spatial autocorrelation, showing the relationship between the values at a location and the spatial lag of those values. The regression line in the plot indicates the overall trend, where a positive slope shows positive spatial autocorrelation.


```{r Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Cape Breton census dissemination areas showing median total income neighbours queens weight (left)  rooks weight (middle) and the combination of the two (right)."}

# Get the bounding box of the valid geometries
bbox <- st_bbox(Income_noNA)

# Make queens map
IncomeQueen <- tm_shape(Income_noNA, bbox = bbox) + tm_borders(col='lightgrey') + 
  tm_shape(Income.net) + tm_lines(col='red')

# Make rooks map
IncomeRook <- tm_shape(Income_noNA, bbox = bbox) + tm_borders(col='lightgrey') + 
  tm_shape(Income.net2) + tm_lines(col='black', lwd = 2)

# Make combined map
IncomeBoth <- tm_shape(Income_noNA, bbox = bbox) + tm_borders(col='lightgrey') + 
  tm_shape(Income.net) + tm_lines(col='red', lwd = 2) +
  tm_shape(Income.net2) + tm_lines(col='black', lwd = 2)

# Print maps in a three pane figure
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)

```


The weighted matrix plays a vital role in spatial analysis, as it illustrates the spatial relationships among various geographic regions. It indicates which regions are neighbours and the strength of their influence on one another, which is crucial for evaluating spatial autocorrelation. In the tutorial, we initiate the process by generating a list of neighbours for each spatial polygon using the poly2nb() function from the spdep package. This function examines shared boundaries between polygons to determine neighbouring relationships. We employ both Queen’s contiguity and Rook’s contiguity for this purpose. Queen’s contiguity accounts for polygons sharing either a boundary or a point, while Rook’s contiguity only considers those sharing a standard edge.

Once the neighbour lists are created, we convert them into a weighted matrix with the nb2listw() function. This function takes the neighbour relationships and assigns weights, crucial for later spatial autocorrelation calculations. For instance, by applying row-standardized weights (style = "W"), each neighbour receives an equal weight that sums to 1 for each polygon. Consequently, each area equally influences its neighbours, though the distribution of that influence varies depending on the number of neighbours present. We also include the parameter zero.policy = TRUE to manage instances where a polygon lacks neighbours.

This weighted matrix is then utilized in calculating Moran’s I, which measures the similarity or differences between each polygon and its neighbours. By defining spatial weights, we ensure that the calculations reflect the degree of relatedness among geographic areas. Thus, the weighted matrix is crucial for grasping the spatial dynamics involved, whether we analyze income levels or other socio-economic indicators.




```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}
# Create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

# Create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

# Access the weights (correct way)
head(Income.lw$weights)[1:3]  # Accessing the first 3 weight vectors

```



$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$



```{r Global Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate Global Moran's I for Income
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

#Extract Global Moran's I results for Income
mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]

#Calculate Global Moran's I for French
miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)

#Extract Global Moran's I results for French
mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]
```



```{r Global Morans Range, echo=TRUE, eval=TRUE, warning=FALSE}
#Function to calculate the range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

#Calculate the range for the Income variable
range <- moran.range(Income.lw)
minRange <- range[1]
maxRange <- range[2]
```



```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))
```




$$
I_i = \frac{x_i - \bar{x}}{S_i^2}\sum{_{j=1}^n}W_{i,j}(x_j - \bar{x})\space \space where \space \space S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1} 
$$

In this context, let $x$ be the assessed variable, where $x_i$ denotes the variable value at a specific point of interest (i) and $x_j$ signifies a neighbor of $x_i$, determined by the queen weighting scheme. The spatial weighting applied through the matrix $W_{i,j}$ is multiplied by the differences between $x_i$ and the mean value of variable $x$, and between $x_j$ and the mean value of variable $x$.

The denominator in this scenario standardizes our values, indicating that higher values of I are associated with positive spatial autocorrelation, while lower values of I indicate negative spatial autocorrelation. It is important to note that the global Moran’s I statistic shows how spatially autocorrelated the data is across the entire dataset, thereby illustrating a spatial pattern on a global scale.


```{r Local Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate LISA test for Income
lisa.testIncome <- localmoran(Income_noNA$`Median total income`, Income.lw)

#Extract LISA test results for Income
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#Calculate LISA test for Income
lisa.testFrench <- localmoran(French_noNA$PercFrench, French.lw)

#Extract LISA test results for Income
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```



The Local Indicators of Spatial Association (LISA) analysis reveals clusters and spatial outliers related to median total income and French language proficiency in Cape Breton. Utilizing the localmoran() function, it computes the local Moran's I statistic (Ii), which illustrates the degree of spatial clustering in each region. Additional metrics, including the expected value (E.Ii), variance (Var.Ii), Z-score (Z.Ii), and p-value (P), provide statistical support for these clusters, aiding in the assessment of the significance of observed patterns.

This localized approach offers in-depth insights into income and language knowledge distribution, highlighting areas of high or low values while identifying notable outliers. It fosters a deeper understanding of socio-economic trends in Cape Breton, which is crucial for effective policy-making and targeted interventions.



```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Cape Breton census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondants with knowledge of french (right)."}
#Map LISA z-scores for Income
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(Income_noNA$Z.Ii),-1.96,1.96,max(Income_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Map LISA z-scores for French
map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(French_noNA$Z.Ii),-1.96,1.96,max(French_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Plot maps in a 2 pane figure
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)
```


<img width="694" alt="Screenshot 2024-10-20 at 10 17 43 AM" src="https://github.com/user-attachments/assets/4db7630a-0989-4460-a4e0-2fa801bd6bf3">



Local Indicators of Spatial Association (LISA)
Local Moran's I, or LISA, helps locally identify clusters of similar values and spatial outliers. LISA lets us pinpoint areas within Cape Breton where high or low values are clustered or anomalies occur.
These LISA maps allow us to visualize which areas of Cape Breton have high or low income and French knowledge values and whether similar or dissimilar regions surround them.



```{r MoransIScatter, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for median total income."}
#Create Moran's I scatter plot for Income
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL)
```

<img width="752" alt="Screenshot 2024-10-20 at 10 18 50 AM" src="https://github.com/user-attachments/assets/ae9a8154-3c71-4d11-b692-bdbe894a802a">



```{r MoransIScatter2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for percentage of respondants with knowledge of french."}
#Create Moran's I scatter plot for French
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL)
```

<img width="675" alt="Screenshot 2024-10-20 at 10 18 56 AM" src="https://github.com/user-attachments/assets/fec1d019-9d5b-45f1-b61b-a9260d043337">


Interpreting the Results

From the Moran's I and LISA analyses, we can draw conclusions about spatial autocorrelation in Cape Breton:

Global Moran's I: The results from Global Moran's I provide insights into whether spatial patterns of household income or French knowledge are clustered across the entire region. A positive and statistically significant Moran's I value indicates clustering, while a negative value suggests a pattern of dissimilarity between neighbors.

LISA: The local Moran's I values help identify areas that contribute most to the global spatial autocorrelation. Hotspots (high values surrounded by high values) and cold spots (low values surrounded by low values) indicate strong local clustering, while outliers (high surrounded by low or vice versa) highlight areas that behave differently from their neighbors.

## Summary

In this tutorial, we examined the spatial distribution of median total income and French knowledge in Cape Breton using spatial autocorrelation techniques. We used Moran's I to assess global clustering patterns and LISA to identify local clusters and spatial outliers.

The analysis revealed that there are areas in Cape Breton where high-income households tend to cluster, and the same is true for knowledge of the French language. By identifying these clusters, we gain valuable insights into the socio-economic landscape of Cape Breton, which can help in urban planning and policy-making to target specific regions more effectively.






## References

•	Anselin, L. (1995). Local Indicators of Spatial Association—LISA. Geographical Analysis, 27(2), 93-115.
•	Bivand, R., Pebesma, E., & Gomez-Rubio, V. (2013). Applied Spatial Data Analysis with R. Springer.
•	Moran, P. A. P. (1950). Notes on Continuous Stochastic Phenomena. Biometrika, 37(1/2), 17-23.
•         Auclair, N., Frigon, C., & St-Amant, G. (2023, March 16). Key facts on the French language in Nova Scotia in 2021. Statistics Canada. https://www150.statcan.gc.ca/n1/pub/89-657-x/89-657-x2023003-eng.htm

 





