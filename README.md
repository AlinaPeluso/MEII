# MEII
US Census Tract Multi-Exposure Environmental Index (MEEI)

## Section 0: Introduction

Physical environment plays a key role in determining human health risks. Exposure to toxins, weather extremes, degraded air and water quality, high levels of noise and limited accessibility to green areas can negatively affect health. Furthermore, adverse environmental exposures are often correlated with each other and with socioeconomic status, thereby compounding disadvantages in marginalized populations. 
Moreover, despite their importance in determining human health risks,  the role of multiple environmental exposures is not well studied, and only a few resources contain aggregate environmental exposure data and only for selected areas of the contiguous US. To fill these gaps, we took a cumulative approach to measuring the environment by generating a composite Multi-Exposure Environmental Index (MEEI) as a US Census Tract-level summary of key environmental factors with known health effects. This measure quantifies multiple environmental exposures in the same area that can result in additive and synergistic effects on health outcomes. This information is crucial to better understand and possibly leverage environmental determinants of health for informed policy-making and intervention.


## Section 1: Data

Description, source, and link details about the data sources acquired as key environmental factors of the multi-exposure index.
<p align="center">
<img width="800" src="./data_sources_MEII.png">
</p>



The key environmental factors with known health effects used to generate the multi-exposure index are available here: 
MEII_data.csv

```{r}
# Environmental exposures factors
df <- read.csv("MEII_data.csv")
df$GEOID = as.character(as.numeric(df$GEOID))
head(df)
```

The 2019 Cartographic Boundary Shapefile, Current Census Tract for United States, 1:500,000 is available here:
[cb_2019_us_tract_500k.zip](https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_tract_500k.zip)

```{r}
# US Censust tract geography
tracts  <- st_read('cb_2019_us_tract_500k/cb_2019_us_tract_500k.shp')
tracts$GEOID = as.character(as.numeric(tracts$GEOID))
head(tracts)
```



```{r}
# Combined data
left_join(tracts, df, by = 'GEOID') -> MapData 
projected = st_transform(MapData, 3085)
projected = projected[!(projected$ST %in% c("02", "15", "60","66","69","72","78")),]
```


## Section 2: Maps of environmental exposures factors

Selected environmental factors used to compute MEEI:

<p align="center">
<img width="400" src="./circle_plot.png">
</p>

Spatial distribution of the selected environmental factors used to compute MEEI:

<p align="center">
<figure class="half">
  <table>
    <tr>
      <td>
        <img style="width:300px;" src="./coldw.png">
      </td>
      <td>
        <img style="width:300px;" src="./hotw.png">
      </td>
    </tr>
  </table>
</figure>

<p align="center">
<figure class="half">
  <table>
    <tr>
      <td>
        <img style="width:300px;" src="./hprec.png">
      </td>
      <td>
        <img style="width:300px;" src="./noise.png">
      </td>
    </tr>
  </table>
</figure>

<p align="center">
<figure class="half">
  <table>
    <tr>
      <td>
        <img style="width:300px;" src="./pm25.png">
      </td>
      <td>
        <img style="width:300px;" src="./ozone.png">
      </td>
    </tr>
  </table>
</figure>

<p align="center">
<figure class="half">
  <table>
    <tr>
      <td>
        <img style="width:300px;" src="./radon.png">
      </td>
      <td>
        <img style="width:300px;" src="./TRI.png">
      </td>
    </tr>
  </table>
</figure>

<p align="center">
<figure class="half">
  <table>
    <tr>
      <td>
        <img style="width:300px;" src="./uv.png">
      </td>
       <td>
        <img style="width:300px;" src="./greenaopens.png">
      </td>
    </tr>
  </table>
</figure>


## Section 3: Multi-Exposure Environmental Index (MEEI)


We computed a composite Multi-Exposure Environmental Index (MEEI) that can be used to characterize CTs for multiple environmental factors.  
This index reflects the number of environmental factors in each CT and their detrimental or beneficial levels.
The index is then ranked to distinguish areas with greater or lesser environmental deprivation.
Health-relevant levels are calculated based on the distribution of values for each factor. This enables us to identify CTs in the top 35th exposure percentile and assign a score of +1 for detrimental dimensions or -1 for beneficial dimensions. Here is an example illustrating the calculation of the index for two specific tracts.


<p align="center">
<img width="300" src="./example_MEII.png">
</p>


```{r}
X <- projected[,c("heavyprec","heatw","coldw","tri","greenaopens",
                  "pm25","ozone","uv","noisemax","radcont")]

topQ <- apply(X,2,function(x) quantile(as.numeric(unlist(x)),0.65,na.rm=T))

# detrimental (+1): 
heavyprec <- ifelse(projected$heavyprec > topQ["heavyprec"],1,0)
coldw <- ifelse(projected$coldw > topQ["coldw"],1,0)      
heatw <- ifelse(projected$heatw > topQ["heatw"],1,0)   
ozone <- ifelse(projected$ozone > topQ["ozone"],1,0)
pm25 <- ifelse(projected$pm25 > topQ["pm25"],1,0)
radcont <- ifelse(projected$radcont > topQ["radcont"],1,0)
tri <- ifelse(projected$tri > topQ["tri"],1,0)
noisemax <- ifelse(projected$noisemax > topQ["noisemax"],1,0)
# beneficial (-1): 
greenaopens <- ifelse(projected$greenaopens > topQ["greenaopens"],-1,0)
uv <- ifelse(projected$uv > topQ["uv"],-1,0)

trvars <- cbind(
  # detrimental (+1): 
  heavyprec,heatw,coldw,tri,
  pm25,ozone,noisemax,radcont,
  # beneficial (-1): 
  greenaopens,uv)

projected$MEII <- factor(rowSums(trvars, na.rm = T), 
                  levels = range(as.numeric(rowSums(trvars, na.rm = T)),na.rm=T)[1]:range(as.numeric(rowSums(trvars, na.rm = T)),na.rm=T)[2])
```

Spatial distribution of the MEEI:

<p align="center">
<img width="800" src="./MEII.png">
</p>


```{r}
table(projected$MEII)
round(table(projected$MEII)/72333*100,2)
```

| MEII class |    -2 |    -1 |     0 |     1  |    2   |   3   |   4   |   5   |   6   |    7 |
| ---------- | ------------------------------------------------------------------------------ |
| N CTs      |    64 | 1,829 | 8,529 | 15,874 | 18,568 |14,932 | 8,365 | 3,298 |  770  | 104  |
| %          |  0.09 |  2.53 | 11.79 |  21.95 |  25.67 | 20.64 | 11.56 |  4.56 | 1.06  | 0.14 |




<p align="center">
<img width="800" src="./MEII_box.png">
</p>

