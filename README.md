# MEII
US Census Tract Multi-Exposure Environmental Index (MEEI)

## Section 0: Introduction

Physical environment plays a key role in determining human health risks. Exposure to toxins, weather extremes, degraded air and water quality, high levels of noise and limited accessibility to green areas can negatively affect health. Furthermore, adverse environmental exposures are often correlated with each other and with socioeconomic status, thereby compounding disadvantages in marginalized populations. 
Moreover, despite their importance in determining human health risks,  the role of multiple environmental exposures is not well studied, and only a few resources contain aggregate environmental exposure data and only for selected areas of the contiguous US. 
To fill these gaps, we took a cumulative approach to measuring the environment by generating a composite Multi-Exposure Environmental Index (MEEI) as a US Census Tract-level summary of key environmental factors with known health effects. This measure quantifies multiple environmental exposures in the same area that can result in additive and synergistic effects on health outcomes. This information is crucial to better understand and possibly leverage environmental determinants of health for informed policy-making and intervention.


## Section 1: Data

The key environmental factors with known health effects used to generate the multi-exposure index are available here: MEII_data.csv

```
# Environmental exposures factors
df <- read.csv("MEII_data.csv")
df$GEOID = as.character(as.numeric(df$GEOID))
head(df)
```

The 2019 Cartographic Boundary Shapefile, Current Census Tract for United States, 1:500,000 is available here:
[cb_2019_us_tract_500k.zip](https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_tract_500k.zip)

```
# US Censust tract geography
tracts  <- st_read('cb_2019_us_tract_500k/cb_2019_us_tract_500k.shp')
tracts$GEOID = as.character(as.numeric(tracts$GEOID))
head(tracts)
```



```
# Combined data
left_join(tracts, df, by = 'GEOID') -> MapData 
projected = st_transform(MapData, 3085)
projected = projected[!(projected$ST %in% c("02", "15", "60","66","69","72","78")),]
```


## Section 2: Maps of environmental exposures factors


<p align="center">
<img width="500" src="./circle_plot.png">
</p>



<figure class="half">
  <table>
    <tr>
      <td>
        <img style="width:500px;" src="./coldw.png">
      </td>
      <td>
        <img style="width:500px;" src="./hotw.png">
      </td>
    </tr>
  </table>
</figure>

<figure class="half">
  <table>
    <tr>
      <td>
        <img style="width:500px;" src="./hprec.png">
      </td>
      <td>
        <img style="width:500px;" src="./noise.png">
      </td>
    </tr>
  </table>
</figure>

<figure class="half">
  <table>
    <tr>
      <td>
        <img style="width:500px;" src="./pm25.png">
      </td>
      <td>
        <img style="width:500px;" src="./ozone.png">
      </td>
    </tr>
  </table>
</figure>

<figure class="half">
  <table>
    <tr>
      <td>
        <img style="width:500px;" src="./radon.png">
      </td>
      <td>
        <img style="width:500px;" src="./TRI.png">
      </td>
    </tr>
  </table>
</figure>



<figure class="half">
  <table>
    <tr>
      <td>
        <img style="width:500px;" src="./uv.png">
      </td>
       <td>
        <img style="width:500px;" src="./greenaopens.png">
      </td>
    </tr>
  </table>
</figure>


## Section 3: Multi-Exposure Environmental Index (MEEI)


<p align="center">
<img width="800" src="./MEII.png">
</p>

<p align="center">
<img width="800" src="./MEII_box.png">
</p>

