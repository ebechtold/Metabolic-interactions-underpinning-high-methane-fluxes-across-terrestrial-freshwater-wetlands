
**Figure 1A**

```
library(tidyverse)
library(scales)
library(ggpubr)
```

Import Source Data File
```
data1a <- read_csv("Source_Data1A.csv")
```
Make Sure temperature is a numerical variable
```{r}
data1a$temp <- as.numeric(data1a$temp)
```

Select only samples that are freshwater wetlands
```{r}
data1 <- data1a %>% 
  filter(Select=="Yes") 
  
```

Plot the data with R^2 determined using linear regression
```{r}
plot1 <- ggplot(data1, aes(x=temp, y=methane)) +
  geom_point(aes(shape=Type, color=Color, size=3))+
    geom_smooth(method=lm, se=FALSE, colour="black") +
  geom_errorbar(aes(ymin=methane-methane_sd, ymax=methane+methane_sd))+
  #stat_regline_equation(label.y = 2, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 2, aes(label = ..rr.label..)) +
  xlab("Mean Annual Temperature(C)") +
  ylab(expression(Annual~CH[4]~ Flux ~(g~C~m^-2~yr^-1)))+
  theme_classic()+ 
  scale_x_continuous(breaks=c(-10, -5, 0, 5, 10, 15, 20, 25, 30))+
  scale_shape_manual(values=c( 2, 0, 1, 6, 5))+ 
  scale_color_manual(values=c("grey", "#f6e8c3", "#c7eae5", "#003c30", "#01665e", "#35978f", "#543005", "#8c510a", "#bf812d", "#80cdc1"))
plot1

```

To convert y-axis to logscale
```
plot1A = plot1+scale_y_log10(limits=c(1, 700))
plot1A 
```

Figure S2 use data from Source Code Figure1a

 Figure S5A uses and Figure S7 use data from Source Code S7.

**Figure 1C**
```{r}
library(tidyverse)
library(circlize)
library(UpSetR)
library(ComplexHeatmap)
```

```{r}
map = read.delim(file = 
          'Source_Code1C.txt',
          row.names = 1)
```

```{r}
map = map %>%
  mutate(Sample = rownames(map)) 
```

```{r}


s=make_comb_mat(map)
s


UpSet(s, set_order = c("Marsh", "Bog", "Fen", "February", "March", "May", "June", "July", "August", "September", "October", "D1", "D2", "D3", "D5"),  comb_col=c("black"))
```
*final figure presentation was improved in illustrator
