**Figure2A**

```{r}
library(tidyverse)
library(vegan)
```
```{r}
data2a <- read_csv("Source_Data_Figure2.csv")
metadata1 <- read_csv("Source_Data_Figure2_metadata.csv")
```


```{r}
flip <- data2a %>% 
  pivot_longer(2:1109, names_to = "Sample", values_to = "count") %>% 
  pivot_wider(names_from = ID, values_from = count)
```

```{r}
genus <- flip %>% 
  select(2:1208)
```

create distance matrix
```{r}
data.t <- as.matrix(vegdist(genus, method = 'bray'))
```

```{r}

mer_perm<-adonis(data.t ~ Depth*Type, data=metadata, permutations = 999)
mer_perm
```
```{r}
NMDS<-metaMDS(data.t, trymax= 200,k=4)

```

```{r}

MDS1 = NMDS$points[,1]
MDS2 = NMDS$points[,2]
NMDS = data.frame(NMDS1 = MDS1, NMDS2 = MDS2)
```



```{r}
metadata$Site <- factor(metadata$Site, levels = c("OWC", "P7", "P8", "Twitchell", "LA2", "JLA", "STM-fen", "STM-bog", "SPRUCE"))
plot2A <- ggplot(NMDS) +
  geom_point(aes(x=NMDS1, y=NMDS2, col=metadata$Site,  fill=metadata$Site, shape=metadata$Type), size=4) + 
  theme_bw() +
  labs(shape = "Sample Type", col= "Site")+
  scale_fill_manual(values=c("#003c30", "#01665e", "#35978f", "#80cdc1", "#c7eae5", "#f6e8c3", "#dfc27d", "#bf812d", "#8c510a"), guide = "none") +
  scale_colour_manual(values=c("#003c30", "#01665e", "#35978f", "#80cdc1", "#c7eae5", "#f6e8c3", "#dfc27d", "#bf812d", "#8c510a")) +
  scale_shape_manual(values=c(24, 21, 22, 23)) + 
   theme(text=element_text(size=21))
plot2A
```
