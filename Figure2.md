**Figure2A**

```{r}
library(tidyverse)
library(vegan)
```
```{r}
data2a <- read_csv("Source_Data_Figure2.csv")
metadata1 <- read_csv("Source_Data_Figure2_metadata.csv")
```

Invert the dataframe so that each row is a sample and each column is a genus. 
```{r}
flip <- data2a %>% 
  pivot_longer(2:1109, names_to = "Sample", values_to = "count") %>% 
  pivot_wider(names_from = ID, values_from = count)
```

```{r}
genus <- flip %>% 
  select(2:1208)
```

The next step is to take the data frame and turn it into a distance matrix using Bray-Curtis distance
```{r}
data.t <- as.matrix(vegdist(genus, method = 'bray'))
```
PERMANOVA (Permutational multivariate analysis of variance) by wetland type
```{r}

mer_perm<-adonis(data.t ~ Type, data=metadata, permutations = 999)
mer_perm
```

Visualize community similarity using NMDS (non-metric multidimensional scaling)
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

**Figure S4**
After running the NMDS use the code below to select significant enviornmental variables and overlay the vectors onto the NMDS
Data is From Source Data Filesand can be filtred to only include the relavant metadata
```{r}
envfit.data <- read.delim("Figure2a_metadata.txt") # text file of enviornmental variables
envfit_perm <- envfit(NMDS, envfit.data, permutations = 999) # this fits environmental vectors
```

```{r}
env.scores <- as.data.frame(scores(envfit_perm, display = "vectors")) #extracts relevant scores from envifit
env.scores <- cbind(env.scores, env.variables = rownames(env.scores)) #and then gives them their names

env.scores <- cbind(env.scores, pval = envfit_perm$vectors$pvals) # add pvalues to dataframe
sig.env.scrs <- subset(env.scores, pval<=0.05) #subset data to show variables significant at 0.05
```


```{r}
plot_env <- NMDS_plot +
  geom_segment(data = sig.env.scrs, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant env variables
  ggrepel::geom_text_repel(data = sig.env.scrs, aes(x=NMDS1, y=NMDS2, label = env.variables), cex = 4, direction = "both", segment.size = 0.25)#+ #add labels for env variables
  #labs(title="Ordination with environmental vectors")
plot_env
```
