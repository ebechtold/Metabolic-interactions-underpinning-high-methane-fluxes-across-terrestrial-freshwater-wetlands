**Figure S3A Shannon Diversity Index**

```{r}
library(plyr)
library(tidyverse)
library(vegan)
library(ggpubr)
```

Read in data (Data is from Source Data file in the paper)
```{r}
data <- read_csv("Figure5sA.csv")
flux <- read_csv("Figure5A.csv")
```



```{r}
data_flip <- data %>% 
  pivot_longer(2:1119, names_to = "sample", values_to="count") %>% 
  pivot_wider(names_from = "ID", values_from = "count")

```

Calculate Shannon Diversity Index
```{r}

new <- ddply(data_flip,~sample,function(x) {
         data.frame(all=diversity(x[-1], index="shannon"))
 })

```

```{r}
test <- new %>% 
  separate(sample, into = c("Site", "Depth", "group"), sep="_")
```
```{r}

test$Site <- factor(test$Site, levels = c("OWC", "P7", "P8", "Twitchell", "LA2", "JLA", "STM-fen", "STM-bog", "SPRUCE"))
plot <- ggplot(test, aes(x=Site, y=all, fill=Site)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.25, height = 0.25, shape = 21) +
  #facet_grid(Type~site, scale="free_x", space = "free_x") +
  scale_fill_manual(values=c("#003c30", "#01665e", "#35978f", "#80cdc1", "#c7eae5", "#f6e8c3", "#dfc27d", "#bf812d", "#8c510a"))+
  scale_color_manual(values=c("#003c30", "#01665e", "#35978f", "#80cdc1", "#c7eae5", "#f6e8c3", "#dfc27d", "#bf812d", "#8c510a"))+
  xlab("Sample Depth") +
  ylab("Shannon Diversity Index")+
  theme_bw()
plot
```

Merge Shannon Data with Flux Data
```{r}
library(tidyverse)
join <- test %>% 
  left_join(flux)
  
```
To test for normality we use a Shapiro-Wilkes test which uses the ANOVA output
```{r}
anova <- aov(all~Type, data = join)

shapiro.test(anova$residuals)
```
Shapiro-Wilkes test showed data was not normally distributed so we use a Kruskal-Wallis test followed by a pairwise wilcox test for the posthoc anlaysis with p-value correction
```{r}
kw_test <- kruskal.test(all~Type, data = join)
kw_test

pairwise.wilcox.test(join$all, join$Type,
                 p.adjust.method = "BH")
```

Calucalte the average Shannon Diversity for each site
```{r}
detach(package:plyr)
filtered <- join %>% 
  group_by(Site, Flux, Residual, Type) %>% 
  summarize(ave = mean(all)) 
```


```{r}
filtered$Site <- factor(filtered$Site, levels = c("OWC", "P7", "P8", "Twitchell", "LA2", "JLA", "STM-fen", "STM-bog", "SPRUCE"))
plot1 <- ggplot(filtered, aes(x=ave, y=Flux)) +
  geom_point(aes(color=Site, shape=Type, fill=Site, size=3))+
    geom_smooth(method=lm, se=FALSE, colour="black") +
  #stat_regline_equation(label.y = 2, aes(label = ..eq.label..)) +
  stat_cor(label.y = 0,label.x = 3.25)+
  stat_regline_equation(label.y = 0.25,label.x = 3.25, aes(label = ..rr.label..)) +
  xlab("Shannon Diversity Index") +
  ylab(expression(Annual~CH[4]~ Flux ~(g~C~m^-2~yr^-1)))+
  theme_classic()+ 
  scale_shape_manual(values=c(24, 21, 22, 25)) +
  scale_color_manual(values=c("#003c30", "#01665e", "#35978f", "#80cdc1", "#c7eae5", "#f6e8c3", "#dfc27d", "#bf812d", "#8c510a")) +
  scale_fill_manual(values=c("#003c30", "#01665e", "#35978f", "#80cdc1", "#c7eae5", "#f6e8c3", "#dfc27d", "#bf812d", "#8c510a")) + 
  guides(size = FALSE) 

plotS3B = plot1+scale_y_log10(limits=c(1, 700))
plotS3B
```
