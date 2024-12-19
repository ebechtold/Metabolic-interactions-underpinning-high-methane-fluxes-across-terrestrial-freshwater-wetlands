
**Figure 5B**

ggtreeEXTRA manual: https://yulab-smu.top/treedata-book/chapter10.html

```
library(ape)
library(ggplot2)
library(ggtree)
library(ggtreeExtra)
library(readxl)
library(dplyr)
library(tidyr)
library(ggnewscale)
library(cowplot)
library(TDbook)
library(treeio)
```

All files can be found on Zenodo: DOI 10.5281/zenodo.14532347


Read in tree file
```
arc_tree = read.tree("gtdbtk.ar53.decorated.tree")
arc_tree
```

Read in taxonomy

```
annotation = read.delim("classification_wOutgroup.txt",sep="\t",header = FALSE)
colnames(annotation)=c("MAG","tax")
annotation=annotation%>%separate(tax,into=c("d","p","c","o","f","g","s"),sep=";",remove=FALSE)
```

Read in data on linking MAG to source
```
site <- read.delim('Methanoregula_MAGs_list.txt')
```

Read in physiology data
```
pangenome <- read.delim('Methanoregula_physiology.txt')
```

 Make archaeal tree
 ```
arc_dat=as.data.frame(arc_tree$tip.label)
colnames(arc_dat)=c("MAG")
arc_dat=left_join(arc_dat,annotation,by="MAG")
```

Undecorated tree - section 4.2.2 of https://yulab-smu.top/treedata-book/chapter4.html
```
regula_tree <- ggtree(arc_tree, color="black", size=0.5, linetype="solid", layout="rectangular",  branch.length = "branch.length")
```

Color palette for sites
```
Sourcecol <- c("GTDB"="#a3c754", "JGI"="#059553", "OWC"="#023c31", "STM"="#dfc27d", 'PPR'="#359890")
MetabCol <- c("Any_genes_present"= "darkorange",   "Majority_of_the_genes"="darkorange3",
              "Present"="chocolate4",  "Missing"="ghostwhite")
```

Script for physiology analyses
```
arc=regula_tree +
  geom_fruit(data=site,geom=geom_tile,
                 mapping=aes(y=MAG,x=0,fill=Source),width = 0.05,offset=0.01,show.legend=TRUE,color = "black",lwd = 0.25,linetype = 1) +
  scale_fill_manual(values=Sourcecol)+
  new_scale_fill() + 
  geom_fruit(data=pangenome,geom=geom_tile,
             mapping=aes(y=MAG,x=0,fill=Mcr_Complex),width = 0.05,offset=0.06,show.legend=TRUE,color = "black",lwd = 0.25,linetype = 1)+
  scale_fill_manual(values=MetabCol)+
  geom_fruit(data=pangenome,geom=geom_tile,
             mapping=aes(y=MAG,x=0,fill=WLP),width = 0.05,offset=0.06,show.legend=TRUE,color = "black",lwd = 0.25,linetype = 1)+
  geom_fruit(data=pangenome,geom=geom_tile,
             mapping=aes(y=MAG,x=0,fill=MoFe_Nitrogenase_Percent_Encoded),width = 0.05,offset=0.06,show.legend=TRUE,color = "black",lwd = 0.25,linetype = 1)+
  geom_fruit(data=pangenome,geom=geom_tile,
             mapping=aes(y=MAG,x=0,fill=Oxygen_stress),width = 0.05,offset=0.06,show.legend=TRUE,color = "black",lwd = 0.25,linetype = 1)+
  geom_fruit(data=pangenome,geom=geom_tile,
             mapping=aes(y=MAG,x=0,fill=Crispr),width = 0.05,offset=0.06,show.legend=TRUE,color = "black",lwd = 0.25,linetype = 1)+
  geom_fruit(data=pangenome,geom=geom_tile,
             mapping=aes(y=MAG,x=0,fill=Methylotrophy_MethylO),width = 0.05,offset=0.06,show.legend=TRUE,color = "black",lwd = 0.25,linetype = 1)+
  geom_fruit(data=pangenome,geom=geom_tile,
             mapping=aes(y=MAG,x=0,fill=Methylotrophy_MethylS),width = 0.05,offset=0.06,show.legend=TRUE,color = "black",lwd = 0.25,linetype = 1)

 arc
```




