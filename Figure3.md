**Figure 3 A-E Network Construction**




This code has been modified from:
https://kelseyandersen.github.io/NetworksPlantPathology/Microbiome_network_ICPP2018_v2.html

```{r}
# install.packages("igraph") # uncomment this line in order to install this package
library(igraph)  
# install.packages("Hmisc") # uncomment this line in order to install this package
library(Hmisc)  
# install.packages("Matrix") # uncomment this line in order to install this package
library(Matrix) 
library(tidyverse)
#devtools::install_github("RMHogervorst/gephi")
library(gephi)
```



```{r}
asv.table<-read.csv("Source_data_Figure3a.csv", header=T, row.names = 1)
tax<-read.csv("Source_data_Figure3_taxonomy.csv",header=T, row.names = 1)
```


Check the dimensions of the data frame
```{r}
dim(asv.table)

```
Filter out low abundance asvs (less than 10 reads)
```{r}
asv.table.filter <- asv.table[ ,colSums(asv.table) >= 10]
print(c(ncol(asv.table),"versus",ncol(asv.table.filter))) #compare initial and filtered counts
```

We use rcorr to calculate the Spearman correlation coefficient between ASVs. This creates a list with 3 values:
"r" (rho): Correlation coefficent
"n": number of observations
"P": p-value
```{r}
asv.cor <- rcorr(as.matrix(asv.table.filter), type="spearman")
```

asv.cor$P will pull the p-value info from the list above and forceSymmetric() will assign NA to self-correlations
```{r}
asv.pval <- forceSymmetric(asv.cor$P) 
```

Filter taxa to select only the ASVs retained after filtering for low read conuts
```{r}
sel.tax <- tax[rownames(asv.cor$P),,drop=FALSE]
```

Make sure your filtered tables match
```{r}
all.equal(rownames(sel.tax), rownames(asv.pval))
```

Filter to retain only significant associations 
```{r}
p.yes <- asv.pval<0.05
```

```{r}
r.val = asv.cor$r # select all the correlation values
p.yes.r <- r.val*p.yes # only select correlation values based on p-value criterion 
```

Select ASVs based off of Spearman Correlation. Here we are keeping only values with correlation coefficient higher than 0.6 weaker.
```{r}
p.yes.r <- abs(p.yes.r)>0.6 # output is logical vector
p.yes.rr <- p.yes.r*r.val # use logical vector for subscripting.
```

Create an adjcency matrix
```{r}
adjm <- as.matrix(p.yes.rr)
```


The next steps creates an object from the adjacency matrix. Weight represents level of correlation
```{r}
net.grph=graph.adjacency(adjm,mode="undirected",weighted=TRUE,diag=FALSE)
```

You can use this to pull out edge weights
```{r}
edgew<-E(net.grph)$weight
```


Creating a vector to remove the isolated nodes (nodes with no interactions) and then remove those nodes from the object
```{r}
bad.vs<-V(net.grph)[degree(net.grph) == 0] 

net.grph <-delete.vertices(net.grph, bad.vs)
```


Write out the edge files
```{r}
gephi_write_edges(net.grph, "networks/p8_filtered_0.75_edge_all_edge.csv")
```

Create a nodes file
```{r}
taxa<-read_csv("networks/genus_id_gtdb.csv")
edges <- read_csv("networks/p8_filtered_0.75_edge_all_edge.csv")
```

```{r}
edges_s <- edges %>% 
  select(1)
```
```{r}
edges_t <- edges %>% 
  select(2)
```
```{r}
edges_all <- edges_s %>% 
  full_join(edges_t,by = c("Source" = "Target")) %>% 
  distinct() %>% 
  rename("GenID" = "Source") %>% 
  left_join(taxa)
```

```{r}
write_csv(edges_all, "node.csv")
```
Using the corresponding network ID files and the taxonomy files, genera can be assigned function. For figure 3 we only retained the methanogens and their connections.

Now we will visualize the networks and get network statistics outside of R using the progran Gephi. Download at https://gephi.org/users/download/


