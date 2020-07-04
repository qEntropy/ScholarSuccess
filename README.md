# ScholarSuccess

### Clustering

Used Louvain algorithm in R to construct the clusters of major subject areas.

![Alt text](images/cluster.png?raw=true)

### PageRanks

Constructed an author network for USA, Europe, and Australasia. Used gephi to compute the page rank for each scholar and constructed a graph shown below. Each node in the graph is a scholar and edges connecting scholars represent joint publications.

![Alt text](images/giant_component.png?raw=true)


### Statistical Modeling

#### Linear Models

The response variable would be citation impact. Our principle interest is to test how the success rate (total number of citations) of a scholar behaves with the cross-disciplinary index. While the key predictor is χ<sub>i</sub>, we also added control factors to our model to account for the effects of co-variates. We included attributes like PageRank of scholar, total funding received etc. In addition we have used also use career age, a categorical
variable, as one of the factor impacting the response variable. ε is the white noise.

![Alt text](images/linear_model.jpg?raw=true)

We got the following results when we run the above linear model in R with US/Canada data. We used a robust/stricter definition for cross-disciplinarity, that is, if and only if a F<sub>i</sub> has published 4 or more than 4 papers with a co-author who is not from the same discipline, only then we consider the cross-disciplinarity of scholar as 1 otherwise we consider it 0. As a concrete example, if F<sub>i</sub> is from Technology, and she has published 8 papers with other researchers who are from Biology field, we consider her χi as 1. Had she only published 3 papers with faculty from a different field, we’d have considerd her χ<sub>i</sub> as 0.

![Alt text](images/results.png?raw=true)
