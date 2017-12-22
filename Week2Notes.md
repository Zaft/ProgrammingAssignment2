## Week Three Notes

#### Looping Functions

##### apply

##### lapply

##### mapply

##### tapply - tapply is used to apply a function over subsets of a vector. 

```
str(tapply)
function (X, INDEX, FUN = NULL, ..., default = NA, simplify = TRUE)  
```

It can be used to apply a function over a column and group by values in another column. The first is the column is the vector and the second is the values your want to group by.

##### split - takes a vector or other objects and splits it into groups determined by a factor or list of factors.

Similar to tapply in that it groups values together, but split returns a list which you can then apply a function to afterwards.







