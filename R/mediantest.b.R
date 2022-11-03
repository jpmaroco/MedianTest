
# This file is a generated template, your changes will not be overwritten

MEDIANTESTClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "MEDIANTESTClass",
    inherit = MEDIANTESTBase,
    public = list(
        initialize=function(...) {
            super$initialize(...)
            require("BSDA") 
        }
    ),
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
            
            
            dep <- self$options$dep
            testvalue <- self$options$testvalue
            
            
            if ( is.null(self$data) || is.null(self$options$dep) )
            {print("Select your variables")} 
            else {
                
                results <- SIGN.test(x=self$data[,dep], 
                                     md=self$options$testvalue, 
                                     alternative=self$options$alt,
                                     conf.level = self$options$conf) 
                
                #self$results$text$setContent(results) 
                
                
                table <- self$results$mediantest
                
                pval <- ifelse(results$p.value <.001, "<.001", results$p.value)
                side <- ifelse(self$options$alt=="two.sided", "=", ifelse(self$options$alt=="greater", "<=", ">="))
                
                
                tabTitStr <- paste0("Median Sign Test: H0: md",side, testvalue)
                tabTit <- jmvcore::format(tabTitStr, dep=self$options$dep)
                table$setTitle(tabTit)
                
                ciTitle <- paste0(self$options$conf*100, "% Conf. Interval - Interpolated")
                table$getColumn("Lower")$setSuperTitle(ciTitle)
                table$getColumn('Upper')$setSuperTitle(ciTitle)
                
                table$setRow(rowNo=1, values=list(
                    var=self$options$dep,
                    Median=results$estimate,
                    Statistic=results$statistic,
                    Eventrate=results$estimate,
                    p=pval,
                    Lower=results$conf.int[1],
                    Upper=results$conf.int[2]
                ))
                
                
                dtci <- data.frame(results$Confidence.Intervals)
                
                tableci <- self$results$medianci
                
                for (i in 1:nrow(dtci)){
                tableci$setRow(rowNo=i, values=list(
                    type=row.names(dtci)[i],
                    ConfLevel=dtci$Conf.Level[i],
                    Eventrate=results$estimate,
                    p=pval,
                    Lower=dtci$L.E.pt[i],
                    Upper=dtci$U.E.pt[i]
                ))
                }
                
              }
        })
)
