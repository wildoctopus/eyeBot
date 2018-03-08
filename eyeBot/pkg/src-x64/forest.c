#include "featContrib.h"

void votingToProb(double *data, double *out, int *nrColumn, int *nrRow,
               int *nTrees, int *lDaughter, int *rDaughter,
               int *nodestatus, int *maxNodeNumber, double *xsplit,
               double *nodepred, int *mbest, int *treeSizes, int *cat, int *inbag) 
{

	int idx1=0;
	int inbag_idx=0;
	 for (int i = 0; i < *nTrees; ++i) 
    {
		

		treeVoteToProb(data, out, *nrColumn, *nrRow, lDaughter+idx1, rDaughter+idx1,nodestatus +idx1,  xsplit+idx1,  mbest+idx1, treeSizes[i], cat, inbag + inbag_idx, nodepred+idx1);
          
		idx1 += *maxNodeNumber; /* increment the offset */ 
		inbag_idx += *nrRow;
    }
   /* for(j=0; j<*maxNodeNumber; j++)
    {
		Rprintf("node pred  %f",  nodepred[j]);
	}*/
	
	
	
	
}
#define cathegorical_choice(split, data) (((unsigned int) split) & (((unsigned int) 1) << ((int) (data) - 1)))	
void  treeVoteToProb(double *data, double *out, int nrColumn, int nrRow,
               int *lDaughter, int *rDaughter,
               int *nodestatus, double *xsplit,
               int *mbest, int treeSize, int *cat, int *inbag, double *nodepred)
{
		int *nodeCount= (int *) Calloc(treeSize, int); 
		memset(nodeCount, 0, treeSize * sizeof(int));
		double *avgPred= (double *) Calloc(treeSize, double); 
		memset(avgPred, 0, treeSize * sizeof(double));
		int k=0;
		int j=0;
		int inBagCount=0;
		int m=0;
		for (j = 0; j < nrRow; ++j) {
		k = 0;
		if(inbag[j]==1)
		{
			inBagCount++;
		
			while (nodestatus[k] != NODE_TERMINAL) { /* go down the tree */
				m = mbest[k] - 1;
				
				if (cat[m] == 1) {
					
					k = (data[m + j*(nrColumn)] <= xsplit[k]) ? lDaughter[k] - 1 : rDaughter[k] - 1;
				} else {
					
					k = cathegorical_choice(xsplit[k], data[m + j*(nrColumn)]) ? lDaughter[k] - 1 : rDaughter[k] - 1;
				}
			}
			
			nodeCount[k]++;
			avgPred[k] += out[j];
		}	
	}  	
    calculateProb(avgPred, nodeCount, nodestatus,lDaughter,rDaughter, 0, nodepred);
    
    
    //nodepred=avgPred;
    Free (nodeCount);
	Free (avgPred);
			
}




void calculateProb(double *avgPred,  int *nodeCount, int *nodestatus, int *lDaughter, int *rDaughter, int k, double *nodepred)
{ 
	if(nodestatus[k] == NODE_TERMINAL)
	{
	        //Rprintf("  term node %d, count  %f, mean %f !\n",k, nodeCount[k], avgPred[k]);
        	if(nodeCount[k]!= 0) {
				avgPred[k] /= nodeCount[k];
				nodepred[k]=avgPred[k];
	          }
                //Rprintf("  term node %d, count  %f, mean %f !\n",k, nodeCount[k], avgPred[k]);
		
	}
	else
	{
		int lDaugh=lDaughter[k]-1;
		int rDaugh=rDaughter[k]-1;
		
		calculateProb(avgPred, nodeCount, nodestatus,lDaughter,rDaughter, lDaugh, nodepred);
		calculateProb(avgPred, nodeCount, nodestatus,lDaughter,rDaughter, rDaugh, nodepred); 
		//Rprintf("node %d, left count  %d, mean %f !\n",k, nodeCount[lDaugh], avgPred[lDaugh]);
		//Rprintf(" node %d, right count  %d, mean %f !\n",k, nodeCount[rDaugh], avgPred[rDaugh]);
		
		nodeCount[k] = nodeCount[lDaugh] + nodeCount[rDaugh];
		if (nodeCount[k] != 0){
			
			avgPred[k] = (nodeCount[lDaugh]*avgPred[lDaugh] + nodeCount[rDaugh]*avgPred[rDaugh]) / nodeCount[k];
			nodepred[k]=avgPred[k];
		}
		//Rprintf("node %d, count %d, mean %f !\n",k, nodeCount[k], avgPred[k]);
	}
                   
}


void predict(double *data, double *yPred, int *nrColumn, int *nrRow,
               int *nTrees, int *lDaughter, int *rDaughter,
               int *nodestatus, int *maxNodeNumber, double *xsplit,
               double *nodepred, int *mbest, int *treeSizes, int *cat, int *inbag) 
{

	int idx1=0;
	int inbag_idx=0;
	for (int i = 0; i < *nTrees; ++i) 
    {
		

		treePred(data, yPred, *nrColumn, *nrRow, lDaughter+idx1, rDaughter+idx1,nodestatus +idx1,  xsplit+idx1,  mbest+idx1, treeSizes[i], cat, inbag + inbag_idx, nodepred+idx1);
          
		idx1 += *maxNodeNumber; /* increment the offset */ 
		inbag_idx += *nrRow;
    }
    for (int i = 0; i < *nrRow; ++i) yPred[i] /= *nTrees;
}

void  treePred(double *data, double *yPred, int nrColumn, int nrRow,
               int *lDaughter, int *rDaughter,
               int *nodestatus, double *xsplit,
               int *mbest, int treeSize, int *cat, int *inbag, double *nodepred)
{

		int k=0;
		int j=0;
		int m=0;
		for (j = 0; j < nrRow; ++j) {
		k = 0;
			
		
			while (nodestatus[k] != NODE_TERMINAL) { /* go down the tree */
				m = mbest[k] - 1;
				
				if (cat[m] == 1) {
					
					k = (data[m + j*(nrColumn)] <= xsplit[k]) ? lDaughter[k] - 1 : rDaughter[k] - 1;
				} else {
					
					k = cathegorical_choice(xsplit[k], data[m + j*(nrColumn)]) ? lDaughter[k] - 1 : rDaughter[k] - 1;
				}
			}
			
			yPred[j] += nodepred[k];
		}	
	 	

			
}
