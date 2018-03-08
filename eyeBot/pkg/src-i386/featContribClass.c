#include "featContrib.h"
//#include <Rmath.h>
//#include <R.h>

/* Node status */
//#define NODE_TERMINAL -1
//#define NODE_TOSPLIT  -2
//#define NODE_INTERIOR -3

//getlocal contributions
void lIncrementClass(double *data, double *out, int *nrColumn, int *nrRow,
               int *nTrees, int *lDaughter, int *rDaughter,
               int *nodestatus, int *maxNodeNumber, double *xsplit,
               double *avnodes, int *mbest, int *treeSizes, int *cat, int *inbag, double *localIncrements,
               double *rootMeans, int *nrClass){

    memset(localIncrements, 0, *nTrees * *maxNodeNumber *  *nrClass * sizeof(double));
    memset(rootMeans, 0, *nrClass**nTrees * sizeof(double));
    int idx1 = 0;
    int idx2 = 0;

    int inbag_idx = 0;

    for (int i = 0; i < *nTrees; ++i) 
    {  
		tlIncrementClass(data, out, *nrColumn, *nrRow, lDaughter+idx1, rDaughter+idx1,nodestatus +idx1,  xsplit+idx1,  mbest+idx1, treeSizes[i], cat, inbag + inbag_idx, localIncrements+idx2, rootMeans+i,  nrClass);          
		idx1 += *maxNodeNumber; /* increment the offset */ 
		idx2 += *maxNodeNumber * *nrClass;
		inbag_idx += *nrRow;
    }  
}

#define cathegorical_choice(split, data) (((unsigned int) split) & (((unsigned int) 1) << ((int) (data) - 1)))

void  tlIncrementClass(double *data, double *out, int nrColumn, int nrRow,
               int *lDaughter, int *rDaughter,
               int *nodestatus, double *xsplit,
               int *mbest, int treeSize, int *cat, int *inbag, double *localIncrements,
               double *rootMeans, int *nrClass)
{

	int j=0,m=0, k=0;
	int inBagCount=0;
	int pos=0;
	
	int *nodeCount= (int *) Calloc(treeSize, int); 
	memset(nodeCount, 0, treeSize * sizeof(int));
	double *avgPred= (double *) Calloc(*nrClass * treeSize, double); 
	memset(avgPred, 0, *nrClass * treeSize * sizeof(double));
	
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
					//Rprintf("Cat data split find for column %d \n", xsplit[k]);
					k = cathegorical_choice(xsplit[k], data[m + j*(nrColumn)]) ? lDaughter[k] - 1 : rDaughter[k] - 1;
				}
			}
			nodeCount[k]++;
			pos=(out[j]-1) + (k* *nrClass);
			avgPred[pos]++;
			//Rprintf("record  %d, out %f, node  nr %d,   node Count %d, average %f\n",j, out[j], k, nodeCount[k], avgPred[pos]);
		}	
	}  
        
	meanActivityClass(avgPred, nodeCount, nodestatus,lDaughter,rDaughter, 0, *nrClass);
/*	for(int o=0;o< treeSize; o++)
	{
		Rprintf("node %d\t",o);
		for(int r=0;r<*nrClass; r++)
			Rprintf("%f\t", avgPred[o * *nrClass+r]);
		Rprintf("\n");
		
	}*/
	
	localIncrementsOfContribClass(avgPred, nodestatus,lDaughter,rDaughter, NULL, 0, localIncrements, *nrClass);
	*rootMeans=avgPred[0];
/*	Rprintf(" local increment \n");
	for(int o=0;o< treeSize; o++)
	{
		Rprintf("node %d\t",o);
		for(int r=0;r<*nrClass; r++)
			Rprintf("%f\t", localIncrements[o* *nrClass +r]);
		Rprintf("\n");
		
	}*/
   
	Free (nodeCount);
	Free (avgPred);
}

void meanActivityClass(double *avgPred,  int *nodeCount, int *nodestatus, int *lDaughter, int *rDaughter, int k, int nrClass)
{ 
	if(nodestatus[k] == NODE_TERMINAL)
	{
	        //Rprintf("  term node %d, count  %f, mean %f !\n",k, nodeCount[k], avgPred[k]);
        	if(nodeCount[k]!= 0){
				for(int i=0;i<nrClass;i++)
				 avgPred[i+k*nrClass] /= nodeCount[k];
			 }
                //Rprintf("  term node %d, count  %f, mean %f !\n",k, nodeCount[k], avgPred[k]);
		
	}
	else
	{
		int lDaugh=lDaughter[k]-1;
		int rDaugh=rDaughter[k]-1;
		
		meanActivityClass(avgPred, nodeCount, nodestatus,lDaughter,rDaughter, lDaugh, nrClass);
		meanActivityClass(avgPred, nodeCount, nodestatus,lDaughter,rDaughter, rDaugh, nrClass); 
		//Rprintf("node %d, left count  %d, mean %f !\n",k, nodeCount[lDaugh], avgPred[lDaugh]);
		//Rprintf(" node %d, right count  %d, mean %f !\n",k, nodeCount[rDaugh], avgPred[rDaugh]);
		
		nodeCount[k] = nodeCount[lDaugh] + nodeCount[rDaugh];
		if (nodeCount[k] != 0)
		{
			for(int i=0;  i<nrClass; i++)
				avgPred[i+k*nrClass] = (nodeCount[lDaugh] * avgPred[i+lDaugh*nrClass] + nodeCount[rDaugh]*avgPred[i+rDaugh*nrClass]) / nodeCount[k];
		}
		//Rprintf("node %d, count %d, mean %f !\n",k, nodeCount[k], avgPred[k]);
	}
                   

}

void localIncrementsOfContribClass(double *avgPred,  int *nodestatus, int *lDaughter, int *rDaughter, int *p_k, int k, double *localIncrements, int nrClass)
{
	if(p_k)
	{
		for(int i=0; i<nrClass;  i++)
		{
			//Rprintf("child %d parent %d\n",i+k*nrClass, i+*p_k * nrClass);
			//Rprintf("node %d class %d,  avPred %f avPred parent %f !\n",k,i, avgPred[i+k*nrClass], avgPred[i+*p_k * nrClass]);
			localIncrements[i+k*nrClass]+=avgPred[i+k*nrClass]-avgPred[i+*p_k * nrClass];
		}
	}
		
    if(nodestatus[k] != NODE_TERMINAL)
    {
		int lDaugh=lDaughter[k]-1;
		int rDaugh=rDaughter[k]-1;
		
		localIncrementsOfContribClass(avgPred, nodestatus,lDaughter,rDaughter, &k, lDaugh, localIncrements, nrClass);
		localIncrementsOfContribClass(avgPred, nodestatus,lDaughter,rDaughter, &k, rDaugh, localIncrements, nrClass);
		
	}
	
}

//calculate feature contribution	
void fContribClass(double *data, int *nrColumn, int *nrRow,
               int *nTrees, int *lDaughter, int *rDaughter,
               int *nodestatus, int *maxNodeNumber, double *xsplit, int *mbest, int *nodepredict, int *treeSizes, int *cat, double *localIncrements, double *contrib, int *nrClass)
{
    memset(contrib, 0, *nrRow  * *nrColumn * sizeof(double));
    int idx1 = 0;
    int idx2 = 0;
   
    for (int i = 0; i < *nTrees; ++i) 
    {
		tfContribClass(data, *nrColumn, *nrRow, lDaughter+idx1, rDaughter+idx1, nodestatus +idx1,  xsplit+idx1,  mbest+idx1, nodepredict, treeSizes[i], cat, localIncrements+idx2, contrib, nrClass);       
		idx1 += *maxNodeNumber; /* increment the offset */ 
		idx2 += *maxNodeNumber * *nrClass;
    }
    for (int i = (*nrRow) * (*nrColumn) - 1; i >= 0; i--)
		contrib[i] /= *nTrees;	
			   
}


void tfContribClass(double *data, int nrColumn, int nrRow,
               int *lDaughter, int *rDaughter,
               int *nodestatus, double *xsplit,
               int *mbest,  int *nodepred, int treeSize, int *cat, double *localIncrements, double *contrib, int *nrClass)
{

	int  j=0,m=0, k=0;
  
	for (j = 0; j < nrRow; ++j) 
	{
		k = 0;
			while (nodestatus[k] != NODE_TERMINAL) 
			{ // go down the tree 
				int descriptor = mbest[k] - 1;
				
				//Rprintf("locIncrement   %f!\n", localIncrements[k]);
				
				if (cat[descriptor] == 1) {
					//Rprintf("ind   %d!\n", m+j*(nrColumn-1)); 
					//Rprintf("ind   %d!\n", descriptor + j*(nrColumn));
					//Rprintf("ind   %d!\n", k);
					//Rprintf("ind   %f!\n", xsplit[k]);
					
					// Find the child
					k = (data[descriptor + j*(nrColumn)] <= xsplit[k]) ? lDaughter[k] - 1 : rDaughter[k] - 1;
				}
				else{
				    //Rprintf("local contrib - Cat data split find for column %d \n", xsplit[k]);
				    k = cathegorical_choice (xsplit[k], data[m + j*(nrColumn)]) ? lDaughter[k] - 1 : rDaughter[k] - 1;
				}
					// Update the contribution
					
					//Rprintf(" index  %d!\n",descriptor + j*(nrColumn) );
					//Rprintf("node %d class %d incr %f\n",k, nodepred[j], localIncrements[(nodepred[j] - 1) + k * *nrClass] ); 		
					
					contrib[descriptor + j*(nrColumn)] += localIncrements[(nodepred[j] - 1) + k * *nrClass];	
				
			}		
	}

}
