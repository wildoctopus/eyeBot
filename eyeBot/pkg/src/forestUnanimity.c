#include "featContrib.h"
//#include <Rmath.h>
//#include <R.h>

/* Node status */
//#define NODE_TERMINAL -1
//#define NODE_TOSPLIT  -2
//#define NODE_INTERIOR -3

//getlocal contributions
void forestUnanimity(double *data, double *out, int *nrColumn, int *nrRow,
               int *nTrees, int *lDaughter, int *rDaughter,
               int *nodestatus, int *maxNodeNumber, double *xsplit,
               double *avnodes, int *mbest, int *treeSizes, int *cat, int *inbag,  int *nrClass, double *funamity){

    memset(funamity, 0, *nTrees**maxNodeNumber* *nrClass * sizeof(double));
    int idx1 = 0;
    int idx2 = 0;

    int inbag_idx = 0;

    for (int i = 0; i < *nTrees; ++i) 
    {
		//Rprintf("Tree nr  %d, nodes nr %d\n",i, treeSizes[i]);  

		treeUnanimity(data, out, *nrColumn, *nrRow, lDaughter+idx1, rDaughter+idx1,nodestatus +idx1,  xsplit+idx1,  mbest+idx1, treeSizes[i], cat, inbag + inbag_idx, nrClass, funamity+idx2);
          
		idx1 += *maxNodeNumber; /* increment the offset */ 
		idx2 += *maxNodeNumber * *nrClass;
		inbag_idx += *nrRow;
    }
       
}

#define cathegorical_choice(split, data) (((unsigned int) split) & (((unsigned int) 1) << ((int) (data) - 1)))

void  treeUnanimity(double *data, double *out, int nrColumn, int nrRow,
               int *lDaughter, int *rDaughter,
               int *nodestatus, double *xsplit,
               int *mbest, int treeSize, int *cat, int *inbag,  int *nrClass, double *funamity)
{

	int  j=0,m=0, k=0;
	int inBagCount=0;
	int pos=0;
	
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

			pos=(out[j]-1) + (k* *nrClass);
			funamity[pos]++;
		}	
	}  
        
	
}


