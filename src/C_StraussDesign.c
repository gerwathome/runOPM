#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <R.h>  /* to include Rconfig.h */

#ifdef ENABLE_NLS
          #include <libintl.h>
          #define _(String) dgettext ("DiceDesign", String)
         /* replace pkg as appropriate */
          #else
          #define _(String) (String)
          #endif

#define MAX(a,b) (a<b ? b:a)
#define MIN(a,b) (a<b ? a:b)

/* Source code to write init as a matrix (mat) of n random numbers in [0,1]^d (mat = dxn)*/
void C_mat_alea( const double *init,const int *d,const int *n,double **mat){
  int point, dim;
  for (point=0; point<*n; point++){
    for (dim=0; dim<*d; dim++){
      mat[dim][point] = init[dim+*d*point];
    }
  }
}

/* ##############################################################################################/
##Jessica FRANCO (2006.10.02)
## Delphine DUPUY ( 2008.12.20 -> 2013.02.14)
## Case with 1D constraints
## inputs :
##	n 	= number of experiments
## 	di 	= dimension of the space
##	nmc 	= number of Monte Carlo iterations
##	alpha	= potential power (default is 0.5)
##	constraints1D = 1 if projection constraints is required and 0 otherwise
##	RND 	= radius in the d-space
##	Note the the radius on the projection (R1D) is fixed at 1/n*0.75
##	v	=  the initial design which is modified during the procedure in order to return the final Strauss Design (dimension dxn)
##       The values of the repulsion parameters are fixed : equal to 0.01 in 1D (see gamma) if alpha=0 and 10 in nD  (see betaND) .
##	betaND	=  real which represents the repulsion parameter in nD
############################################################################################## */

void Strauss(const int *n, const int *d,const int *nmc,const double *alpha, const int *constraints1D, const double *RND, const double *repulsion, const double *gamma1D, const int *seed, double **v){

  double R1D = (1.0/(float)*n)*0.75;	/* radius in 1D */
  double gamma = *gamma1D;				/* repulsion parameter in 1D for alpha=0 */
  int graine = *seed;

  double RNDcarre=(*RND)*(*RND); 
  double p,pND,p1D,nu,nx,sum,sumu,betaND,gammaND,pu=0,px=0,nnx,nnu;
  int iunif,dim,point;
  int inmc, k;

GetRNGstate();

  /* initialization of the random generator */
  // unsigned int graine=time(NULL);
 // srand(graine); 

  /*   Allocation of memory of a d-dimensional vector u corresponding to the proposed new point  */
  double *u = (double*) malloc((*d) * sizeof(double));
  if (!u){
    error(_("Strauss: cannot allocate memory"));
    // fprintf(stderr, "Strauss : cannot alocate %u bytes, aborting ",(*d)*sizeof(double) ); exit(-1);
  }
	  
  if (*alpha != 0){
    betaND = *repulsion;
    for (inmc=0; inmc< *nmc; inmc++){
      for (k=0; k< *n; k++){
        /* selection of a point (column number betwenn 0 and (n-1)) */
		iunif = (int) (((double) *n) * ( (double)unif_rand() ));
		/* computation of nx and nu for the one-dimensional case (row by row)
			nx = number of points closed to the selected point x 
			nu = number of points closed to the possible new point u
		*/
        p1D = 1;
        for (dim = 0 ; dim<*d; dim++){
			nu=0;	nx=0;
			/* Propose a new point u (d lines and 1 column) */
			u[dim] = (double)unif_rand() ;
			
		if (*constraints1D == 1){	
            for (point=0; point<*n; point++){
              if (iunif != point){
                if( (fabs(v[dim][point]-v[dim][iunif])) < (R1D)){
                  nx++;
                }
                if( (fabs(v[dim][point]-u[dim])) < (R1D)){
                  nu++;
                }
              }
            }
            p1D = p1D*pow(gamma,MAX(0,nu-nx)); 
          }
        }
        px=0;pu=0;
        /* Computation of nx and nu for the N-dimensional case (sum over the columns) */
        for (point=0; point<*n; point++){
          if (point != iunif){
            sum = 0;
            sumu = 0;
            for (dim=0; dim<*d; dim++){
              sum += (v[dim][point]- v[dim][iunif])*(v[dim][point]- v[dim][iunif]);
              sumu += (v[dim][point]- u[dim])*(v[dim][point]- u[dim]);
            }
            nx = sqrt(sum); 
            nu = sqrt(sumu);
            if (nx < *RND){
              nnx = pow((double)(1-((float)nx/(float)*RND)),*alpha);
              px += nnx;
            }
            if (nu < *RND){
              nnu = pow((double)(1-((float)nu/(float)*RND)),*alpha);
              pu += nnu;
            }
          }
        }
        pND = MIN(1,(float)(exp(-betaND*pu))/exp(-betaND*px));
        p = pND * p1D;
        if	(unif_rand() < p){
          for (dim=0; dim<*d; dim++){
            v[dim][iunif] = u[dim];
          }
        }
      }
    }
  }else{
    gammaND=*repulsion;
    
    for (inmc=0; inmc<*nmc; inmc++){
      for (k=0; k<*n; k++){
        /* selection of a point (column number betwenn 0 and n) */
		iunif = (int) (((double) *n) * ( (double)unif_rand() ));
		/* computation of nx and nu for the one-dimensional case (row by row) */
        p1D = 1;	
        /* study dimension by dimension (row by row) */
        for (dim = 0 ; dim<*d; dim++){
			nu=0;	nx=0;
			/* Propose a new point u (d lines and 1 column) */
			u[dim] = (double)unif_rand() ;
			if (*constraints1D == 1){
				for (point=0; point<*n; point++){
					if (iunif != point){
						if( (fabs(v[dim][point]-v[dim][iunif])) < (R1D)){
							nx++;
						}
						if( (fabs(v[dim][point]-u[dim])) < (R1D)){
							nu++;
						}
					}
				}
				p1D = p1D*pow(gamma,MAX(0,nu-nx));
			}
        }
		nx=0;nu=0;
        /* Computation of nx and nu for the N-dimensional case (sum over the columns) */
        for (point=0; point<*n; point++){
          if (point != iunif){
				sum = 0.0;
				sumu = 0.0;
				for (dim=0; dim<*d; dim++){
				sum += (v[dim][point]- v[dim][iunif])*(v[dim][point]- v[dim][iunif]);
				sumu += (v[dim][point]- u[dim])*(v[dim][point]- u[dim]);
				}
				if (sum < RNDcarre){
				nx++;
				}
				if (sumu < RNDcarre){
				nu++;
				}
			}
        }
    	p = pow(gammaND,MAX(0,nu-nx)) * p1D;      
        if	(unif_rand() < p){
			for (dim=0; dim<*d; dim++){
				v[dim][iunif] = u[dim];
			}
        }
      }
    }
  }
  free(u);
PutRNGstate();


}

/* #####################################################################################
## *** Main program ***
## inputs :
##	n 	= number of experiments
## 	d 	= dimension of the space
##	constraints1D = 1 if 1D projection constraints are required and 0 otherwise
##	nmc 	= number of Monte Carlo iterations
##	RND 	= radius in the d-space
##	alpha	= potential power (default is 0.5)
##      seed   = seed for the generation of the new points
## output :
##      ans = the Strauss-Design finally obtained.
#################################################################################### */
void C_StraussDesign(const double *init, const int *n,const int *d, const int *constraints1D, const int *nmc, const double *RND, const double *alpha, const double *repulsion, const double *gamma1D,  const int *seed, double *ans){
  int dim, point;
  /* Construction of the initial design of experiments (random distribution of n points in the unit cube [0,1]^d) */
  double **v = (double**) malloc ((*d) * sizeof(double*)); 
  if (!v){
    error(_("C_StraussDesign: cannot allocate memory for v"));
    // fprintf(stderr, "C_StraussDesign: cannot alocate %zu bytes, aborting ",(*d) * sizeof(double*) ); 	exit(-1);
  }
  for (dim=0; dim<*d; dim++){
    v[dim] = (double*) malloc ((*n) * sizeof(double));
    if (!v[dim]){
       error(_("C_StraussDesign: cannot allocate memory for v[dim]"));
      // fprintf(stderr, "C_StraussDesign: cannot alocate %zu bytes, aborting ",(*n)*sizeof(double) ); 	exit(-1);
    }
  }
  C_mat_alea(init,d,n,v);
  
  Strauss(n, d, nmc, alpha, constraints1D, RND,repulsion,gamma1D,seed,v);

  /* Writing the coordinate of the final designs 'v' in 'ans' */
  int indix=0;
  for (point=0; point<*n; point++){
    for ( dim=0; dim<(*d); dim++){
      ans[indix] = v[dim][point];
      indix++;
    }
  }
  free(v);
}
