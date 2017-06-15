#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define precision 0.000000000001

#define SQUARE(x) ((x)*(x))
#define CUBE(x) ((x)*(x)*(x))
#define MAX(a,b) (a<b ? b:a)


double C_uniform_cdf(const double *p) {

	return(0.5 * (MAX(*p + 1., 0.) - MAX(*p - 1., 0.)));

}


double C_sumof2uniforms_cdf(const double *p, const double *ax, const double *ay, const int *k) {
	
	double s = 0.;
	
	if ((fabs(ax[*k]) > precision) && (fabs(ay[*k]) > precision)) {
		s += SQUARE(MAX(*p - ax[*k] - ay[*k], 0.));
		s += SQUARE(MAX(*p + ax[*k] + ay[*k], 0.));
		s -= SQUARE(MAX(*p - ax[*k] + ay[*k], 0.));
		s -= SQUARE(MAX(*p + ax[*k] - ay[*k], 0.));
		return(s / (8 * ax[*k] * ay[*k]));
	} 
	else return(C_uniform_cdf(p));

}


double C_sumof3uniforms_cdf(const double *p, const double *ax, const double *ay, const double *az, const int *k) {
	
	double s = 0.;
	
	if ((fabs(ax[*k]) > precision) && (fabs(ay[*k]) > precision) && (fabs(az[*k]) > precision)) {
		s += CUBE(MAX(*p + ax[*k] + ay[*k] + az[*k], 0.));
		s += CUBE(MAX(*p + ax[*k] - ay[*k] - az[*k], 0.));
		s += CUBE(MAX(*p - ax[*k] + ay[*k] - az[*k], 0.));
		s += CUBE(MAX(*p - ax[*k] - ay[*k] + az[*k], 0.));
		s -= CUBE(MAX(*p - ax[*k] - ay[*k] - az[*k], 0.));
		s -= CUBE(MAX(*p - ax[*k] + ay[*k] + az[*k], 0.));
		s -= CUBE(MAX(*p + ax[*k] - ay[*k] + az[*k], 0.));
		s -= CUBE(MAX(*p + ax[*k] + ay[*k] - az[*k], 0.));
		return(s / (48 * ax[*k] * ay[*k] * az[*k]));
	} 
	else if ((fabs(ax[*k]) <= precision)) return(C_sumof2uniforms_cdf(p, ay, az, k));
	else if ((fabs(ay[*k]) <= precision)) return(C_sumof2uniforms_cdf(p, ax, az, k));
	else return(C_sumof2uniforms_cdf(p, ax, ay, k));
}


void C_rss2Dloop(const double *x, const double *y, const double *ax, const double *ay, const int *n, const int *n_theta, double *ans) {
	
	double p;
	
	for (int i_theta=0; i_theta < *n_theta; i_theta++) {
			
		for (int i=0; i < *n; i++) {
			p = x[i]*ax[i_theta] + y[i]*ay[i_theta];
			ans[i + *n * i_theta] = C_sumof2uniforms_cdf(&p, ax, ay, &i_theta);
		}
		
	}

}


void C_rss3Dloop(const double *x, const double *y, const double *z, const double *ax, const double *ay, const double *az, const int *n, const int *n_theta, const int *n_phi, const int *i_phi, double *ans) {
	
	double p;
	int k;
	
	for (int i_theta=0; i_theta < *n_theta; i_theta++) {
			
		for (int i=0; i < *n; i++) {
			k = i_theta + *n_theta * *i_phi;
			p = x[i]*ax[k] + y[i]*ay[k] + z[i]*az[k];
			ans[i + *n * i_theta] = C_sumof3uniforms_cdf(&p, ax, ay, az, &k);
		}
		
	}
}





