/*
   scale.c - image scaling

   Raster graphics library

   Copyright (c) 1997, 1988, 1999 Alfredo K. Kojima

   This file is part of the cocoa Library and is provided
   under the terms of the GNU Library General Public License.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>

#include "CGImage.h"


#ifndef PI
#define PI 3.14159265358979323846
#endif
typedef int (*__compar_fn_t) (__const void *, __const void *);

void *
_lfind (const void *key, const void *base, size_t *nmemb, size_t size,
       __compar_fn_t compar)
{
  const void *result = base;
  size_t cnt = 0;

  while (cnt < *nmemb && (*compar) (key, result) != 0)
    {
      result += size;
      ++cnt;
    }

  return cnt < *nmemb ? (void *) result : NULL;
}

RImage*
RCreateImage2(unsigned width, unsigned height, int alpha)
{
RImage *image;
int size = width * height * (alpha ? 4 : 3) + 4;

    assert(width>0 && height>0);
			// the +4 is to give extra bytes at the end of the buffer, so that 
			// we can optimize image conversion for MMX(tm).. see convert.c
    if (!(image = calloc(1, sizeof(RImage))) || !(image->idata = malloc(size)))
        return NULL;

    image->width = width;
    image->height = height;
    image->format = alpha ? RRGBAFormat : RRGBFormat;

    return image;
}

/*
 *----------------------------------------------------------------------
 * RScaleImage--
 * 	Creates a scaled copy of an image.
 * 
 * Returns:
 * 	The new scaled image.
 * 
 *---------------------------------------------------------------------- 
 */
RImage*
RScaleImage(RImage *image, unsigned new_width, unsigned new_height)
{
int ox, px, py;
register int x, y, t;
int dx, dy;
unsigned char *s;
unsigned char *d;
RImage *img;

    assert(new_width >= 0 && new_height >= 0);

    if (new_width == image->width && new_height == image->height)
		return image;

    img = RCreateImage2(new_width, new_height, image->format == RRGBAFormat);
    
    if (!img)
      return NULL;

    /* fixed point math idea taken from Imlib by 
     * Carsten Haitzler (Rasterman) */
    dx = (image->width<<16)/new_width;
    dy = (image->height<<16)/new_height;

    py = 0;
    
    d = img->idata;

    if (image->format == RRGBAFormat)
		{
		for (y=0; y<new_height; y++)
			{
			t = image->width*(py>>16);

			s = image->idata+(t<<2); /* image->idata+t*4 */

			ox = 0;
			px = 0;
			for (x=0; x<new_width; x++)
				{
				px += dx;

				*(d++) = *(s);
				*(d++) = *(s+1);
				*(d++) = *(s+2);
				*(d++) = *(s+3);

				t = (px - ox)>>16;
				ox += t<<16;

				s += t<<2; /* t*4 */
				}
			py += dy;
			}
		}
	else
		{
		for (y=0; y<new_height; y++)
			{
			t = image->width*(py>>16);

			s = image->idata+(t<<1)+t; /* image->idata+t*3 */

			ox = 0;
			px = 0;
			for (x=0; x<new_width; x++)
				{
				px += dx;

				*(d++) = *(s);
				*(d++) = *(s+1);
				*(d++) = *(s+2);

				t = (px - ox)>>16;
				ox += t<<16;

				s += (t<<1)+t; /* t*3 */
				}
			py += dy;
			}
		}
    
    return img;
}


/*
	Filtered Image Rescaling code copy/pasted from
	Graphics Gems III
	Public Domain 1991 by Dale Schumacher
*/


/*
 *	filter function definitions
 */
#define	box_support			(0.5)
#define	triangle_support	(1.0)
#define	bell_support		(1.5)
#define	B_spline_support	(2.0)
#define	Lanczos3_support	(3.0)
#define	Mitchell_support	(2.0)

#define	B	(1.0 / 3.0)
#define	C	(1.0 / 3.0)


static double
box_filter(double t)
{
	return ((t > -0.5) && (t <= 0.5)) ? (1.0) : (0.0);
}

static double
triangle_filter(double t)
{
    if(t < 0.0)
		t = -t;

	return (t < 1.0) ? (1.0 - t) : (0.0);
}

static double
bell_filter(double t)		/* box (*) box (*) box */
{
    if(t < 0) t = -t;
    if(t < .5) return(.75 - (t * t));
    if(t < 1.5) {
	t = (t - 1.5);
	return(.5 * (t * t));
    }
    return(0.0);
}

static double
B_spline_filter(double t)	/* box (*) box (*) box (*) box */
{
double tt;
    
    if(t < 0)
		t = -t;
    if(t < 1)
		{
		tt = t * t;
		return ((.5 * tt * t) - tt + (2.0 / 3.0));
		}
	if (t < 2)
		{
		t = 2 - t;
		return ((1.0 / 6.0) * (t * t * t));
		}
    return (0.0);
}

static double
sinc(double x)
{
    x *= PI;

	return (x != 0) ? (sin(x) / x) : (1.0);
}

static double
Lanczos3_filter(double t)
{
    if(t < 0)
		t = -t;

	return (t < 3.0) ? (sinc(t) * sinc(t/3.0)) : (0.0);
}

static double
Mitchell_filter(double t)
{
double tt;
    
    tt = t * t;
    if(t < 0) t = -t;
    if(t < 1.0) {
	t = (((12.0 - 9.0 * B - 6.0 * C) * (t * tt))
	     + ((-18.0 + 12.0 * B + 6.0 * C) * tt)
	     + (6.0 - 2 * B));
	return(t / 6.0);
    } else if(t < 2.0) {
	t = (((-1.0 * B - 6.0 * C) * (t * tt))
	     + ((6.0 * B + 30.0 * C) * tt)
	     + ((-12.0 * B - 48.0 * C) * t)
	     + (8.0 * B + 24 * C));
	return(t / 6.0);
    }
    return(0.0);
}

static double (*filterf)() = Mitchell_filter;
static double fwidth = Mitchell_support;

void
_wraster_change_filter(int type)
{
    switch (type)
		{
		case RBoxFilter:
			filterf = box_filter;
			fwidth = box_support;
			break;
		case RTriangleFilter:
			filterf = triangle_filter;
			fwidth = triangle_support;
			break;
		case RBellFilter:
			filterf = bell_filter;
			fwidth = bell_support;
			break;
		case RBSplineFilter:
			filterf = B_spline_filter;
			fwidth = B_spline_support;
			break;
		case RLanczos3Filter:
			filterf = Lanczos3_filter;
			fwidth = Lanczos3_support;
			break;
		default:
			case RMitchellFilter:
			filterf = Mitchell_filter;
			fwidth = Mitchell_support;
			break;
		}
}

/*
	image rescaling routine
*/

typedef struct {
    int pixel;
    double	weight;
} Contrib;

typedef struct {
    int	n;			/* number of contributors */
    Contrib	*p;		/* pointer to list of contributions */
} Clist;

Clist *ctrb;		/* array of contribution lists */

/* clamp the input to the specified range */
#define CLAMP(v,l,h)    ((v)<(l) ? (l) : (v) > (h) ? (h) : v)


/* return of calloc is not checked if NULL in the function below! */
RImage*
RSmoothScaleImage(RImage *src, unsigned new_width, unsigned new_height)
{    
RImage *tmp;					/* intermediate image */
double xscale, yscale;			/* zoom scale factors */
int i, j, k;					/* loop variables */
int n;							/* pixel number */
double center, left, right;		/* filter calculation variables */
double width, fscale;			/* filter calculation variables */
double rweight, gweight, bweight;
unsigned char *p;
unsigned char *sp;
int sch = src->format == RRGBAFormat ? 4 : 3;
RImage *dst = RCreateImage2(new_width, new_height, 0);

    /* create intermediate image to hold horizontal zoom */
    tmp = RCreateImage2(dst->width, src->height, 0);
    xscale = (double)new_width / (double)src->width;
    yscale = (double)new_height / (double)src->height;

    /* pre-calculate filter contributions for a row */
    ctrb = (Clist *)calloc(new_width, sizeof(Clist));
    if (xscale < 1.0)
		{
		width = fwidth / xscale;
		fscale = 1.0 / xscale;
		for (i = 0; i < new_width; ++i)
			{
			ctrb[i].n = 0;
			ctrb[i].p = (Contrib *)calloc((int)(width * 2 + 1), sizeof(Contrib));
			center = (double) i / xscale;
			left = ceil(center - width);
			right = floor(center + width);
			for(j = left; j <= right; ++j)
				{
				rweight = center - (double) j;
				rweight = (*filterf)(rweight / fscale) / fscale;
				if(j < 0)
					n = -j;
				else if(j >= src->width)
					n = (src->width - j) + src->width - 1;
				else
					n = j;
				k = ctrb[i].n++;
				ctrb[i].p[k].pixel = n*sch;
				ctrb[i].p[k].weight = rweight;
				}
			}
		}
	else
		{
		for(i = 0; i < new_width; ++i)
			{
			ctrb[i].n = 0;
			ctrb[i].p = (Contrib *)calloc((int) (fwidth * 2 + 1), sizeof(Contrib));
			center = (double) i / xscale;
			left = ceil(center - fwidth);
			right = floor(center + fwidth);
			for(j = left; j <= right; ++j) {
			rweight = center - (double) j;
			rweight = (*filterf)(rweight);
			if(j < 0) {
			n = -j;
			} else if(j >= src->width) {
			n = (src->width - j) + src->width - 1;
			} else {
			n = j;
			}
			k = ctrb[i].n++;
			ctrb[i].p[k].pixel = n*sch;
			ctrb[i].p[k].weight = rweight;
			}
			}
		}
    
    /* apply filter to zoom horizontally from src to tmp */
    p = tmp->idata;

    if (xscale < 1.0 && src->format == RGRAYFormat)
		{
		unsigned char *limit = src->idata + (src->width * src->height);

		for(k = 0; k < tmp->height; ++k)
			{
			Contrib *pp;
	
			sp = src->idata + src->width * k * sch;
		
			for(i = 0; i < tmp->width; ++i)
				{
				rweight = gweight = bweight = 0.0;
				pp = ctrb[i].p;
				
				for(j = 0; j < ctrb[i].n; ++j)
					{
					if (sp + pp[j].pixel < limit)	// FIX ME looks and is dangerous
						{
						rweight += sp[pp[j].pixel] * pp[j].weight;
						gweight += sp[pp[j].pixel+1] * pp[j].weight;
						bweight += sp[pp[j].pixel+2] * pp[j].weight;
						}
					}
				*p++ = CLAMP(rweight, 0, 255);
				*p++ = CLAMP(gweight, 0, 255);
				*p++ = CLAMP(bweight, 0, 255);
				}
			}
		}
	else
		for(k = 0; k < tmp->height; ++k)
			{
			Contrib *pp;
	
			sp = src->idata + src->width * k * sch;
		
			for(i = 0; i < tmp->width; ++i)
				{
				rweight = gweight = bweight = 0.0;
				pp = ctrb[i].p;
				
				for(j = 0; j < ctrb[i].n; ++j)
					{
					rweight += sp[pp[j].pixel] * pp[j].weight;
					gweight += sp[pp[j].pixel+1] * pp[j].weight;
					bweight += sp[pp[j].pixel+2] * pp[j].weight;
					}
				*p++ = CLAMP(rweight, 0, 255);
				*p++ = CLAMP(gweight, 0, 255);
				*p++ = CLAMP(bweight, 0, 255);
				}
			}

    /* free the memory allocated for horizontal filter weights */
    for(i = 0; i < tmp->width; ++i)
		free(ctrb[i].p);
    free(ctrb);
    
    /* pre-calculate filter contributions for a column */
    ctrb = (Clist *)calloc(dst->height, sizeof(Clist));
    if(yscale < 1.0) {
	width = fwidth / yscale;
	fscale = 1.0 / yscale;
	for(i = 0; i < dst->height; ++i) {
	    ctrb[i].n = 0;
	    ctrb[i].p = (Contrib *)calloc((int) (width * 2 + 1), sizeof(Contrib));
	    center = (double) i / yscale;
	    left = ceil(center - width);
	    right = floor(center + width);
	    for(j = left; j <= right; ++j) {
		rweight = center - (double) j;
		rweight = (*filterf)(rweight / fscale) / fscale;
		if(j < 0) {
		    n = -j;
		} else if(j >= tmp->height) {
		    n = (tmp->height - j) + tmp->height - 1;
		} else {
		    n = j;
		}
		k = ctrb[i].n++;
		ctrb[i].p[k].pixel = n*3;
		ctrb[i].p[k].weight = rweight;
	    }
	}
    } else {
	for(i = 0; i < dst->height; ++i) {
	    ctrb[i].n = 0;
	    ctrb[i].p = (Contrib *)calloc((int) (fwidth * 2 + 1), sizeof(Contrib));
	    center = (double) i / yscale;
	    left = ceil(center - fwidth);
	    right = floor(center + fwidth);
	    for(j = left; j <= right; ++j) {
		rweight = center - (double) j;
		rweight = (*filterf)(rweight);
		if(j < 0) {
		    n = -j;
		} else if(j >= tmp->height) {
		    n = (tmp->height - j) + tmp->height - 1;
		} else {
		    n = j;
		}
		k = ctrb[i].n++;
		ctrb[i].p[k].pixel = n*3;
		ctrb[i].p[k].weight = rweight;
	    }
	}
    }

    /* apply filter to zoom vertically from tmp to dst */
    sp = malloc(tmp->height*3);

    for(k = 0; k < new_width; ++k) {
	Contrib *pp;
	
	p = dst->idata + k*3;

	/* copy a column into a row */
	{
	    int i;
	    unsigned char *p, *d;

	    d = sp;
	    for(i = tmp->height, p = tmp->idata + k*3; i-- > 0; p += tmp->width*3)
		{
		*d++ = *p;
		*d++ = *(p+1);
		*d++ = *(p+2);
	    }
	}
	for(i = 0; i < new_height; ++i) {
	    rweight = gweight = bweight = 0.0;
	    
 	    pp = ctrb[i].p;
	    
	    for(j = 0; j < ctrb[i].n; ++j) {
		rweight += sp[pp[j].pixel] * pp[j].weight;
		gweight += sp[pp[j].pixel+1] * pp[j].weight;
		bweight += sp[pp[j].pixel+2] * pp[j].weight;
	    }
	    *p = CLAMP(rweight, 0, 255);
	    *(p+1) = CLAMP(gweight, 0, 255);
	    *(p+2) = CLAMP(bweight, 0, 255);
	    p += new_width*3;
	}
    }
    free(sp);

    /* free the memory allocated for vertical filter weights */
    for(i = 0; i < dst->height; ++i)
		free(ctrb[i].p);

    free(ctrb);
	free(tmp->idata);
	free(tmp);

    return dst;
}
