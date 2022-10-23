/*
 * Copyright (c) 2022 Ricardo Yanez <ricardo.yanez@calel.org>
 *
 * Part of a C wrapper to the NAG Fortran Library
 *
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 *
 */

#include <stdio.h>
#include <stdlib.h>

/*
 * X05BAF returns the amount of processor time used since an unspecified 
 * previous time, via the routine name.
 *
 * This wapper uses the GNU C Library function clock(). 
 * Dividing clock() by CLOCKS_PER_SEC gives the processor time in seconds.
 *
 */

#include <time.h>

double c_x05baf_() {
  return (double)clock()/CLOCKS_PER_SEC;
}

/*
 * S15ADF returns the value of the complementary error function, erfc x,
 * via the routine name.
 *
 * This wapper uses the GNU C Library function erfc().
 *
 */

#include <math.h>
#include <fenv.h>

double c_s15adf_(double *x, int *ifail) {

  double f;
  feclearexcept(FE_ALL_EXCEPT);
  f = erfc(*x);
  *ifail = 0;
  if ( fetestexcept(FE_INVALID|FE_DIVBYZERO|FE_OVERFLOW|FE_UNDERFLOW) ) {
    *ifail = 1;
  }
  return f;
}
