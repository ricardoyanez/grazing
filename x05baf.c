/*
 * Copyright (c) 2022 Ricardo Yanez <ricardo.yanez@calel.org>
 *
 * C wrapper to NAG Fortran Library X05BAF Routine
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
#include <time.h>

/*
 * X05BAF returns the amount of processor time used since an unspecified 
 * previous time, via the routine name.
 *
 * This wapper uses the C function clock(). Dividing by CLOCKS_PER_SEC
 * gives the number of seconds.
 *
 */

void nag_x05baf_(double *elaps) {
  *elaps = (double)clock()/CLOCKS_PER_SEC;
  return;
}
