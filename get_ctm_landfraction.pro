; $Id$
;-----------------------------------------------------------------------
;+
; NAME:
;        GET_CTM_LANDFRACTION
;
; PURPOSE:
;        This program retrieves the area fraction of GEOS grids which is land.
;        1 = box area is entirely land
;        0 = box area is entirely water. This includes fresh and salt water.
;        The land map is derived from the Olson land use map, which is used
;        for dry deposition in GEOS-Chem (rdland.f).
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        LandFraction = GET_CTM_LANDFRACTION( GridInfo )
;
; INPUTS:
;        GridInfo - a CTM grid structure. Currently only supports GEOS 
;                   grid at 2x2.5 or 4x5 resolution.
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;        LandFraction - a 2D array of the area fraction of land within each
;                       grid square. The array dimensions are the same as
;                       the LON x LAT array for the corresponding GEOS grid.
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;        Requires the files LAND.2x25.txt, LAND.4x5.txt
;
; EXAMPLE:
;        GridInfo = ctm_grid( ctm_type( 'GEOS5', resolution=4 ) )
;        Land = get_ctm_landfraction( GridInfo )
;
;        tvmap, Land, GridInfo.xmid, GridInfo.ymid, /sample, $
;               title='Land Fraction', /cbar
;
;
; MODIFICATION HISTORY:
;        cdh, 26 Aug 2008: VERSION 1.00
;
;-
; Copyright (C) 2008, Christopher Holmes, Harvard University
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to cdh@io.as.harvard.edu
; with subject "IDL routine get_ctm_landfraction"
;-----------------------------------------------------------------------


function get_ctm_landfraction, GridInfo
 
   ; Directory containing files
   dir = !BENCHMARK+'/GEOS-Chem_fields/'

   Case GridInfo.DJ of
      2: file=dir+'LAND.2x25.txt'
      4: file=dir+'LAND.4x5.txt'
   Endcase
 
   LandFraction = fltarr( GridInfo.IMX, GridInfo.JMX )
 
   junk=''
 
   OPENR, LUN, FILE, /GET_LUN
 
   ; Skip first line, a header
   READF, LUN, junk
 
   ; The remainder is data
   READF, LUN, LandFraction
 
   FREE_LUN, LUN
   
   Return, LandFraction
end
