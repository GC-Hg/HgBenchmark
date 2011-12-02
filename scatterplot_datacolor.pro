; $Id$
;-----------------------------------------------------------------------
;+
; NAME:
;        SCATTERPLOT_DATACOLOR
;
; PURPOSE:
;        This program makes an X-Y scatterplot with points that are
;        colored by a third variable Z. This program calculates the
;        colors for each point based on the currently active
;        colortable. Also adds a colorbar.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        SCATTERPLOT_DATACOLOR, X, Y, Z[, Keywords]
;
; INPUTS:
;        X, Y, Z - Array of the same length. If Z has missing data
;                  (NaNs), then those points are not plotted.
;
; KEYWORD PARAMETERS:
;        Zmin, Zmax - These keywords specify the maximum and minimum
;                     values of the Z variable which should be
;                     plotted. They also specify the upper and lower
;                     range of the colorbar. Z values outside this range
;                     will have the same plot color as Zmin or
;                     Zmax. Default values are max(Z) and min(Z).
;
;        Title      - Title for the plot, passed to Plot
;
;        NoCB       - Suppress the colorbar. Default is to display it.
;
;        CBposition - position for the colorbar. Default is in the
;                     lower right corner of the plot area.
; 
;        CBabspos   - Set CBabspos=1 so that CBposition will use 
;                     position on the page. Otherwise, the 
;                     coordinates are within the plotting axes 
;        
;        CBTitle    - title for the colorbar, avoids confusion with the plot
;                     title
;
;        CBCharsize - size for the text used in colorbar labels
;
;        Divisions  - number of divisions in the colorbar. Default is
;                     7. 
;
;        _EXTRA     - Other keywords are passed to PLOTS (via SCATTERPLOT),
;                     COLORBAR and BYTSCL  
; 
; OUTPUTS:
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;        bmy, 13 Apr 2008: VERSION 1.00
;
;-
; Copyright (C) 2008, Bob Yantosca, Harvard University
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to bmy@io.as.harvard.edu
; or phs@io.as.harvard.edu with subject "IDL routine scatterplot_datacolor"
;-----------------------------------------------------------------------


pro scatterplot_datacolor, X, Y, Z, Zmin=Zmin, Zmax=Zmax, $
                           CBposition=CBposition, Divisions=Divisions, $
                           CBtitle=CBtitle, NoCB=NoCB, Title=Title, $ 
                           CBabspos=CBabspos, CBcharsize=CBcharsize, $
                           C_Levels=C_levels, _Extra=_Extra

   ; Default values
   If N_elements( Zmax ) eq 0 then Zmax = max( Z,/NaN )
   If N_elements( Zmin ) eq 0 then Zmin = min( Z,/NaN )
   If not Keyword_Set( CBposition ) then CBposition = [0.4,0.15,0.8,0.2]
   If not Keyword_Set( Divisions )  then Divisions =7


   If Keyword_set( C_Levels ) then begin

        NCL = N_Elements( C_Levels ) ; Number of contour levels

         ; Scale Colors to the number of C_Levels - 1. Preserve the
         ; beginning, middle, and end colors. This is great for 
         ; blue-white-red colorbars, but other colortables as well 
         ; since it ensures that one end (the left)
         ; will not dominate the new colorbar
         NCol = NCL - 1 ; Number of colors
         CC_Colors = IntArr( NCol )       

         ; Find the middle of CC_Colors and interpolate
         ; beginning to middle and middle to end.
         Middle = Fix( ( NCol - 1 ) / 2 )
         CC_Colors[ 0:Middle ] = $
           Interpol( [!Myct.Bottom, $
                      !Myct.Bottom + Fix( !Myct.NColors / 2 ) ], $
                     Middle+1 )
         CC_Colors[ Middle:NCol-1 ] = $
           Interpol( [!Myct.Bottom + Fix( !Myct.NColors / 2), $
                      !Myct.Bottom + !Myct.NColors-1], $
                     NCol-Middle )

         ; Use value_locate to identify what contour bin the data 
         ; belongs within. If data < min(C_Levels) value_locate will 
         ; return -1. Find these values and make them the minimum color.
         ; Bins contains the index number for CC_Colors.
         Bins = Value_Locate( C_Levels, Z )
         Lt_Min = Where( Bins eq -1, Num_Lt_Min )
         if( Num_Lt_Min gt 0 ) then Bins[ Lt_Min ] = 0

         N = n_elements(Z)
         Z_color = fltarr( N )
         for i=0L, N-1L do $
            Z_color[i] = cc_colors[ min( [Bins[i], NCol-1] ) ] 

   endif else begin
      ; Calculate the color values for each datum
      ; Scale the data to lie within the range of the current color table
      ; Use floats to accommodate NaN (missing data)
      Z_color = float( bytscl( Z, top=!Myct.Ncolors-1L, /NaN, $
                               min=Zmin, max=Zmax, _Extra=_Extra ) + $
                       !Myct.bottom )

   endelse

   ; Check if there are any points with NaN (missing data)
   ind = where( ~finite( Z ), ct )

   ; Set color of mising data to NaN, so that it doesn't plot.
   If ( ct gt 0 ) then $
     Z_color[ ind ] = !Values.f_NaN
 
   ; Plot the points with their colors
   scatterplot, X, Y, color=Z_color, Title=Title, _Extra=_Extra

   ; Save the system plot setting so that we can reactivate
   ; the plot axes after plotting the colorbar
   xx = !X
   yy = !Y
   pp = !P

   ; Set colorbar position relative to page
   If Keyword_set( CBabspos ) then begin
      CBpagepos = CBposition
   endif else begin
      CBpagepos = CBposition
      ; Width,height of the plot window
      dx = !X.window[1]-!X.window[0]
      dy = !Y.window[1]-!Y.window[0]
      ; Position of the colorbar within the plot window
      CBpagepos[[0, 2]] = CBposition[[0, 2]]*dx +!X.window[0]
      CBpagepos[[1, 3]] = CBposition[[1, 3]]*dy +!Y.window[0]
   endelse

   ; Add a colorbar, unless NoCB keyword is set
   If not Keyword_Set( NoCB ) then begin

      If keyword_set( c_levels ) then begin
         colorbar, c_levels=c_levels, divisions=divisions, $
                   position=CBpagepos, $
                   title=CBtitle, charsize=CBcharsize, _Extra=_Extra

      endif else begin
         colorbar, min=Zmin, max=Zmax, divisions=divisions, $
                   position=CBpagepos, $
                   title=CBtitle, charsize=CBcharsize, _Extra=_Extra
      endelse

   endif

   ; Restore the plot settings to reactivate the plot axes
   ; this makes overplotting possible
   !X = xx
   !Y = yy
   !P = pp

 
end
