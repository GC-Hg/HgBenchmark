; $Id$
;-----------------------------------------------------------------------
;+
; NAME:
;        SCATTERPLOT
;
; PURPOSE:
;        Make a scatterplot, simplifies plotting, overplotting, and
;        defaults to filled circles
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        SCATTERPLOT, X, Y [, Keywords]
;
; INPUTS:
;        X, Y - Arrays of values to plot 
;
; KEYWORD PARAMETERS:
;       OverPlot - Plots new points on top of existing axes, 
;                  Default: erase plot area
;       Color    - Scalar or Array of color indices
;                  If Color is a scalar, then all points will have the 
;                  same color. If Color is an Array (must
;                  have same dimension as X), then the elements give
;                  the color of each point.
;                  Default: all points are black
;                  Indices with NaN values are not plotted.
;       Psym     - Plotting symbol
;                  Default: filled circle
;       Sym      - Plotting symbol as an argument to sym(). This supercedes psym
;       Outline  - Draw outline for the plotting symbols.
;                  Outline only works with the SYM keyword or default plotting
;                  symbol. Outline does not work with psym keyword.
;       AxisColor - Color index for axes
;                   Default: black
;       NoClip   - If NoClip is TRUE, then points outside the graph axes 
;                  will be plotted. Default is NOT to plots these points.
;       _Extra   - Additional keywords are passed to the plotting commands
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
;        cdh, 12 Jul 2010: added NoClip keyword
;        cdh, 03 Aug 2007: VERSION 1.00
;
;-
; Copyright (C) 2007, Christopher Holmes, Harvard University
; This software is provided as is without any warranty
; whatsoever. It may be freely used, copied or distributed
; for non-commercial purposes. This copyright notice must be
; kept with any copy of this software. If this software shall
; be used commercially or sold as part of a larger package,
; please contact the author.
; Bugs and comments should be directed to cdh@io.as.harvard.edu
; with subject "IDL routine point_plot"
;-----------------------------------------------------------------------

pro scatterplot, x, y, OverPlot=OverPlot, Color=Color, psym=psym, $
                 Outline=Outline, SymNum=SymNum, $
         AxisColor=AxisColor, NoClip=NoClip, _Extra=_Extra
 
   ; Default to use black
   if not Keyword_Set( color ) then color = !myct.black

   ; Default to use black axes
   if not Keyword_set( AxisColor ) then AxisColor = !myct.black
   
   ; Default to clip points outside the plot axes
   if not Arg_present( NoClip ) then NoClip=0

   ; Default to plot filled circles, if neither psym or sym keywords are used
   if ( not Keyword_Set( psym ) and not Keyword_set( SymNum ) ) then SymNum = 1

   ; Use sym() function if SYM keyword is present
   useSym = Keyword_set( SymNum )
   
   ; Plot outlines only if requested and we are using the sym() function
   Outline = Keyword_set( Outline ) and useSym
    
   ; Clear the window for new plot, unless OverPlot keyword is set
   if not Keyword_Set( OverPlot ) then begin
 
      ; Set up Axes, but show no data
      plot, x, y, color=1, /noData, _Extra=_Extra
      
   endif
 
   ; Check whether all points are the same color
   n_colors = n_elements( color )
 
   ; Plot all points in same color if only one color is given
   if ( n_colors eq 1 ) then begin
 
      ; Make OverPlot
      if (useSym) then $
         oplot, x, y, color=color, psym=sym(symNum), NoClip=NoClip, $
                _Extra=_Extra $
      else $
         oplot, x, y, color=color, psym=psym, NoClip=NoClip, _Extra=_Extra
 
      ; Draw outline around plotting symbol
      if (Outline) then $
         oplot, x, y, color=AxisColor, psym=sym(symNum+5), NoClip=NoClip, $
                _Extra=_Extra
         

   ; Plot points in multiple colors if array of colors is passed
   endif else begin
 
      ; Error if n_colors is not the same as number of points
      if ( n_colors ne n_elements( x ) ) then $
         message, 'Number of colors must be 1 or same as X!!'
 
      ; Plot each point sequentially
      for i=0L, n_colors-1L do begin
      
         ; Skip plotting this point if it is NaN or Inf
         If not finite( color[i] ) then continue
         
         ; Plot point in given color
         if (useSym) then $
            plots, x[i], y[i], color=color[i], psym=sym(symNum), $
                   NoClip=NoClip, _Extra=_Extra $
         else $
            plots, x[i], y[i], color=color[i], psym=psym, NoClip=NoClip, $
                   _Extra=_Extra
            
         ; Draw outline around plotting symbol
         if (Outline) then $
            oplot, [x[i]], [y[i]], color=AxisColor, psym=sym(symNum+5), $
                   NoClip=NoClip, _Extra=_Extra
   
      endfor
 
   endelse
 
end
