pro plot_usa_wetdep_seasonal, FileName   = FileName  , $
                              psFileName = psFileName, $
                              PS         = PS        , $
                              MDNyear    = MDNyear_in, $
                              lonrange   = lonrange  , $
                              Divisions  = Divisions , $
                              Reference  = Reference

;==================================================================
; PURPOSE   
;  Plot annual total wet deposition over the United States
;  Compare with MDN data
;
; INPUTS
;  FileName   : file name(s) of bpch output from GEOS-Chem
;  psFileName : save PostScript image under this file name
;  PS         : 
;  MDNyear    : year(s) of MDN observations you'd like to compare
;               the model aganist. If left blank, will automatically
;               select available MDN years that match the year of
;               your model run from FileName.
;  lonrange   :
;  Divisions  : 
;  Reference  : file name(s) of bpch output from GEOS-Chem that
;               you'd like to compare aganist the simulation from
;               FileName. 
;
; OUTPUTS
;  Plot of seasonal wet deposition over the USA. 
;
; EXAMPLES
;  Plot MERRA and GEOS5 2007-2009 mean Hg wet dep vs MDN obs.
;  IDL> merra1='~hamos/amos2011/runs/MERRA.base/ctm.bpch.2007'
;  IDL> merra2='~hamos/amos2011/runs/MERRA.base/ctm.bpch.2008'
;  IDL> merra3='~hamos/amos2011/runs/MERRA.base/ctm.bpch.2009'
;  IDL> merra = [merra1, merra2, merra3]
;  IDL> geos1 ='~hamos/amos2011/runs/GEOS5.base/ctm.bpch.2007'
;  IDL> geos2 ='~hamos/amos2011/runs/GEOS5.base/ctm.bpch.2008'
;  IDL> geos3 ='~hamos/amos2011/runs/GEOS5.base/ctm.bpch.2009'
;  IDL> geos  = [geos1, geos2, geos3]
;  IDL> plot_usa_wetdep_seasonal, filename=merra, reference=geos
;
; MODFICATION HISTORY
;         2010 - H Amos - inherited code from C Holmes
;  18 Apr 2011 - H Amos - modify code to plot mutiple model simulations
;                         as different color lines, like Figure 10 in
;                         Holmes et al, 2010 
; 05 Jun 2011 - H Amos - replace CompareFile keyword with Reference,
;                        to be consistent with benchmark scripts
;         
;====================================================================


   ;=======================================
   ; Setup
   ;=======================================

   if ( ~ Keyword_set( FileName ) ) then $
      FileName = '~/runs/HgBr.v8-01-01/ctm.bpch.stdOx'

   if Keyword_set( psFileName ) then $
      PS = 1L $
   else $
      PS = 0L

   if ( ~ Keyword_set( psFileName ) ) then $
      psFileName = 'wetdep_USA_seasonal.ps' 
   
   if ( ~ Keyword_set( Divisions ) ) then $
      Divisions = 5 

   ; Range of Longitudes to extract
   if ( ~ keyword_set( lonRange ) ) then $
      lonRange=[-92.5, -72.5]
   
   ; File with Annual mean MDN data
   MDNfile = !BENCHMARK+'/data/MDNmonthly.sav'

   ; Range of wet deposition to plot, ug/m2/y
   Range = [0, 20]

   ; Convert kg to ug
   kg2ng = 1d12

   ; convert seconds to days
   sec2day = 86400.

   ;=======================================
   ; Read MDN data
   ;=======================================
   
;    MDNsite = ''
;    MDNlat = 0.
;    MDNlon = 0.
;    MDNi = 1L
;    MDNj = 1L
;    MDNelev = 0.
;    d1995 = 0.
;    d1996 = 0.
;    d1997 = 0.
;    d1998 = 0.
;    d1999 = 0.
;    d2000 = 0.
;    d2001 = 0.
;    d2002 = 0.
;    d2003 = 0.
;    d2004 = 0.

;    transread, lun,filename=MDNfile, skiplines=1, /debug, $
;               MDNsite, MDNlat, MDNlon, MDNi, MDNj, MDNelev, $
;               d1995, d1996, d1997, d1998, d1999, $
;               d2000, d2001, d2002, d2003, d2004, $
;               format='(A4,2(X,F0.0),2(X,I0),11(X,F0.0))'

;    ; Combine MDN data into one array
;    MDN = [[d1995], [d1996], [d1997], [d1998], [d1999], $
;           [d2000], [d2001], [d2002], [d2003], [d2004]]

;    ; Convert ng/m2/y -> ug/m2/y
;    MDN = MDN/1E3
;    MDNyear = [1995, 1996, 1997, 1998, 1999, $
;               2000, 2001, 2002, 2003, 2004]

   ; Restore MDN structure
   restore, MDNfile, /verbose



   ;=======================================
   ; Extract GEOS-Chem Data
   ;=======================================


   ; Wet deposition, ug/m2/d
   WetDep = get_total_wetdep( FileName = FileName      , $
                              species  = ['Hg2', 'HgP'], $
                              GridInfo = GridInfo      , $
                              Year     = Year          , $
                              /ByMonth                    )
   
   ; Convert ug/m2/d -> ng/m2/d
   WetDep = WetDep * 1e3

   ii = where( (GridInfo.xmid ge lonRange[0]) and (GridInfo.xmid le lonRange[1]) ) 
   jj = where( (GridInfo.ymid ge 28) and (GridInfo.ymid le 48) )


   ; Wet deposition for regions in eastern US
   usa_wetdep = wetdep[ ii, *, *]
   usa_wetdep = usa_wetdep[*, jj, *]
   usa_wetdep = mean( usa_wetdep, 1 )

   ; Repeat for central US
 ;  ii2 = where( (GridInfo.xmid ge -92.5) and (GridInfo.xmid le -82.5) ) 
 ;  usa_wetdep2 = wetdep[ ii2, *, *]
 ;  usa_wetdep2 = usa_wetdep2[*, jj, *]
 ;  usa_wetdep2 = mean( usa_wetdep2, 1 )

   ; Combine
;   usa_wetdep = [ usa_wetdep2,  usa_wetdep ]

   if Keyword_set( Reference ) then begin
      ; Wet deposition, ug/m2/d
      WetDep2 = get_total_wetdep( FileName = Reference     , $
                                  species  = ['Hg2', 'HgP'], $
                                  GridInfo = GridInfo      , $
                                  Year     = Year          , $
                                  /ByMonth                    )
   
      ; Convert ug/m2/d -> ng/m2/d
      WetDep2 = WetDep2 * 1e3

      ii2 = where( (GridInfo.xmid ge lonRange[0]) and (GridInfo.xmid le lonRange[1]) ) 
      jj2 = where( (GridInfo.ymid ge 28) and (GridInfo.ymid le 48) )


      ; Wet deposition for regions in eastern US
      usa_wetdep2 = wetdep2[ ii2, *, *]
      usa_wetdep2 = usa_wetdep2[*, jj2, *]
      usa_wetdep2 = mean( usa_wetdep2, 1 )


   endif

   ; Latitudes for plotting
   lat = GridInfo.ymid[jj]

   ;=======================================
   ; Get MDN data for the same year as model
   ;=======================================

   ; Assume that FileName and Reference are the same model years
   
   if keyword_set( MDNyear_in ) then $
      year = MDNyear_in

   PlotMDN =  0L
  ; if (Gridinfo.di eq 4) then
   ; Boxes are centered at 30, 34, 38, 42, 46
  ; centerLat = [30, 34, 38, 42, 46]
   centerLat=gridinfo.ymid[jj2]
  ;endif


   nLats = n_elements( centerLat )

   ; days per month
   dpm = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

   pMDN = fltarr( nLats, 12 )
   pMDNerr = fltarr( nLats, 12 )
   pMDN[*] =  !values.f_nan
   pMDNerr[*] =  !values.f_nan
   ctMDN = fltarr( nLats )

   for J=0L, nLats-1L do begin

      yearmatch=0L
      for y=0L, n_elements( year )-1L do begin
         yearmatch = ( MDN.year eq year[Y] ) + yearmatch 
      endfor
      yearmatch = yearmatch < 1L

       ; When using MDNfullmonthly.sav
;      ii = where( yearmatch and $;(MDN.year eq year) and $
;                  (abs(MDN.lat-CenterLat[J]) le 2 ) and $
;                  (MDN.lon ge lonRange[0]) and (MDN.lon le lonRange[1]), ct )
         
      ; When using MDNmonthlys.sav
      ii = where( (abs(MDN.lat-CenterLat[J]) le 2 ) and $
                  (MDN.lon ge lonRange[0]) and (MDN.lon le lonRange[1]), ct )
      jj = where( yearmatch )


      if (ct ge 1) then begin
      
         ; When using MDNfullmonthly.sav
;         pMDN[J,*] = mean_nan( MDN.HgDep[ii, *], 1 )
;         pMDNerr[J,*] = stddev2( MDN.HgDep[ii, * ], 1 )

         ; When using MDNmonthly.sav
         for M=0, 11L do begin
            pMDN[J,M] = mean_nan( ((MDN.HgDep[ii, *, *])[*, jj, *])[*, *, M])
            pMDNerr[J,M] = stddev2( ((MDN.HgDep[ii, *, *])[*, jj, *])[*, *, M], /nan)
         endfor

         ; Convert ng/m2/mo -> ng/m2/d
         pMDN[J,*] = pMDN[J,*] / dpm
         pMDNerr[J,*] = pMDNerr[J,*] / dpm

         ctMDN[J] = ct


         PlotMDN = 1L

      endif

   endfor




   ;=======================================
   ; Plotting
   ;=======================================

   ; First letter of each month
   monthstr=['J','F','M','A','M','J','J','A','S','O','N','D']  

   ; Number of plots is the number of north-south regions
   nrows = (size(usa_wetdep, /dim))[0]

   If Keyword_Set( PS ) then $
      ps_setup, /open, file=psFileName, xsize=4, ysize=7
   
   multipanel, col=1, row=1,  omargin=[0.05, 0.05, 0.1, 0.1], $
               margin=0, position=pos

   map_set, /noerase, color=!myct.gray50,  position=pos, $
            limit=[28, lonRange[0], 48, lonRange[1]], /usa

   multipanel, /off

   multipanel, cols=1, rows=nrows, omargin=[0.05, 0.05, 0.1, 0.1], $
               margin=0, /noerase

   xmid = GridInfo.xmid
   ymid = GridInfo.ymid

   for I = 0, nrows-1L do begin

      II = nrows-1L-I

      multipanel, position=pos, /noerase

      plot, usa_wetdep[II,*], /color, position=pos, /noerase, $
            yrange=[0, 100], /ystyle, yticks=5, $
            xrange=[-0.5, 11.5], /xstyle, $
            xticks=11, xtickv=indgen(12), xtickname=monthstr, $
            ytitle='Wet deposition, ng m!U-2!N d!U-1', $
            title=string(lat[II], format='(I2,"N")')+$
                  string(ctMDN[II], format='(I2," sites")' )

      oplot, usa_wetdep[II, * ], color=2,  thick=3

      if Keyword_set( Reference ) then $
         oplot,  usa_wetdep2[II, *],  color=4,  thick=3

      if Keyword_set( PlotMDN ) then begin
         oplot, pMDN[II, *], /color,  thick=3
         errorbar, indgen(12), pMDN[II, *], pMDNerr[II, * ], $
                   color=1, /noerase
      endif

      multipanel, /advance, /noerase

   endfor

   ; Make legend
   if ( ~ Keyword_set( Reference ) ) then begin
   legend, label=['MDN', 'Model' ], $
           line=[0, 0], lcolor=[1, 2], $
           halign=.8, valign=.8, charsize=.8, /color, /frame
   endif else begin
   legend, label=['MDN', 'New Model', 'Old Model'], $
;   legend, label=['MDN', 'MERRA base', 'MG wetscav'], $
           line=[0, 0, 0], lcolor=[1, 2, 4], $
           halign=.8, valign=.8, charsize=.8, /color, /frame 
   endelse

   multipanel, /off

   If Keyword_Set( PS ) then $
      ps_setup, /close


end
