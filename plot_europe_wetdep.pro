pro plot_europe_wetdep, FileName=FileName, $
                     psFileName=psFileName, $
                     PS=PS, $
                     EMEPyear=EMEPyear_in, $
                     nearest_year=nearest_year,$
                     Divisions=Divisions

; Plot annual total wet deposition over the United States
; Compare with EMEP data

   ;------------------------------------------------;
   ; eds 5/11/11 modified to include reference file ;
   ; for use in mercury model benchmarking          ;
   ; jaf 5/17/11 added nearest_year keyword to plot ;
   ; nearest year of data if no data available      ;
   ;------------------------------------------------;

   ;=======================================
   ; Setup
   ;=======================================

   DataDir = !BENCHMARK+'/data/'

;   if ( not Keyword_set( FileName ) ) then $
;      FileName = '~/runs/HgBr.v8-01-01/ctm.bpch.stdOx'

   if Keyword_set( psFileName ) then $
      PS = 1L $
   else $
      PS = 0L

   if ( not Keyword_set( psFileName ) ) then $
      psFileName = 'wetdep_europe.ps'

   if ( not Keyword_set( Divisions ) ) then $
      Divisions = 5

   ; File with Annual mean EMEP data
;   EMEPfile = DataDir + 'emep_ann_hg_dep_2000s.dat' ;median data
   EMEPfile = DataDir + 'emep_ann_hg_dep_2013-15.dat' ;ha 5/15/18

   ; Range of wet deposition to plot, ug/m2/y
   Range = [0, 20]

   ; Convert kg to ug
   kg2ug = 1d9

   ;=======================================
   ; Read EMEP data
   ;=======================================

   EMEPsite = ''
   EMEPlat = 0.
   EMEPlon = 0.
   EMEPelev = 0.
   d2015 = 0.

   transread_delim, lun, filename=EMEPfile, skiplines=3, delim=',', /debug, $
              EMEPsite, EMEPlat, EMEPlon, EMEPelev, $
                    d2015, $
              format='(A,7F)'

   EMEP = d2015
   EMEPyear = 2015

   ; Convert ng/m2/y -> ug/m2/y
   EMEP = EMEP / 1e3


   ;=======================================
   ; Extract GEOS-Chem Data
   ;=======================================

   WetDep = get_total_wetdep( FileName=FileName, species=['Hg2', 'HgP'], $
                              GridInfo=GridInfo, Year=Year )

   ;=======================================
   ; Get EMEP data for the same year as model
   ;=======================================

   ; Default- Don't plot unless we have data
   PlotEMEP = 0L

; Year can be a multi-element array (H Amos, 29 May 2011)
;   if (Keyword_set(nearest_year) and (year lt 2013)) then $
;      EMEPyear_in = 2013 else $
;   if (Keyword_set(nearest_year) and (year gt 2014)) then $
;      EMEPyear_in = 2014
   if (Keyword_set(nearest_year) and (max(year) lt 2015)) then $
      EMEPyear_in = 2015 else $
   if (Keyword_set(nearest_year) and (max(year) gt 2015)) then $
      EMEPyear_in = 2015


   if Keyword_set( EMEPyear_in ) then $
      thisYear = EMEPyear_in $
   else $
      thisYear = year

   FIRST = 1L

   nYears=n_elements( thisYear )

   for i=0L, nYears-1L do begin

      iYr = where( EMEPyear eq thisYear[i], ctYr)
      iYr = iYr[0]
      if (ctYr ge 1) then begin

         ; Locate sites with data
         iSite = where( reform(EMEP[*, iYr]) gt 0, ctSite )
         if (ctSite ge 1) then begin

            ; We have data, so turn on plot switch
            PlotEMEP = 1L

            if (FIRST) then begin
               pEMEP = EMEP[iSite, iYr]
               pEMEPlon = EMEPlon[iSite]
               pEMEPlat = EMEPlat[iSite]
               pEMEPid = EMEPsite[iSite]
               pYear = thisYear[i]
               FIRST = 0L
            endif else begin
               pEMEP = [pEMEP, EMEP[iSite, iYr]]
               pEMEPlon = [pEMEPlon, EMEPlon[iSite]]
               pEMEPlat = [pEMEPlat, EMEPlat[iSite]]
               pEMEPid = [pEMEPid, EMEPsite[iSite]]
               pYear = [pYear, thisYear[i]]
            endelse
         endif

      endif

   endfor

   PlotEMEP = PlotEMEP * keyword_set( pEMEP )

   if (PlotEMEP) then begin

      ; temporary variables
      pEMEP1    = pEMEP
      pEMEPlon1 = pEMEPlon
      pEMEPlat1 = pEMEPlat

      ; Sort data and average over years
       pEMEPall    = tapply( pEMEP, pEMEPid, 'mean_nan' )
       pEMEPlonall = tapply( pEMEPlon, pEMEPid, 'mean_nan')
       pEMEPlatall = tapply( pEMEPlat, pEMEPid, 'mean_nan')

       ; number of years of EMEP data for each site
       EMEPnYears = tapply( finite( pEMEP ), pEMEPid, 'total' )


       ; Stes with data for as many years as model
       iFinite = where( EMEPnYears eq nYears, ct)
       if ( ct ge 1 ) then begin
          pEMEP = pEMEPall[ iFinite ]
          pEMEPlon = pEMEPlonall[ iFinite ]
          pEMEPlat = pEMEPlatall[ iFinite ]
       endif else begin
          pEMEP = [!values.f_nan]
          pEMEPlon = [0]
          pEMEPlat = [0]
       endelse

       ; Sites with data fewer years than model
       iLess = where( (EMEPnYears lt nYears) and (EMEPnYears ge 1), ct )
       PlotEMEPless = 0L
       if (ct ge 1 ) then begin
          pEMEPless = pEMEPall[ iLess ]
          pEMEPlon_less = pEMEPlonall[ iLess ]
          pEMEPlat_less = pEMEPlatall[ iLess ]
          PlotEMEPless = 1L
       endif



   endif

   ;=======================================
   ; Plotting
   ;=======================================

   If Keyword_Set( PS ) then $
      ps_setup, /open, file=psFileName, xsize=6, ysize=4, /landscape

   multipanel, col=ncols, row=nrows, omargin=[0.05, 0.05, 0.1, 0.1]

   xmid = GridInfo.xmid
   ymid = GridInfo.ymid


   if Keyword_set( PlotEMEP ) then begin

       myct, 33, ncolors=17 ;eds 5/11/11

      ; Make title
      title = 'Hg Wet Deposition, GEOS-Chem ' + $
              strjoin( strtrim( string(year), 2), ', ')+$
              '!C EMEP '+strjoin(strtrim(string(pYear), 2), ', ')

       tvmap_region, region='europe', $
                     wetdep, xmid, ymid, $
                     pEMEP, pEMEPlon, pEMEPlat, t_symbol=4, $
                     ;/sample, title=title, /ystyle, $
                     title=title, /ystyle, $
                     mindata=range[0], maxdata=range[1], $
                     margin=[0.015,  0.01, 0.015, 0.02],  $
                     xtitle=xtitle, ytitle=ytitle, botoutofrange=!myct.bottom, $
                     csfac=1.15, /usa, /continents, /hires, $
                     t_color=1, /noadvance, symsize=2


       if (PlotEMEPless) then begin
          scatterplot_datacolor, /overplot, /nocb, $
                       pEMEPlon_less, pEMEPlat_less, pEMEPless, $
                       zmin=range[0], zmax=range[1], symNum=1, $
                       /Outline, symsize=2

          ; Replot data with all years to make sure it is on top
          scatterplot_datacolor, /overplot, /nocb, $
                       pEMEPlon, pEMEPlat, pEMEP, $
                       zmin=range[0], zmax=range[1], $
                       /Outline, symsize=2, sym=4

       endif


   endif else begin

      ; Make title
      title = 'Hg Wet Deposition, GEOS-Chem ' + $
              strjoin( strtrim(string(year), 2), ', ')

      tvmap_region, region='europe', $
                    wetdep, xmid, ymid, $
                    title=title, /ystyle, $
                    mindata=range[0], maxdata=range[1], $
                    margin=[0.015,  0.01, 0.015, 0.02],  $
                    xtitle=xtitle, ytitle=ytitle, botoutofrange=!myct.bottom, $
                    csfac=1.15, /usa, /continents, /hires

   endelse

   colorbar, min=range[0], max=range[1], div=Divisions, $
             unit='ug m!U-2!N y!U-1', $
             position=[0.86, 0.2, 0.88, 0.8], /vertical, /triangle

   multipanel, /off

   If Keyword_Set( PS ) then $
      ps_setup, /close



end
