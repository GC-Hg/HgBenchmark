pro plot_usa_wetdep, FileName=FileName, $
                     psFileName=psFileName, $
                     PS=PS, $
                     MDNyear=MDNyear_in, $
                     nearest_year=nearest_year,$
                     Divisions=Divisions, wetdep ;eds 11/16/10 return wetdep
   
; Plot annual total wet deposition over the United States
; Compare with MDN data

   ;------------------------------------------------;
   ; eds 5/11/11 modified to include reference file ;
   ; for use in mercury model benchmarking          ;
   ; jaf 5/17/11 added nearest_year keyword to plot ;
   ; nearest year of data if no data available      ;
   ;------------------------------------------------;

   ;=======================================
   ; Setup
   ;=======================================

   DataDir = !BENCHMARK+'/data/' ;eds 5/11/11

;   if ( not Keyword_set( FileName ) ) then $
;      FileName = '~/runs/HgBr.v8-01-01/ctm.bpch.stdOx'

   if Keyword_set( psFileName ) then $
      PS = 1L $
   else $
      PS = 0L

   if ( not Keyword_set( psFileName ) ) then $
      psFileName = 'wetdep_USA.ps' 
   
   if ( not Keyword_set( Divisions ) ) then $
      Divisions = 5 

   ; File with Annual mean MDN dat
   MDNfile = DataDir + 'MDNannual.sav' ;eds 5/11/11

   ; Range of wet deposition to plot, ug/m2/y
   Range = [0, 20]

   ; Convert kg to ug
   kg2ug = 1d9

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
   
   ; Convert ng/m2/y -> ug/m2/y
   MDNyear = MDN.year
   MDNlat = MDN.lat
   MDNlon = MDN.lon
   MDNsite = MDN.siteID
   MDN = MDN.HgDep/1E3

   ;=======================================
   ; Extract GEOS-Chem Data
   ;=======================================

   wetdep = get_total_wetdep( FileName = FileName, species=['Hg2', 'HgP'], $
                              GridInfo=GridInfo, Year=Year )


   ;=======================================
   ; Get MDN data for the same year as model
   ;=======================================

   ; Default- Don't plot unless we have data
   PlotMDN = 0L

   if (Keyword_set(nearest_year) and (year lt min(MDNyear))) then $
      MDNyear_in = min(MDNyear) else $
   if (Keyword_set(nearest_year) and (year gt max(MDNyear))) then $
      MDNyear_in = max(MDNyear)

   if Keyword_set( MDNyear_in ) then $
      thisYear = MDNyear_in $
   else $
      thisYear = year

   FIRST = 1L

   for i=0L, n_elements(thisYear)-1L do begin

      iYr = where( MDNyear eq thisYear[i], ctYr)
      iYr = iYr[0]
      if (ctYr ge 1) then begin
      
         ; Locate sites with data
         iSite = where( reform(MDN[*, iYr]) gt 0, ctSite )
         if (ctSite ge 1) then begin

            ; We have data, so turn on plot switch
            PlotMDN = 1L

            if (FIRST) then begin
               pMDN = MDN[iSite, iYr]
               pMDNlon = MDNlon[iSite]
               pMDNlat = MDNlat[iSite]
               pMDNid = MDNsite[iSite]
               pYear = thisYear[i]
               FIRST = 0L
            endif else begin
               pMDN = [pMDN, MDN[iSite, iYr]]
               pMDNlon = [pMDNlon, MDNlon[iSite]]
               pMDNlat = [pMDNlat, MDNlat[iSite]]
               pMDNid = [pMDNid, MDNsite[iSite]]
               pYear = [pYear, thisYear[i]]
            endelse
         endif

      endif

   endfor

   if (PlotMDN) then begin
      ; Sort data and average over years
      pMDN    = tapply( pMDN, pMDNid, 'mean' )
      pMDNlon = tapply( pMDNlon, pMDNid, 'mean')
      pMDNlat = tapply( pMDNlat, pMDNid, 'mean')

      ; Don't plot sites in Canada
      pMDN = pMDN[ where( pMDNlat le 48) ]   
      pMDNlon = pMDNlon[ where( pMDNlat le 48) ]   
      pMDNlat = pMDNlat[ where( pMDNlat le 48) ]   

   endif

   ;=======================================
   ; Plotting
   ;=======================================

   If Keyword_Set( PS ) then $
      ps_setup, /open, file=psFileName, xsize=10, ysize=7, /landscape
   
   multipanel, col=ncols, row=nrows, omargin=[0.05, 0.05, 0.1, 0.1]

   myct, 33, ncolors=17 ;eds 5/11/11

   xmid = GridInfo.xmid
   ymid = GridInfo.ymid

   if Keyword_set( PlotMDN ) then begin

      ; Make title
      title = 'Hg Wet Deposition, GEOS-Chem ' + $
              strjoin( strtrim( string(year), 2), ', ')+$
              '!C MDN '+strjoin(strtrim(string(pYear,'(i)'), 2), ', ') 

       tvmap_region, region='conus', $
                     wetdep, xmid, ymid, $
                     pMDN, pMDNlon, pMDNlat, t_symbol=1, $
                     ;/sample, $ ;eds 11/16/10
                     title=title, /ystyle, $
                     mindata=range[0], maxdata=range[1], $
                     margin=[0.015,  0.01, 0.015, 0.02],  $
                     xtitle=xtitle, ytitle=ytitle, botoutofrange=!myct.bottom, $
                     csfac=1.15, /usa, /continents, /hires

   endif else begin
      
      ; Make title
      title = 'Hg Wet Deposition, GEOS-Chem ' + strtrim(string(year), 2)

      tvmap_region, region='conus', $
                    wetdep, xmid, ymid, $
                    ;/sample, $ ;eds 5/5/11
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
