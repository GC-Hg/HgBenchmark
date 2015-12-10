; $Id$
;-----------------------------------------------------------------------
;+
; NAME:
;        PLOT_SURFACE_MEAN_TGM
;
; PURPOSE:
;        Plot surface mean concentration of Total Gaseous Mercury
;        (TGM) from BPCH file.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        PLOT_SURFACE_MEAN_TGM
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;        FILENAME : name of BPCH file 
;        PSFILENAME : post-script file name. will plot to screen if
;        not used.
;        PS : set TRUE if you want to use the default post-script
;        filename
;        DATARANGE : 2-element array giving limits of plotting range,
;        in pptv
;        LOG : will plot log concentration if used
;        DIVISIONS : number of divisions in the colorbar 
;        PAGETITLE : string label for top of page
;        REGION    : Plot region, passed to TVMAP_REGION
;        _EXTRA    : passed to CTM_GET_DATABLOCK and TVMAP
;        PPQ : keyword to plot in ppqv instead of ng/m3 ;eds
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
;        cdh, 10 Mar 2009: VERSION 1.00
;        eds, 10 May 2011: Modified for use with mercury
;             model benchmarking. Plots in ng/m3 not ppqv.
;-
; Copyright (C) 2009, Christopher Holmes, Harvard University
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to cdh@io.as.harvard.edu
; with subject "IDL routine plot_zonal_12month"
;-----------------------------------------------------------------------


pro plot_surface_mean_TGM, FileName=FileName, $
                           psFileName=psFileName, $
                           PS=PS, $
                           DataRange=Range, $
                           Log=Log,  $
                           Divisions=Divisions, $
                           PageTitle=PageTitle, $
                           Region=Region, $
                           PPQ=PPQ, $
                          _Extra=_Extra

   ;=======================================
   ; Setup
   ;=======================================

   DataDir = !BENCHMARK+'/data/' ;eds
   
   Species = 'TGM'
   DiagN = 'IJ-AVG-$'

   if ( not Keyword_set( FileName ) ) then $
      FileName = 'ctm.bpch'
 
   ; Plot whole globe by default
   Global = 0L
   if ( not Keyword_set( lonRange ) ) then begin
      lonRange = [-180, 180]
      Global = 1L
   endif
 
   if Keyword_set( psFileName ) then $
      PS = 1L $
   else $
      PS = 0L
 
   if ( not Keyword_set( psFileName ) ) then begin
      if ( Global ) then begin
         psFileName = 'surface.'+species+'.ps' 
      endif else begin
         sLonCenter = strtrim( string( fix( mean( lonRange ) ) ), 2)
         psFileName = 'surface.' + sLonCenter + $
                      '.'+species+'.ps'
      endelse
   endif
 
   LOG = Keyword_set( Log )
 
   if ( not Keyword_set( Divisions ) ) then $
      Divisions = 5 

   if ( not Keyword_set( Region )) then $
      Region = ''

   if ( not Keyword_set( Range ) ) then begin
      if keyword_set(ppq) then begin
         Range = [100, 400]
         unit = 'ppqv'
      endif else begin
         Range = [0.75, 3.50]
         unit = 'ng/m!u3!n !C (not !C linear)' ;eds 5/10/11
      endelse
   endif

   ; Unit conversion: ng/m3 -> ppqv
   ngm3_ppqv = 112.
   pptv_ngm3 = 8.93d0 ;eds 5/10/11

   ;=======================================
   ; Read observation data
   ;=======================================

   ;---------------
   ; Land stations

   TGMFile = DataDir + 'TGMSiteAnnual.csv' ;eds 5/10/11

   LANDID =''
   LANDlat=0.
   LANDlon=0.
   LANDalt=0.
   LANDTGM=0.
   LANDHg0=0.

   transread_delim, lun, filename=TGMFile, skiplines=2, /debug, $
     LANDID, LANDlat, LANDlon, LANDalt, LANDTGM, LANDHg0, delim=',', $
     format='(A0,5F0.0)'
 
   TGM = LANDTGM
   ii = where( LANDTGM eq -9999 )
   LANDtgm[ii] = LANDHg0[ii]
   TGM[ii] = LANDHg0[ii]
   TGMlat = LANDlat
   TGMlon = LANDlon
 
   ;---------------
   ; Cruise Data

   CruiseFile = DataDir + 'TGMCruise.csv' ;eds 5/10/11

   ID =''
   lat=0.
   lon=0.
   d3=0.

   transread_delim, lun, filename=CruiseFile, skiplines=0, /debug, $
     ID, lat, lon, D3, delim=',', $
     format='(A0,3F0.0)'
   
   TGMlat = [TGMlat, lat]
   TGMlon = [TGMlon, lon]
   TGM    = [TGM, d3]

   CRUISEtgm=d3
   CRUISElat=lat
   CRUISElon=lon
   
   ;---------------
   ; Galathea Cruise

   TGMfile = DataDir + 'Galathea3_cruise.csv' ;eds 5/10/11

   lat = 0d0
   lon = 0d0
   D4 = 0d0
   ID = 0L

   transread, lun, filename=TGMfile, skiplines=1, /debug, $
     ID, lat, lon, D4, $
     format='(I0,X,F0.0,X,F0.0,X,F0.0)'

   TGMlat = [TGMlat, lat]
   TGMlon = [TGMlon, lon]
   TGM    = [TGM,    D4]
   
   CRUISEtgm = [CRUISEtgm, D4]
   CRUISElat = [CRUISElat, lat]
   CRUISElon = [CRUISElon, lon]
   
   if keyword_set(ppq) then begin ;eds
      TGM = TGM * ngm3_ppqv
      LANDtgm = LANDtgm * ngm3_ppqv
      CRUISEtgm = CRUISEtgm * ngm3_ppqv
  endif

   ;=======================================
   ; Read BPCH data
   ;=======================================

   Data_mean = get_mean_concentration( FileName=FileName, $
                                       Species=['Hg0', 'Hg2'], $
                                       GridInfoOut=GridInfo, /tgm )
   Data_mean = Data_mean[*, *, 0]

   if keyword_set(ppq) then begin
      ; convert pptv -> ppqv
      Data_mean = Data_mean * 1e3
   endif else begin
      ; convert pptv -> ng/m3
      Data_mean = Data_mean * pptv_ngm3 ;eds 5/10/11
   endelse
   
   ;=======================================
   ; Test Correlation, for terrestrial sites
   ;=======================================

   nsites = n_elements( LANDtgm )

   GRIDindex = lonarr( nSites )

   for s=0L, nsites-1L do begin

       ; Find grid location of site
       ctm_index, GridInfo, I, J, /non_interactive, $
                  center=[LANDlat[S], LANDlon[S] ]

       ; Convert fortran -> IDL index
       I = I - 1L
       J = J - 1L

       GRIDindex[S] = I * 10000L + J       

   endfor

   ;Annual concentration, model and obs
   TGMann_obs = tapply( LANDtgm, GRIDindex, 'mean', group=uniqGRIDindex )

   nGRID = n_elements( uniqGRIDindex )

   TGMann_mod = fltarr( nGRID )

   for S=0L, nGRID-1L do begin

      I = floor( uniqGRIDindex[S] / 10000L )
      J = uniqGRIDindex[S] mod 10000L

      TGMann_mod[S] = Data_mean[I, J]

   endfor

   Rsq = correlate( TGMann_obs,  TGMann_mod ) ^2
   ; Save these for later (jaf, 6/1/11)
   ObsMean = mean(TGMann_obs)
   ObsSD = stddev(TGMann_obs)
   ModMean = mean(TGMann_mod)
   ModSD = stddev(TGMann_mod)

   print, 'The R^2 of annual mean TGM at terrestrial sites is ', $
          Rsq
   print, 'Observed mean +/- s.d.:', $
          ObsMean,  ObsSD, ' ', unit ;eds
   print, 'Modeled mean +/- s.d.:', $
          ModMean,  ModSD, ' ', unit ;eds


   ;=======================================
   ; Test Correlation, for Europe and N. America
   ;=======================================
   
   ii = where( (TGMlat ge 25) and (TGMlat le 70) and $
               (TGMlon ge -130) and (TGMlon le 90) )

   TGMann_obs_limited = tapply( LANDtgm[ii], GRIDindex[ii], 'mean', $
                                group=uniqGRIDindex_limited )

   nGRID = n_elements( uniqGRIDindex_limited )

   TGMann_mod_limited = fltarr( nGRID )

   for S=0L, nGRID-1L do begin

      I = floor( uniqGRIDindex_limited[S] / 10000L )
      J = uniqGRIDindex_limited[S] mod 10000L

      TGMann_mod_limited[S] = Data_mean[I, J]

   endfor

   Rsq_limited = correlate( TGMann_obs_limited,  TGMann_mod_limited ) ^2

   print, 'The R^2 of annual mean TGM in Europe and N. America is ', $
          Rsq_limited
   print, 'Observed mean +/- s.d.:', $
          mean( TGMann_obs_limited ),  $
          stddev( TGMann_obs_limited ), ' ', unit ;eds
   print, 'Modeled mean +/- s.d.:', $
          mean( TGMann_mod_limited ),  $
          stddev( TGMann_mod_limited ), ' ', unit ;eds

   ;=======================================
   ; Plotting
   ;=======================================
 
   If Keyword_Set( PS ) then $
      ps_setup, /open, file=psFileName, xsize=10, ysize=7, /landscape
   
   multipanel, col=ncols, row=nrows, omargin=[0.05, 0.05, 0.1, 0.1]

   xmid = GridInfo.xmid
   ymid = GridInfo.ymid

   if (not Keyword_set(PageTitle) ) then $
      PageTitle = 'Surface '+Species

   myct, 33, ncolors=17 ;eds 5/10/11

   if keyword_set(ppq) then begin
      clev = [maken(100, 200, 11), maken(250, 500, 6)]
   endif else begin
      clev = [maken(0.75, 1.75, 11), maken(2.0, 3.5, 6)] ;eds 5/10/11
   endelse

   tvmap_region, Region=Region, $
          data_mean, xmid, ymid, $
          CRUISETGM, CRUISElon, CRUISElat, t_symbol=1, $
          c_levels=clev, $
          title=PageTitle, /ystyle, $
          margin=[0.015,  0.01, 0.015, 0.02],  $
          csfac=1.15, /continents, Log=Log, $
          /nogx, /nogy, $
          /noadvance, /robinson, /horizon, $
          /cbar, cbposition=[1.02, 0.2, 1.04, 0.8], /vertical, /triangle, $
          botoutofrange=!myct.bottom,unit=unit, $
          _Extra=_Extra


   scatterplot_datacolor, LANDlon, LANDlat, LANDtgm, $
                          sym=4, /outline, /overplot, /nocb, $
                          symsize=1.7, c_levels=clev

   ; Print correlation in lower center
   str = string( Rsq, format='("Terrestrial R!U2!N = ",F4.2)')
   ; Move to lower left (jaf, 6/1/11)
   ; Print obs+mod means, sd (jaf, 6/1/11)
   if keyword_set(ppq) then begin
   strobs = string( ObsMean, format='("Mean Obs. = ",I3)') + $
            string( ObsSD, format='(" +/- ",I3)') + ' ppqv'
   strmod = string( ModMean, format='("Mean Mod. = ",I3)') + $
            string( ModSD, format='(" +/- ",I3)') + ' ppqv'
   endif else begin
   strobs = string( ObsMean, format='("Mean Obs. = ",F4.2)') + $
            string( ObsSD, format='(" +/- ",F4.2)') + ' ng/m!u3'
   strmod = string( ModMean, format='("Mean Mod. = ",F4.2)') + $
            string( ModSD, format='(" +/- ",F4.2)') + ' ng/m!u3'
   endelse
   xyouts, 0.06, 0.15, str, /color, /normal, align=0
   xyouts, 0.55, 0.15, strobs, /color, /normal, align=0
   xyouts, 0.55, 0.10, strmod, /color, /normal, align=0

   multipanel, /off
 
   If Keyword_Set( PS ) then $
      ps_setup, /close

end
