; $Id$
;-----------------------------------------------------------------------
;+
; NAME:
;        PLOT_GRADIENT_TGM
;
; PURPOSE:
;        Plot meridional gradient of TGM at surface from BPCH file
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        PLOT_GRADIENT_TGM
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
;        PAGETITLE : string label for top of page
;        _EXTRA    : passed to CTM_GET_DATABLOCK
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
;        eds, 11 May 2011: modified for mercury benchmarking
;
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


 pro plot_gradient_tgm, FileName=FileName, $
                       Reference=Reference, $
                       psFileName=psFileName, $
                       PS=PS, $
                       DataRange=Range, $
                       PageTitle=PageTitle, $
                       PPQ=PPQ, $
                       _Extra=_Extra

   ;=======================================
   ; Setup
   ;=======================================
   
   DataDir = !BENCHMARK+'/data/' ;eds 5/11/11

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

   multipanel, omargin=[0.05, 0.05, 0.1, 0.1]
 
   if ( not Keyword_set( psFileName ) ) then begin
      if ( Global ) then begin
         psFileName = 'surface.'+species+'.ps' 
      endif else begin
         sLonCenter = strtrim( string( fix( mean( lonRange ) ) ), 2)
         psFileName = 'surface.' + sLonCenter + $
                      '.'+species+'.ps'
      endelse
   endif
 
   if ( not Keyword_set( Range ) ) then begin
      if (keyword_set(ppq)) then begin
         Range = [50, 300]
         unit = 'ppqv'
         ytitle = 'TGM [ppqv]'
      endif else begin
         Range = [0.5, 3.0]
         unit = 'ng/m!u3!n'  ;eds 5/11/11
         ytitle = 'TGM [ng/m!u3!n]'
      endelse
   endif

   ; Unit conversion: ng/m3 -> ppqv
   ngm3_ppqv = 112.
   pptv_ngm3 = 8.93d0 ;eds 5/10/11

   ;=======================================
   ; Read Observations
   ;=======================================

   GradFile = DataDir + 'tgm_gradient.sav' ;eds 5/11/11
   restore, GradFile, /verbose

   ;---------------
   ; Land stations

   TGMFile = DataDir + 'TGMSiteAnnual.csv' ;eds 5/11/11

   ; eds 5/11/11
   if (keyword_set(ppq)) then begin
      ;convert pptv -> ppqv
      gradient.Temme.tgm = gradient.Temme.tgm * 1d3
      gradient.Lamborg.tgm = gradient.Lamborg.tgm * 1d3
      gradient.Laurier2007.tgm = gradient.Laurier2007.tgm * 1d3
      gradient.Galathea.Hg0 = gradient.Galathea.Hg0 * 1d3
   endif else begin
      ;convert pptv -> ng/m3
      gradient.Temme.tgm = gradient.Temme.tgm * pptv_ngm3
      gradient.Lamborg.tgm = gradient.Lamborg.tgm * pptv_ngm3
      gradient.Laurier2007.tgm = gradient.Laurier2007.tgm * pptv_ngm3
      gradient.Galathea.Hg0 = gradient.Galathea.Hg0 * pptv_ngm3
   endelse

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

   if (keyword_set(ppq)) then begin
      ; Convert ng/m3 -> ppq
      LANDtgm = LANDtgm * ngm3_ppqv ;eds 5/11/11
   endif

   ii = where( LANDlon ge -130 and LANDlon le 45)
   LANDtgm = LANDtgm[ii]
   LANDlat = LANDlat[ii]
   LANDlon = LANDlon[ii]
   LANDalt = LANDalt[ii]


   ;=======================================
   ; Read BPCH data
   ;=======================================

   nFiles = n_elements( FileName )

   ; Tracer
   ctm_get_data, Hg0DataInfo, DiagN, FileName=FileName[0], Tracer=1L

   ; Number of time steps in the DataInfo structures (should all be same)
   n_times = n_elements( Hg0DataInfo )

   ; check whether there are 12 times in the file (assume they are months)
   if ( n_times NE 12 ) then message, 'File does not contain 12 monthly means'

   ; Get grid info
   getmodelandgridinfo, Hg0DataInfo[0], ModelInfo, GridInfo

   ; Initialize array for mean annual concentration
   ; renamed data_mean -> model_mean for clarity eds 5/11/11
   Model_mean = fltarr( GridInfo.imx, GridInfo.jmx ) 
   sum_deltaTs = 0.

   for F=0L, nFiles-1L do begin

   ; Tracer
   ctm_get_data, Hg0DataInfo, DiagN, FileName=FileName[F], Tracer=1L
   ctm_get_data, Hg2DataInfo, DiagN, FileName=FileName[F], Tracer=2L

   ; Gas fraction, if it exists
   ctm_get_data, FgDataInfo, 'PL-HG2-$', FileName=FileName[F], Tracer=9L
   
      for iMonth=0L, n_times-1L do begin

         ; Read or set HgII gas fraction
         if ( n_elements(FgDataInfo) gt 0 ) then $
            Fg=(*( FgDataInfo[iMonth].Data))[*,*,0] else $
            Fg=0.5

         ; Start and end times of data entry [hr]
         tau0 = Hg0DataInfo[iMonth].Tau0
         tau1 = Hg0DataInfo[iMonth].Tau1

         ; Elapsed time during data collection [s]
         deltaTs  = (Tau1-Tau0) * 3600.

         ; Calculate mean concentration
         Model_mean = Model_mean + deltaTs * $
                     ( (*( Hg0Datainfo[iMonth].Data))[*, *, 0] + $
                       Fg * (*( Hg2Datainfo[iMonth].Data))[*, *, 0] )
      
         ; sum of weights
         sum_deltaTs = sum_deltaTs + deltaTs

      endfor
   endfor

   Model_mean = Model_mean / sum_deltaTs

   if (keyword_set(ppq)) then begin
      ;convert pptv -> ppqv
      Model_mean = Model_mean * 1d3
   endif else begin
      ;convert pptv -> ng/m3
      Model_mean = Model_mean * pptv_ngm3 ;eds 5/11/11
   endelse

   ;REPEAT FOR REFERENCE FILE
   ;eds 5/11/11
   
   if keyword_set(reference) then begin

   nFiles = n_elements( Reference )

   ; Tracer
   ctm_get_data, Hg0DataInfo, DiagN, FileName=Reference[0], Tracer=1L

   ; Number of time steps in the DataInfo structures (should all be same)
   n_times = n_elements( Hg0DataInfo )

   ; check whether there are 12 times in the file (assume they are months)
   if ( n_times NE 12 ) then message, 'File does not contain 12 monthly means'

   ; Get grid info
   getmodelandgridinfo, Hg0DataInfo[0], ModelInfo, GridInfo

   ; Initialize array for mean annual concentration
   Ref_mean = fltarr( GridInfo.imx, GridInfo.jmx )
   sum_deltaTs = 0.

   for F=0L, nFiles-1L do begin

   ; Tracer
   ctm_get_data, Hg0DataInfo, DiagN, FileName=Reference[F], Tracer=1L
   ctm_get_data, Hg2DataInfo, DiagN, FileName=Reference[F], Tracer=2L

   ; Gas fraction, if it exists
   ctm_get_data, FgDataInfo, 'PL-HG2-$', FileName=FileName[F], Tracer=9L
   
      for iMonth=0L, n_times-1L do begin

         ; Read or set HgII gas fraction
         if ( n_elements(FgDataInfo) gt 0 ) then $
            Fg=(*( FgDataInfo[iMonth].Data))[*,*,0] else $
            Fg=0.5

         ; Start and end times of data entry [hr]
         tau0 = Hg0DataInfo[iMonth].Tau0
         tau1 = Hg0DataInfo[iMonth].Tau1

         ; Elapsed time during data collection [s]
         deltaTs  = (Tau1-Tau0) * 3600.

         ; Calculate mean concentration
         Ref_mean = Ref_mean + deltaTs * $
                     ( (*( Hg0Datainfo[iMonth].Data))[*, *, 0] + $
                       Fg * (*( Hg2Datainfo[iMonth].Data))[*, *, 0] )

         ; sum of weights
         sum_deltaTs = sum_deltaTs + deltaTs

      endfor
   endfor

   Ref_mean = Ref_mean / sum_deltaTs

   if keyword_set(ppq) then begin
      Ref_mean = Ref_mean * 1d3
   endif else begin
      Ref_mean = Ref_mean * pptv_ngm3 ;eds 5/11/11
   endelse

   endif

   ; clear memory
   ctm_cleanup

   ;=======================================
   ; Plotting
   ;=======================================
 
   If Keyword_Set( PS ) then $
      ps_setup, /open, file=psFileName, xsize=10, ysize=7, /landscape
   
   xmid = GridInfo.xmid
   ymid = GridInfo.ymid

   if (not Keyword_set(PageTitle) ) then $
      PageTitle = 'Surface TGM' 

   plot, ymid, mean2( Model_mean, 1), /color, $ ;took out *1e3 for ppqv
      xrange=[-90, 90], yrange=range, /xstyle, /ystyle, $
;      xtitle='Latitude', ytitle='TGM, ppq',    $
      xtitle='Latitude', ytitle=ytitle, $  ;eds 5/11/11
      xticks=6, $;, ticklen=0.03, yminor=2,      $
      title=PageTitle
   oplot, ymid, mean2(Model_mean,1), color=2, thick=3 ;new model version
   if keyword_set(reference) then begin
      oplot, ymid, mean2(Ref_mean,1), color=4, thick=3 ;old model version
   endif

   ; Add observations
   oplot, gradient.Temme.lat, gradient.Temme.tgm, /color, psym=1 ;took out *1e3 for ppqv
   oplot, gradient.Lamborg.lat, gradient.Lamborg.tgm, /color, psym=6
   oplot, gradient.Laurier2007.lat, gradient.Laurier2007.tgm, /color, psym=4
   oplot, gradient.Galathea.lat, gradient.Galathea.Hg0, /color, psym=sym(6)
   oplot, LANDlat, LANDtgm, color=!myct.green, psym=sym(5)

   ; land stations as line
;   t=tapply(LANDtgm, round(LANDlat/10)*10, 'mean', group=l)
;   oplot, l, t, color=!myct.green 

   ; Make legend
   if keyword_set(reference) then begin

   ;modified legends eds 5/11/11
   legend, label=['Land-based stations', 'Temme et al. 2003', $
                  'Laurier et al. 2007', 'Lamborg et al. 1999', $
                  'Sorenson et al. 2009', 'New Model Version Zonal Mean', $
                  'Old Model Version Zonal Mean'], $
           symbol=[5, 11, 9, 10, 6, 0, 0], $
           line=[-1,-1,-1,-1,-1, 0, 0], $
           color=[!myct.green, 1, 1, 1, 1, 0, 0], $
           lcolor=[0, 0, 0, 0, 0, 2, 4], $
           halign=.05, valign=.95, charsize=1, /frame

   endif else begin

   legend, label=['Land-based stations', 'Temme et al. 2003', $
                  'Laurier et al. 2007', 'Lamborg et al. 1999', $
                  'Sorenson et al. 2009', 'New Model Version Zonal Mean'], $
           symbol=[5, 11, 9, 10, 6, 0], $
           line=[-1,-1,-1,-1,-1, 0], $
           color=[!myct.green, 1, 1, 1, 1, 0], $
           lcolor=[0, 0, 0, 0, 0, 2], $
           halign=.05, valign=.95, charsize=1, /frame

   endelse

   multipanel, /off
 
   If Keyword_Set( PS ) then $
      ps_setup, /close

end
