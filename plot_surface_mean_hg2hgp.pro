; $Id$
;-----------------------------------------------------------------------
;+
; NAME:
;        PLOT_SURFACE_MEAN_Hg2HgP
;
; PURPOSE:
;        Plot surface mean concentration of Hg2 + HgP
;        from BPCH file.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        PLOT_SURFACE_MEAN_Hg2HgP
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


pro plot_surface_mean_hg2hgp, FileName=FileName, $
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
   
   Species = 'Hg(II)+Hg(p)'
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

   if ( not Keyword_set(PPQ) ) then $
      ppq = 0L

   if ( not Keyword_set( Range ) ) then begin
      if (keyword_set(ppq)) then begin
         Range = [0, 15]
         unit = 'ppqv'
      endif else begin
         Range = [0, 150]
         unit = 'pg/m!u3!n' ;eds 5/10/11
      endelse
   endif else begin
      clev=[maken(range)]
   endelse

   ; Unit conversion
   ngm3_ppqv = 112D0
   pptv_ngm3 = 8.93D0 ;eds 5/10/11

   DataDir = !BENCHMARK+'/data/' ;eds 5/10/11
   
   ;=======================================
   ; Read observation data
   ;=======================================

   ;---------------
   ; Land stations

;eds 5/10/11 added read in & plot rgm data
   RGMFile = DataDir + 'rgmannualjan06.csv' ;eds 5/10/11

   RGMSITE = ''
   RGMLAT = 0D0
   RGMLON = 0D0
   RGM = 0D0
   TPM = 0D0
   HG2 = 0D0
   CITATION = ''

   transread_delim, lun, filename=RGMFile, /debug, $
     RGMSITE, RGMLAT, RGMLON, RGM, TPM, HG2, CITATION, delim=',', $
     format='(A0,5F0.0,A0)'

   ;data in pg/m3
   if (keyword_set(ppq)) then begin
      ;convert pg/m3 -> ng/m3 -> ppqv
      HG2 = (HG2/1D3)*ngm3_ppqv
   endif

   ;=======================================
   ; Read BPCH data
   ;=======================================

   Data_mean = get_mean_concentration( FileName=FileName, $
                                       Species=['Hg2', 'HgP'], $
                                       GridInfoOut=GridInfo )
   Data_mean = Data_mean[*, *, 0]

   if (keyword_set(ppq)) then begin
      ;convert pptv->pg/m3
      Data_mean = Data_mean * 1d3
   endif else begin
      ;convert pptv->ng/m3->pg/m3
      Data_mean = Data_mean*pptv_ngm3 ;eds 5/10/11
      Data_mean = Data_mean*1d3 ;ng/m3 -> pg/m3
   endelse

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
  
   ;eds 5/10/11 modified keywords
   tvmap_region, Region=Region, $
          data_mean, xmid, ymid, $
          HG2, RGMLON, RGMLAT, t_symbol=1, $
          mindata=range[0], maxdata=range[1], div=5, $
          title=PageTitle, /ystyle, $
          margin=[0.015,  0.01, 0.015, 0.02],  $
          csfac=1.15, /continents, Log=Log, $
          /nogx, /nogy, $
          /noadvance, /robinson, /horizon, $
          /cbar, cbposition=[1.02, 0.2, 1.04, 0.8], /vertical, /triangle, $
          unit=unit, botoutofrange=!myct.bottom, $
          _Extra=_Extra

   multipanel, /off
 
   If Keyword_Set( PS ) then $
      ps_setup, /close
        
end
