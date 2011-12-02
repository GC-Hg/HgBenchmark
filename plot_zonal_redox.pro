pro plot_zonal_redox, FileName=FileName, $
                        psFileName=psFileName, $
                        PS=PS, $
                        DataRange=Range, $
                        Log=Log,  $
                        Divisions=Divisions, $
                        PageTitle=PageTitle, $
                        Region=Region, $
                        Chemistry=Chemistry, $
                        NoPlot=NoPlot, $
                        _Extra=_Extra, $
                        OxBr, OxOH, OxO3, $
                        Reduction, NetOx, $
                        xmid, ymid
   
   ;---------------------------------------;
   ;                                       ;
   ; eds 5/13/11 modified for hg benchmark ;
   ;                                       ;
   ;---------------------------------------;               
   
   ;=======================================
   ; Setup
   ;=======================================

   species = 'redox'

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
         psFileName = 'zonal.'+species+'.ps' 
      endif else begin
         sLonCenter = strtrim( string( fix( mean( lonRange ) ) ), 2)
         psFileName = 'zonal.' + sLonCenter + $
                      '.'+species+'.ps'
      endelse
   endif
 
   LOG = Keyword_set( Log )
 
   if ( not Keyword_set( Divisions ) ) then $
      Divisions = 5 

   if ( not Keyword_set( Region )) then $
      Region = ''

   if ( not Keyword_set( Range ) ) then $
;      Range = [0, 1] ;[0, 1e-11]
      Range = [0,100]

   if ( not Keyword_set( Chemistry ) ) then $
      Chemistry = 'Br'

;   unit = 'pmol mol!U-1!N y!U-1'
    unit = 'kg/m!u3!n/y' ;eds

   ;=======================================
   ; Read BPCH data
   ;=======================================

   nFiles = n_elements( FileName )

   Reduction = 0.
   OxOH = 0.
   OxBr = 0.
   OxO3 = 0.
   NetOx = 0.

   for F=0L, nFiles-1L do begin

   ; Get Taus from file
   ctm_get_data, DataInfo, 'PL-HG2-$', FileName=FileName[F], Tracer=1L
     
   GetModelAndGridInfo, DataInfo[0], ModelInfo, GridInfo

   ; Number of time steps in the DataInfo structures (should all be same)
   n_times = n_elements( DataInfo )

   for i=0L, n_times-1L do begin

      tau0 = DataInfo[i].tau0

      s = ctm_get_datablock( NetOx1, 'PL-Hg2-$', tracer=1, $
                             FileName=FileName[F], Average=1, $
                             alt=[0, 20], Tau0=Tau0, $
                             xmid=xmid, ymid=ymid, zmid=zmid )
      ;eds assumes Br chemistry default
      if (chemistry eq 'OHO3') then begin
         s = ctm_get_datablock( OxOH1, 'PL-Hg2-$', tracer=2, $
                             FileName=FileName[F], Average=1, $
                             alt=[0, 20], Tau0=Tau0, $
                             xmid=xmid, ymid=ymid, zmid=zmid )
         s = ctm_get_datablock( OxO31, 'PL-Hg2-$', tracer=3, $
                             FileName=FileName[F], Average=1, $
                             alt=[0, 20], Tau0=Tau0 )
         OxBr1 = 0d0
      endif else begin
         s = ctm_get_datablock( OxBr1, 'PL-Hg2-$', tracer=6, $
                             FileName=FileName[F], Average=1, $
                             alt=[0, 20], Tau0=Tau0 )
         OxO31 = 0d0
         OxOH1 = 0d0
      endelse ;eds

;      if (s ne 1) then begin
;         print, 'No Hg(0)+Br found. Assuming zero...'
;         OxBr = 0
;      endif

      Reduction1 = OxO31 + OxOH1 + OxBr1 - NetOx1

;to normalize to pmol/mol - commented out for now eds 5/13/11
      ; Mass of air per grid cell
;      mair = get_geos_state('airmas', ModelInfo, i+1, average=1, $
;                               alt=[0, 20])

      ; Convert kg/box -> kg/kg
;      OxO31 = OxO31 / Mair
;      OxOH1 = OxOH1 / Mair
;      OxBr1 = OxBr1 / Mair
;      Reduction1 = Reduction1 / Mair
 
      ; Convert kg/kg -> pmol/mol
;      OxO31 = OxO31 * 29./201. * 1e12
;      OxOH1 = OxOH1 * 29./201.* 1e12
;      OxBr1 = OxBr1 * 29./201.* 1e12
;      Reduction1 = Reduction1 * 29./201.* 1e12

;     Alternatively, we could normalize by air volume ;eds preferred
     ; Grid Volume, m3
     vol = ctm_boxsize( GridInfo, /volume, /m3 )
     vol = mean2( vol, 1)
      ; Convert kg -> kg/m3
     OxO3 = OxO31/vol
     OxOH = OxOH1/vol
     OxBr = OxBr1/vol
     Reduction = Reduction1/vol
     NetOx = NetOx1/vol ;eds

     OxO3 = OxO3 + OxO31
     OxOH = OxOH + OxOH1
     OxBr = OxBr + OxBr1
     Reduction = Reduction + Reduction1
     NetOx = NetOx + NetOx1


   endfor
   endfor

   OxO3 = OxO3 / nFiles
   OxOH = OxOH / nFiles
   OxBr = OxBr / nFiles
   Reduction = Reduction / nFiles

   ; clear memory
   ctm_cleanup

   ;=======================================
   ; Plotting
   ;=======================================

   If keyword_set( noplot ) then begin  ;eds
      ;just return values
   endif else begin
 
   If Keyword_Set( PS ) then $
      ps_setup, /open, file=psFileName, xsize=10, ysize=7, /landscape
   
   multipanel, cols=2, row=2, omargin=[0.05, 0.05, 0.1, 0.1], $
     margin=0.01

   if (not Keyword_set(PageTitle) ) then $
      PageTitle = 'Zonal '+Species 

   if (chemistry eq 'Br') then begin
   tvplot, OxBr, xmid, ymid, /sample, title='Zonal Hg+Br', /ystyle, $
           mindata=range[0], maxdata=range[1], $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1.15, Log=Log, _Extra=_Extra

   endif else begin

   tvplot, OxOH, xmid, ymid, /sample, title='Zonal Hg+OH', /ystyle, $
           mindata=range[0], maxdata=range[1], $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1.15, Log=Log, _Extra=_Extra   

   tvplot, OxO3, xmid, ymid, /sample, title='Zonal Hg+O3', /ystyle, $
           mindata=range[0], maxdata=range[1], $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1.15, Log=Log, _Extra=_Extra

   endelse

   tvplot, Reduction, xmid, ymid, /sample, title='Zonal Reduction', /ystyle, $
           mindata=range[0], maxdata=range[1], $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1.15, Log=Log, _Extra=_Extra


   colorbar, min=range[0], max=range[1], div=Divisions, unit=unit, $
             position=[0.9, 0.55, 0.91, 0.85], /vertical, /triangle, Log=Log
 
   multipanel, /off
 
   If Keyword_Set( PS ) then $
      ps_setup, /close

   endelse

end
