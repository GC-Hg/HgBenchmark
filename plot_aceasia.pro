pro plot_aceasia, FileName=FileName, $
                  Reference=Reference, $                      
                  psFileName=psFileName, $
                  PS=PS, $
                  DataRange=Range, $
                  PageTitle=PageTitle, $
                  PPQ=PPQ, $
                  _Extra=_Extra

   ;------------------------------------------------;
   ; eds 5/11/11 modified to include reference file ;
   ; for use in mercury model benchmarking          ;
   ;------------------------------------------------;

   ;=======================================
   ; Setup
   ;=======================================

   DataDir = !BENCHMARK+'/data/'
   
   Species = 'Hg0'
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
         psFileName = 'vertical.ps' 
   endif
 
   if ( not Keyword_set( Range ) ) then begin
      if (keyword_set(ppq)) then begin
         xrange = [100, 250]
         xtitle = 'Hg [ppqv]'
      endif else begin
         xrange = [1.0, 2.5]
         xtitle = 'Hg [ng/m!u3!n]'
      endelse
   endif else begin
      xrange = range
      xtitle = 'Hg'
   endelse

   ngm3_ppqv = 112d0 ;eds

   ;=======================================
   ; Read Observations
   ;=======================================

   ; Concentrations and StdDev during ACE-Asia, ng/m3
   aceHg0 = [2.1, 1.87,  1.72, 1.8, 1.75, 1.95, 2.18, 1.35]
   aceStd = [0.3, 0.25, 0.3, 0.27, 0.3, 0.27, 0.42, 0.17]

   if keyword_set(ppq) then begin ;eds
      ; Convert concentration ng/m3 -> ppqv
      aceHg0 = aceHg0 * ngm3_ppqv
      aceStd = aceStd * ngm3_ppqv
   endif

   ;Convert pptv -> ng/m3
   pptv_ngm3 = 8.93D0

   aceAlt = [0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5]

   ;=======================================
   ; Read BPCH data
   ;=======================================

   ;eds 5/11/11 modified for multiple models
   Hg0_mod = 0.
   TGM_mod = 0.
   Hg0_ref = 0.
   TGM_ref = 0.

   nFiles=n_elements( FileName )
   for F=0L, nFiles-1L do begin

   ; Tracer
   ctm_get_data, Hg0DataInfo, DiagN, FileName=FileName[F], Tracer=1L

   ; Number of time steps in the DataInfo structures (should all be same)
   n_times = n_elements( Hg0DataInfo )

   ; check whether there are 12 times in the file (assume they are months)
   if ( n_times NE 12 ) then message, 'File does not contain 12 monthly means'

   ; Get data for April
   s = ctm_get_datablock( Hg0apr, DiagN, FileName=FileName[F], Tracer=1L, $
                          lat=[23, 42], lon=[124, 144], average=3, $
                          xmid=alt, tau0=Hg0DataInfo[3].tau0 )

   ; Get data for April
   s = ctm_get_datablock( Hg2apr, DiagN, FileName=FileName[F], Tracer=2L, $
                          lat=[23, 42], lon=[124, 144], average=3, $
                          xmid=alt, tau0=Hg0DataInfo[3].tau0 )

   ; Average Hg0 for April
   Hg0_mod = Hg0_mod + Hg0apr

   ; Average Hg0 for April
   ; NOTE: eventually need to update this to account for variable Fg
   TGM_mod = TGM_mod + Hg0apr + 0.5*Hg2apr

   endfor
   
   Hg0_mod = Hg0_mod / nFiles
   TGM_mod = TGM_mod / nFiles

   if keyword_set(ppq) then begin
      ;convert pptv -> ppqv
      Hg0_mod = Hg0_mod * 1d3 
      TGM_mod = TGM_Mod * 1d3
   endif else begin
      ;convert pptv -> ng/m3 eds 5/11/11
      Hg0_mod = Hg0_mod * pptv_ngm3
     TGM_mod = TGM_mod * pptv_ngm3
   endelse


   ;REPEAT FOR REFERENCE FILE
   ;eds 5/11/11

   if keyword_set(Reference) then begin

   nFiles=n_elements( Reference )
   for F=0L, nFiles-1L do begin

   ; Tracer
   ctm_get_data, Hg0DataInfo, DiagN, FileName=Reference[F], Tracer=1L

   ; Number of time steps in the DataInfo structures (should all be same)
   n_times = n_elements( Hg0DataInfo )

   ; check whether there are 12 times in the file (assume they are months)
   if ( n_times NE 12 ) then message, 'File does not contain 12 monthly means'

   ; Get data for April
   s = ctm_get_datablock( Hg0apr, DiagN, FileName=Reference[F], Tracer=1L, $
                          lat=[23, 42], lon=[124, 144], average=3, $
                          xmid=alt, tau0=Hg0DataInfo[3].tau0 )

   ; Get data for April
   s = ctm_get_datablock( Hg2apr, DiagN, FileName=Reference[F], Tracer=2L, $
                          lat=[23, 42], lon=[124, 144], average=3, $
                          xmid=alt, tau0=Hg0DataInfo[3].tau0 )

   ; Average Hg0 for April
   Hg0_ref = Hg0_ref + Hg0apr

   ; Average Hg0 for April
   TGM_ref = TGM_ref + Hg0apr + 0.5*Hg2apr

   endfor

   Hg0_ref = Hg0_ref / nFiles
   TGM_ref = TGM_ref / nFiles


   if (keyword_set(ppq)) then begin
       ;convert pptv -> ppqv
      Hg0_ref = Hg0_ref * 1d3
      TGM_ref = TGM_ref * 1d3
   endif else begin
       ;convert pptv -> ng/m3 eds 5/11/11
      Hg0_ref = Hg0_ref * pptv_ngm3
      TGM_ref = TGM_ref * pptv_ngm3
   endelse
   

   endif

   ; clear memory
   ctm_cleanup


   ;=======================================
   ; Plotting
   ;=======================================
   
   If Keyword_Set( PS ) then $
      ps_setup, /open, file=psFileName, xsize=6, ysize=4, /landscape

   multipanel, col=1, row=1, omargin=[0.05, 0.05, 0.1, 0.1], pos=p1

   p = getpos( 2, pos=p1, margin=0 )

   ; ACE-Asia data
   plot, aceHg0, aceAlt, /color, $
;         yrange=[0, 8], xrange=[100, 250], $
         yrange=[0, 8], xrange=xrange, $ ;eds 5/11/11
;         xtitle='Hg!U0!N, ppq', $
         xtitle=xtitle, $
         ytitle='Altitude, km', $
         title='ACE-Asia', pos=p, $
         thick=3, _Extra=_Extra

   ;ACE-Asia error bars
   errorbar, aceHg0, aceAlt, aceStd, /x, /color, $
         _Extra=_Extra

   ;following now all in ng/m3 eds 5/11/11

   ;New Model Hg(0)
   oplot, Hg0_mod, Alt, color=3, $
         thick=3, _Extra=_Extra
   ;New Model Hg(0)+Hg(II)
   oplot, TGM_mod, Alt, color=2, $
         thick=3, _Extra=_Extra

   if keyword_set(reference) then begin

   ;Old Model Hg(0)
   oplot, Hg0_ref, Alt, color=6, $
         thick=3, _Extra=_Extra
   ;Old Model Hg(0)+Hg(II)
   oplot, TGM_ref, Alt, color=4, $
         thick=3, _Extra=_Extra

   endif

   ; Make legend ;modified eds 5/11/11
   if keyword_set(reference) then begin

   legend, label=['Observations', 'New Model Hg(0)', 'New Model TGM', $
           'Old Model Hg(0)', 'Old Model TGM'], $
           line=[0, 0, 0, 0, 0], lcolor=[1, 3, 2, 6, 4], $
           halign=1., valign=.05, charsize=1., /color, /frame, $
           plotposition=p1

   endif else begin

   legend, label=['Observations', 'New Model Hg(0)', 'New Model TGM'], $
           line=[0, 0, 0], lcolor=[1, 2, 3], $
           halign=1., valign=.05, charsize=1., /color, /frame, $
           plotposition=p1

   endelse

   multipanel, /off

   If Keyword_Set( PS ) then $
      ps_setup, /close

end
