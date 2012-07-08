function get_mean_concentration, FileName=FileName, $
                           Species=Species, $
                           Months=Months, $
                           YearOut=YearOut, $
                           GridInfoOut=GridInfoOut, $
                           TGM=TGM, $
                           ngm3=ngm3, $
                           _Extra=_Extra
   
; Get mean concentration, averaged over time
; Return the result as pptv
; TGM is returned as TGM = Hg0 + Fg*HgII
; If Fg is undefined in output, then Fg=0.5

   ;=======================================
   ; Setup
   ;=======================================

   Conc = 0d0 ;eds 5/10/11 clean-up previous output

   if ( not Keyword_set( Species ) ) then $
     Species = ['Hg0', 'Hg2', 'HgP']

   if ( not Keyword_set( Months ) ) then $
     Months = indgen( 12 )+1 

   ; Convert kg to ug
   kg2ug = 1d9

   ; Convert kg to Mg
   kg2Mg = 1d-3

   ; Convert molec to kg
   molec2kg = 0.201 / 6.02d23

   ; Convert ppt to ng/m3
   pptv_ngm3 = 8.93d0

   ;=======================================
   ; Extract GEOS-Chem Data
   ;=======================================

   SumConc = 0d0
   SumTime = 0d0


   for F=0L, n_elements(FileName)-1L do begin
   for i=0L, n_elements(Species)-1L do begin

      case strlowcase(Species[i]) of
         'hg0': tracer = 1
         'hg2': tracer = 2
         'hgp': tracer = 3
      endcase

      ; Concentration [pptv]
      ctm_get_data, DataInfo, 'IJ-AVG-$', Filename=filename[F], $
                    Tracer=Tracer

      ; Gas fraction [unitless] if it exists
      ctm_get_data, DataInfo_Fg, 'PL-HG2-$',Filename=filename[F],$
                    Tracer=9

      ; Number of time steps in the DataInfo structures (should all be same)
      n_times = n_elements( DataInfo )
   
      ; check whether there are 12 times in the file (assume they are months)
      if ( n_times NE 12 ) then $
        message, 'File does not contain 12 monthly means'

      ; Which year is the model?
      YearOut = ( tau2yymmdd(  DataInfo[0].Tau0 ) ).year

      ; Get grid info
      getmodelandgridinfo, DataInfo[0], ModelInfoOut, GridInfoOut

      for iMonth=0L, n_times-1L do begin
         
         ; Total only requested months
         tmp = where( Months eq iMonth+1, ct )
         if (ct ge 1) then begin

            ; Start and end times of data entry [hr]
            tau0 = DataInfo[iMonth].Tau0
            tau1 = DataInfo[iMonth].Tau1
            
            ; Elapsed time during data collection [s]
            deltaTs  = (Tau1-Tau0) * 3600.

            ; Cumulative concentration [pptv*s]
            if (keyword_set(TGM) and tracer eq 2) then begin
               ; Read HgII gas fraction if available
               if (n_elements(DataInfo_Fg) gt 0) then $
                  Fg=*(DataInfo_Fg[iMonth].Data) else $
                  Fg = 0.5 
               SumConc = SumConc +  *(DataInfo[iMonth].Data)* Fg * deltaTs
            endif else SumConc = SumConc +  *(DataInfo[iMonth].Data) * deltaTs

            ; Cumulative seconds
            SumTime = SumTime + deltaTs
         endif

      endfor

   endfor
   endfor

   ; Concentration [pptv]
   Conc = SumConc / (SumTime/n_elements(species))

   if keyword_set(ngm3) then conc=conc*pptv_ngm3

return, Conc

end
