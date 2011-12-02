function get_total_seasalthg, FileName=FileName, $
                           Months=Months, $
                           GridInfoOut=GridInfoOut, $
                              Total=Total, $
                              PerDay=PerDay, $
                           noCheck=noCheck, $
                           _Extra=_Extra
   
; Get total Hg2 uptake into seasalt aerosols
; Return the result as ug/m2

   ;=======================================
   ; Setup
   ;=======================================

   if ( not Keyword_set( FileName ) ) then $
      FileName = '~/runs/HgBr.v8-01-01/ctm.bpch.stdOx'

   if ( not Keyword_set( Months ) ) then $
     Months = indgen( 12 )+1 

   ; Convert kg to ug
   kg2ug = 1d9

   ; Convert kg to Mg
   kg2Mg = 1d-3

   ; Convert molec to kg
   molec2kg = 0.201 / 6.02d23

   ; keyword to return result at ug/m2/d
   PerDay= keyword_set( PerDay )

   ; We require 12 monthly entries, unless noCheck is True
   doCheck = ~keyword_set( noCheck )

   ;=======================================
   ; Extract GEOS-Chem Data
   ;=======================================

   SeasaltHg = 0d0

   SumDeltaTd = 0d0

   nFiles=n_elements( FileName )

   for F=0L, nFiles-1L do begin

      ; Sea-salt Hg2 uptake, kg
      ctm_get_data, DataInfo, 'PL-HG2-$', FileName=FileName[F], Tracer=4

      getmodelandgridinfo, DataInfo[0], ModelInfo, GridInfo

      ; Get grid area [m2]
      area_m2  = ctm_boxsize( GridInfo, /m2 )   
   
      ; Number of time steps in the DataInfo structures (should all be same)
      n_times = n_elements( DataInfo )
   
      ; check whether there are 12 times in the file (assume they are months)
      if ( n_times ne 12 and (doCheck) ) then $
        message, 'File does not contain 12 monthly means'

      for iMonth=0L, n_times-1L do begin
         
         ; Total only requested months
         tmp = where( Months eq iMonth+1, ct )
         if (ct ge 1) then begin

            ; Start and end times of data entry [hr]
            tau0 = DataInfo[iMonth].Tau0
            tau1 = DataInfo[iMonth].Tau1
            
            ; Elapsed time during data collection [d]
            deltaTd  = (Tau1-Tau0) / 24.            

            ; Total time elapsed during all [d]
            SumDeltaTd = SumDeltaTd + deltaTd

            ; Calculate Dry Dep [ug/m2/yr]
            SeasaltHg = SeasaltHg + $
                        *(DataInfo[iMonth].Data) * kg2ug / area_m2
            

         endif
      endfor
   endfor

   ; Total Wet dep, kg
   Total = total( SeasaltHg * area_m2 / kg2ug )

   ; Average over files
   SeasaltHg = SeasaltHg / nFiles

   ; Return dep per day if requested
   if ( PerDay ) then begin
      SeasaltHg = SeasaltHg / ( float(SumDeltaTd) / float(nFiles) )
   endif

return, SeasaltHg

end
