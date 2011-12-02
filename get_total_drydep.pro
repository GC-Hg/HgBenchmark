function get_total_drydep, FileName=FileName, $
                           Species=Species, $
                           Months=Months, $
                           YearOut=YearOut, $
                           GridInfoOut=GridInfoOut, $
                           Total=Total, $
                           ByMonth=ByMonth, $
                           PerDay=PerDay, $
                           noCheck=noCheck, $
                           noocean=noocean, $
                           _Extra=_Extra
   
; Get total drydep for convective and stratiform precipitation
; Return the result as ug/m2

   ;=======================================
   ; Setup
   ;=======================================

   if ( not Keyword_set( FileName ) ) then $
      FileName = '~/runs/HgBr.v8-01-01/ctm.bpch.stdOx'

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

   ByMonth = Keyword_set( ByMonth )

   ; keyword to return result at ug/m2/d
   PerDay= keyword_set( PerDay )

   ; We require 12 monthly entries, unless noCheck is True
   doCheck = ~keyword_set( noCheck )

   ;=======================================
   ; Extract GEOS-Chem Data
   ;=======================================

   nFiles=n_elements( FileName )

   Drydep = 0d0

   SumDeltaTs = 0d0

   ; Wet deposition flux (due to convection) [kg/s]
   s= ctm_get_datablock( tmp, 'DRYD-FLX', Filename=filename[0], $
                 Tracer=2, lev=1, /first )

   ; Set up size of output variable
   if keyword_set( ByMonth ) then $
      DryDep = fltarr(  [size(tmp, /dim), n_elements( months ) ] ) $
   else $
      DryDep = fltarr(  size(tmp, /dim) )


   for F=0L, nFiles-1L do begin
   for i=0L, n_elements(Species)-1L do begin

      case strlowcase(Species[i]) of
         'hg0': tracer = 1
         'hg2': tracer = 2
         'hgp': tracer = 3
      endcase

      ; Dry deposition flux [molec/cm2/s]
      ctm_get_data, DataInfo, 'DRYD-FLX', Filename=filename[F], $
                    Tracer=Tracer
      
      ; We need to include gas-exchange of Hg(0)
      if ((Tracer eq 1) and ~keyword_set(noocean)) then begin

         ; Deposition of Hg(0) to the ocean [kg]
         ctm_get_data, DataInfo0, 'HG-SRCE', Filename=filename[F], $
           Tracer=17

      endif

      ; Number of time steps in the DataInfo structures (should all be same)
      n_times = n_elements( DataInfo )
   
      ; check whether there are 12 times in the file (assume they are months)
      if ( n_times ne 12 and (doCheck) ) then $
        message, 'File does not contain 12 monthly means'

      ; Which year is the model?
      YearOut = ( tau2yymmdd(  DataInfo[0].Tau0 ) ).year

      ; Get grid info
      getmodelandgridinfo, DataInfo[0], ModelInfoOut, GridInfoOut

      ; Get grid area [m2] and [cm2]
      area_m2  = ctm_boxsize( GridInfoOut, /m2 )
      area_cm2 = area_m2 * 1d4

      for iMonth=0L, n_times-1L do begin
         
         ; Total only requested months
         tmp = where( Months eq iMonth+1, ct )
         if (ct ge 1) then begin

            ; Start and end times of data entry [hr]
            tau0 = DataInfo[iMonth].Tau0
            tau1 = DataInfo[iMonth].Tau1
            
            ; Elapsed time during data collection [s]
            deltaTs  = (Tau1-Tau0) * 3600.

            ; Total time elapsed during all [s]
            SumDeltaTs = SumDeltaTs + deltaTs 

            ; Calculate Dry Dep [ug/m2/yr]
            ThisDryDep = $
                     *(DataInfo[iMonth].Data) * molec2kg * kg2ug * $
                     deltaTs * area_cm2 / area_m2 
            
            ; Add ocean gas exchange of Hg(0 ) 
            if ((Tracer eq 1) and ~keyword_set(noocean)) then begin
               
               ; Deposition of Hg(0) to the ocean [ug/m2/yr]
               ThisDryDep = ThisDryDep + $
                        *(DataInfo0[iMonth].Data) * kg2ug / area_m2
            
            endif

            if ByMonth then begin
               DryDep[*, *, iMonth] = DryDep[*, *, iMonth] + $
                                      ThisDryDep * sec2day / deltaTs 
            endif else begin
               DryDep = DryDep + ThisDryDep
            endelse

         endif

      endfor

   endfor
   endfor

   ; Total Dry dep, kg
   Total = total( DryDep * area_m2 / kg2ug / nFiles )

   DryDep = DryDep / nFiles

   ; Return dry dep per day if requested
   if ( PerDay and ~ byMonth ) then begin
      nDays = SumDeltaTs / nFiles / (3600d0*24d0)
      DryDep = DryDep / nDays
   endif

return, Drydep

end
