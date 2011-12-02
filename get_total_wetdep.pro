function get_total_wetdep, FileName=FileName, $
                           Species=Species, $
                           Months=Months, $
                           YearOut=YearOut, $
                           GridInfoOut=GridInfoOut, $
                           Total=Total, $
                           bymonth=bymonth, $
                           perDay=perDay, $
                          noCheck=noCheck, $
                           _Extra=_Extra
   
; Get total wetdep for convective and stratiform precipitation
; Return the result as ug/m2/y

   ;=======================================
   ; Setup
   ;=======================================

;   if ( not Keyword_set( FileName ) ) then $
;      FileName = '~/runs/HgBr.v8-01-01/ctm.bpch.stdOx'

   if ( not Keyword_set( Species ) ) then $
     Species = ['Hg2', 'HgP']

   if ( not Keyword_set( Months ) ) then $
     Months = indgen( 12 )+1 

   ; Convert kg to ug
   kg2ug = 1d9

   ; Convert kg to Mg
   kg2Mg = 1d-3

   ; Convert sec -> day
   sec2day = 86400.

   ByMonth = Keyword_set( ByMonth )

   ; keyword to return result at ug/m2/d
   PerDay= keyword_set( PerDay )

   ; We require 12 monthly entries, unless noCheck is True
   doCheck = ~keyword_set( noCheck )

   ;=======================================
   ; Extract GEOS-Chem Data
   ;=======================================

   nFiles=n_elements( FileName )

   WetDep = 0d0
   TotDeltaTs = 0d0
   FIRST = 1L

   ; Wet deposition flux (due to convection) [kg/s]
   s= ctm_get_datablock( tmp, 'WETDCV-$', Filename=filename[0], $
                 Tracer=2, lev=1, /first )

   ; Set up size of output variable
   if keyword_set( ByMonth ) then $
      WetDep = fltarr(  [size(tmp, /dim), n_elements( months ) ] ) $
   else $
      WetDep = fltarr(  size(tmp, /dim) )

   for F=0L, nFiles-1L do begin
   for i=0L, n_elements(Species)-1L do begin

      case strlowcase(Species[i]) of
         'hg0': message, 'No Hg0 wet deposition'
         'hg2': tracer = 2
         'hgp': tracer = 3
      endcase

      ; Wet deposition flux (due to convection) [kg/s]
      ctm_get_data, WDcvDataInfo, 'WETDCV-$', Filename=filename[F], $
                    Tracer=Tracer
      
      ; Wet deposition flux (due to large scale motion) [kg/s]
      ctm_get_data, WDlsDataInfo, 'WETDLS-$', Filename=filename[F], $
                    Tracer=Tracer

      ; Number of time steps in the DataInfo structures (should all be same)
      n_times = n_elements( WDcvDataInfo )
   
      ; check whether there are 12 times in the file (assume they are months)
      if ( n_times NE 12 and (doCheck) ) then $
        message, 'File does not contain 12 monthly means'

      ; Which year is the model?
      YearOut = FIRST ?  ( tau2yymmdd(  WDcvDataInfo[0].Tau0 ) ).year : $
                [YearOut, ( tau2yymmdd(  WDcvDataInfo[0].Tau0 ) ).year]
      FIRST = 0L

      ; Get grid info
      getmodelandgridinfo, WDcvDataInfo[0], ModelInfoOut, GridInfoOut

      ; Get grid area [m2]
      area_m2  = ctm_boxsize( GridInfoOut, /m2 )

      for iMonth=0L, n_times-1L do begin
         
         ; Total only requested months
         tmp = where( Months eq iMonth+1, ct )
         if (ct ge 1) then begin

            ; Start and end times of data entry [hr]
            tau0 = WDcvDataInfo[iMonth].Tau0
            tau1 = WDcvDataInfo[iMonth].Tau1
            
            ; Elapsed time during data collection [s]
            deltaTs  = (Tau1-Tau0) * 3600.

            ; Calculate Wet Dep [ug/m2/yr]
            ThisWetDep = total( $
                     *(WDcvDataInfo[iMonth].Data) + $
                     *(WDlsDataInfo[iMonth].Data), 3) $
                     * kg2ug * deltaTs / area_m2 
            ;ThisWetDep =  $
            ;         *(WDcvDataInfo[iMonth].Data) + $
            ;         *(WDlsDataInfo[iMonth].Data)
            ;ThisWetDep =  ThisWetDep[*,*,0] $
            ;         * kg2ug * deltaTs / area_m2 
            ;ThisWetDep = ( total( $
            ;         *(WDlsDataInfo[iMonth].Data), 3) + $
            ;         (*(WDcvDataInfo[iMonth].Data))[*,*,0] ) $
            ;         * kg2ug * deltaTs / area_m2 

            if ByMonth then begin
               WetDep[*, *, iMonth] = WetDep[*, *, iMonth] + $
                                      ThisWetDep * sec2day / deltaTs 
            endif else begin
               WetDep = WetDep + ThisWetDep
            endelse

            TotDeltaTs = TotDeltaTs + DeltaTs

         endif

      endfor

   endfor
   endfor

   ; Total Wet dep, kg
   Total = total( WetDep * area_m2 / kg2ug / nFiles )

   WetDep = WetDep / nFiles
   
   ; Return dry dep per day if requested
   if ( PerDay and ~ byMonth ) then begin
      nDays = TotDeltaTs / nFiles / (3600d0*24d0)
      WetDep = WetDep / nDays
   endif

   ; Keep only unique years
   YearOut = YearOut[ uniq( YearOut, sort(YearOut) ) ]




return, WetDep

end
