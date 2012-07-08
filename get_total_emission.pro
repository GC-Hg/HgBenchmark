function get_total_emission, FileName=FileName, $
                           Source=Source, $
                           Species=Species, $
                           Months=Months, $
                           YearOut=YearOut, $
                           GridInfoOut=GridInfoOut, $
                           total=total, $
                           PerDay=PerDay, $
                           noCheck=noCheck, $
                           kg=kg, $
                           _Extra=_Extra
   
; Get total emissions for specified species and source
; Return the result as ug/m2/y

   ;=======================================
   ; Setup
   ;=======================================

   if ( not Keyword_set( FileName ) ) then $
      FileName = '~/runs/HgBr.v8-01-01/figs/ctm.bpch.stdOx'

   if ( not Keyword_set( Species ) ) then $
     Species = 'Hg0'
   
   if ( not Keyword_set( Source ) ) then $
      Source='anthro'

   if ( not Keyword_set( Months ) ) then $
     Months = indgen( 12 )+1 

   ; Convert kg to ug
   kg2ug = 1d9

   ; Treat a few special cases of sources
;   case strlowcase( Source ) of
;      'all': Source = ['anthro', 'bioburn', 'ocean', 'rapid', 'geo', $
;                       'transpir', 'soil', 'snow']
;      'natural': Source = ['ocean', 'rapid', 'geo', 'transpir', 'soil', $
;                           'snow']
;      else:
;   endcase

   ; keyword to return result at ug/m2/d
   PerDay= keyword_set( PerDay )

   ; We require 12 monthly entries, unless noCheck is True
   doCheck = ~keyword_set( noCheck )

   ;=======================================
   ; Extract GEOS-Chem Data
   ;=======================================

   nFiles=n_elements( FileName )

   SumDeltaTs = 0d0
   Emission = 0d0

   for F=0L, nFiles-1L do begin
   for i=0L, n_elements(Species)-1L do begin
   for S=0L, n_elements(Source)-1L do begin

      tracer = 0L

      ; Select tracer number for the specified species and source type
      case strlowcase(Species[i]) of
         'hg0': begin
            case strlowcase(Source[S]) of
               'anthro': tracer = 1L
;               'ocean':  tracer = 16L ;eds
               'ocean_net': tracer=3L
               'rapid':  tracer = 4L
               'geo':    tracer = 5L
               'bioburn':tracer = 13L
               'transpir':tracer= 14L
               'soil':   tracer = 15L
;               'snow':   tracer = 26L
               'snow':  tracer = 18L ;eds
               'fxup' : tracer = 16L ;eds 5/12/11 new tracers
               'fxdn' : tracer = 17L
;               'hg0aq' : tracer = 2
;               'hgtaq' : tracer = 10L
            endcase
            end
         'hg2':begin
            case strlowcase(Source[S]) of
               'anthro': tracer = 6L
;               'hg2aq' : tracer = 7L ;eds
               'sink' : tracer = 8L ;eds
            endcase
            end
         'hgp':begin
            case strlowcase(Source[S]) of
               'anthro': tracer = 9L
;               'hgpaq' : tracer = 11L ;eds
;               'hgtop' : tracer = 12L ;eds
            endcase
            end
      endcase

      ; Error message if tracer number is not found above
      if ( tracer eq 0L ) then begin

         message, string(Source[S], Species[i], $
                       format='("Source-species pair unknown:",A,",",A)' )
      endif

      ; Emission flux [kg/month]
      ctm_get_data, DataInfo, 'HG-SRCE', Filename=filename[F], $
                    Tracer=Tracer
      
      ; Number of time steps in the DataInfo structures (should all be same)
      n_times = n_elements( DataInfo )

      ; Earlier versions of GEOS-Chem did not have snow emissions, so
      ; skip this entry if no snow data are present
      if ( n_times eq 0 and strmatch( source[S], 'snow', /fold_case ) ) then $
         continue

      ; check whether there are 12 times in the file (assume they are months)
      if ( n_times NE 12 and (doCheck) ) then $
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

            ; Calculate Cumulative emission [ug/m2]
            if keyword_set(kg) then Emission = Emission + *(DataInfo[iMonth].Data) $
            else Emission = Emission + $
                     *(DataInfo[iMonth].Data) * kg2ug / area_m2
            

         endif

      endfor

   endfor
   endfor
   endfor

   ; Total emission, kg
   total = total( Emission * area_m2 / kg2ug )

   Emission = Emission / nFiles

   ; Return emission per day if requested
   if ( PerDay ) then begin
      nDays = SumDeltaTs / nFiles / (3600d0*24d0)
      Emission = Emission / nDays
   endif
 
return, emission

end
