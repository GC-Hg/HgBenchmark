function get_total_ocean_mass, FileName=FileName, $
                           Species=Species, $
                           Months=Months, $
                           YearOut=YearOut, $
                           GridInfoOut=GridInfoOut, $
                           total=total, $
                           PerDay=PerDay, $
                           noCheck=noCheck, $
                           _Extra=_Extra
 
;eds 5/12/12 modified from get_total_emissions
  
; Get total mass for specified species [kg]
; averaged over months

   ;=======================================
   ; Setup
   ;=======================================

;   if ( not Keyword_set( FileName ) ) then $
;      FileName = '~/runs/HgBr.v8-01-01/figs/ctm.bpch.stdOx'

   if ( not Keyword_set( Species ) ) then $
     Species = 'Hg0'
   
   if ( not Keyword_set( Months ) ) then $
     Months = indgen( 12 )+1 


   ; We require 12 monthly entries, unless noCheck is True
   doCheck = ~keyword_set( noCheck )

   ;=======================================
   ; Extract GEOS-Chem Data
   ;=======================================

   nFiles=n_elements( FileName )

   SumDeltaTs = 0d0
   Mass = 0d0

   for F=0L, nFiles-1L do begin
   for i=0L, n_elements(Species)-1L do begin

      tracer = 0L

      ; Select tracer number for the specified species and source type
      case strlowcase(Species[i]) of
               'hg0aq' : tracer = 2L
               'hgtaq' : tracer = 10L
               'hg2aq' : tracer = 7L ;eds
               'hgpaq' : tracer = 11L ;eds
               'hgtop' : tracer = 12L ;eds
      endcase

      ; Error message if tracer number is not found above
      if ( tracer eq 0L ) then begin

         message, string(Species[i], $
                       format='("Species unknown:,"A")' )
      endif

      ; Mass [kg]
      ctm_get_data, DataInfo, 'HG-SRCE', Filename=filename[F], $
                    Tracer=Tracer
      
      ; Number of time steps in the DataInfo structures (should all be same)
      n_times = n_elements( DataInfo )

      ; check whether there are 12 times in the file (assume they are months)
      if ( n_times NE 12 and (doCheck) ) then $
        message, 'File does not contain 12 monthly means'

      ; Which year is the model?
      YearOut = ( tau2yymmdd(  DataInfo[0].Tau0 ) ).year

      ; Get grid info
      getmodelandgridinfo, DataInfo[0], ModelInfoOut, GridInfoOut

      for iMonth=0L, n_times-1L do begin
         
         ; Total only requested months
         tmp = where( Months eq iMonth+1, ct )
         if (ct ge 1) then begin

            ; Calculate Cumulative Mass [kg]
            Mass = Mass + $
                     *(DataInfo[iMonth].Data) 
            

         endif

      endfor

   endfor
   endfor

   Mass = Mass / nFiles
   Mass = Mass / n_elements(Months)

return, Mass

end
