; $Id$
;-----------------------------------------------------------------------
;+
; NAME:
;        MERCURY_BUDGET
;
; PURPOSE:
;        Calculate the atmosphere and ocean mercury budget from GEOS-Chem
;        output. 
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        MERCURY_BUDGET
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;       The atmospheric masses are currently do not discriminate between the
;       troposphere and stratosphere.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;        cdh, 27 Jun 2008: VERSION 1.00
;  19 Mar 2011 - H Amos: modifed so that multiple files can be
;                passed to mercury_budget.pro and output is an
;                avg of all files
;  07 Apr 2011 - H Amos: remove 'stop' at end of routine
;  29 May 2011 - H Amos: merge my version of mercury_buget (which accepts
;                multiples input files) with Bess' version (which has
;                been modified for mercury_benchmark.pro)
;  
;
;-
; Copyright (C) 2008, Christopher Holmes, Harvard University
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to cdh@io.as.harvard.edu
; with subject "IDL routine mercury_budget"
;-----------------------------------------------------------------------

;; Sum or average a GEOS-Chem field for specified tracer over all months in
;; the specified bpch file. Files without 12 time entries will produce errors.
function combine_months, File=File, DiagN=DiagN, Tracer=Tracer, $
		avg=avg, Land=Land, Ocean=Ocean, Troposphere=Troposphere

   ; Open data file
   CTM_Get_Data, DataInfo, DiagN, Filename=File, Tracer=Tracer

   ; Error check
   ; Note: some implementations don't have HG-SRCE tracer 12 working. This is
   ; mostly irrelevant for the benchmark, so just skip the problem here.
   If n_elements(DataInfo) ne 12 then begin
        if (tracer eq 12) and (diagn eq 'HG-SRCE') then TracerMass=!Values.f_nan else $
        Message, 'Input file does not contain 12 monthly entries for tracer'+$
	strtrim(string(Tracer),2)+' '+strtrim(DiagN,2)
        goto, skip
   endif

   GetModelAndGridInfo, DataInfo[0], ModelInfo, GridInfo

   ;; Area and volume of each grid box, result in m2
   getmodelandgridinfo, DataInfo[0], ModelInfo, GridInfo
   GridArea = ctm_boxsize( GridInfo, /M2 )

   GetModelAndGridInfo, DataInfo[0], ModelInfo, GridInfo

   ;; Retrieve land fraction if needed
   Mask = 0L
   IF Keyword_set( LAND ) or Keyword_set( OCEAN ) then begin
      Mask = 1L      
      MaskFraction = get_ctm_landfraction( GridInfo )
      If Keyword_set( OCEAN ) then $
         MaskFraction = 1D0 - MaskFraction
   endif
 
   useVolume=0L
   useTime  =0L
   if Keyword_Set(avg) then $
	useTime=1L

   ; Mass conversion factor
   ;  molec/cm2/s -> kg/grid/d
   case DataInfo[0].unit of
        'kg':   conv = 1D
        'kg/s': begin
		conv = 24D*3600D
		useTime = 1L
		end
        'molec/cm2/s': begin
		conv = 201D-3 /6.02D23 * 1D4 *(24D*3600D)*GridArea
		useTime = 1L
		end
        'pptv': begin
		conv = 201D-12 / 28D
		useVolume=1L
		end
        'v/v':  begin
		conv = 201D    / 28.9D
		useVolume=1L
		end  
        else: Message,'Unknown Units!!'
   endcase    

   ; Initialize
   TracerMass=0D

   ; Number of days per month
   days = [31D, 28D, 31D, 30D, 31D, 30D, 31D, 31D, 30D, 31D, 30D, 31D]

   For i=0, 11 do begin
	
	If (useVolume) then begin  
	        ;; Air mass in each box, kg
;        	AirMass = Get_AirMass( ModelInfo.Name, DataInfo[i].tau0 )
           AirMass = get_geos_state( 'airmas', ModelInfo, i+1 )

        	; Calculate mass        
        	MassIncrement = *(DataInfo[i].Data) * $
				AirMass * conv

	endif Else Begin

        	; Calculate mass        
        	MassIncrement = *(DataInfo[i].Data) * conv
                ; hack to avoid NaN in ocean files 
                fi2=where(finite(massincrement, /NaN))
                if (fi2[0] ne -1) then massincrement[fi2]=0

	endelse

        ; Multiply by time for weighted average or total flux (if saved as rate)
	if (useTime) then $
		MassIncrement = MassIncrement * days[i]

        If Keyword_set( Troposphere ) then begin
           ; Tropopause level
           TPL = get_geos_state( 'TP-L', ModelInfo, i+1 )
           
           ; number of vertical levels
           If Size( TracerMass, /N_dim ) gt 2 then $
              NLevels = (size( MassIncrement, /Dim ))[2] $
           else $
              NLevels = 1L    

           ; Keep only data below the tropopause level
           For L=0, NLevels-1L do begin

              ind = where( L ge TPL, ct )
              if (ct ge 1) then begin
                 ind = array_indices( MassIncrement[*, *, L], ind )
                 tmp = MassIncrement[*, *, L]
                 tmp[reform(ind[0, *]), reform(ind[1, *])] = 0d0
                 MassIncrement[*, *,L] = tmp
;                 tvmap, tmp, /cbar, /sample

              endif

           endfor

        endif



	TracerMass = TracerMass + MassIncrement
   Endfor

   ;; Fractions over land or water, if needed
   If (MASK) then begin
      
      ; number of vertical levels
      If Size( TracerMass, /N_dim ) gt 2 then $
         NLevels = (size( TracerMass, /Dim ))[2] $
         else $
         NLevels = 1L    

      ; Apply Land/Water mask to each vertical layer 
      For L=0, NLevels-1L do $
         TracerMass[*,*,L] = TracerMass[*,*,L] * MaskFraction

   endif 


   If Keyword_Set(avg) then begin
   	; Convert to mean mass, kg
   	TracerMass = Total( TracerMass /365D )

   endif else begin
   ; Convert to mean mass, kg
	TracerMass = Total( TracerMass )

   endelse

skip:
return, TracerMass
end

;; Format and print a line in the budget
;; INPUT
;;    STR - string label for the budget item (<20 characters)
;;    Value - amount of budget item, in kg or kg/y
;; KEYWORD 
;;    Rate - Units are 'Mg/a' if true and 'Mg' otherwise
;; OUTPUT
;;    None returned, but text printed to screen
pro write_line, Str, Value, Rate=Rate, Time=Time

   ; Avoid overwriting input value
   Value1 = Value

   If Keyword_set(Rate) then begin
      Value1=Value1/1e3
      unit='Mg/a' 
; Change output format (H Amos, 3 Mar 2011)
      fmt1='(A20,I7,A5)'
;      fmt1='(A20,F7.2,A5)'
   endif else if Keyword_set(Time) then begin
      unit='/a'
      fmt1='(A20,F7.2,A5)'
   endif else begin
      Value1=Value1/1e3
      unit='Mg'
      fmt1='(A20,I7,A5)'
   endelse
   
   print, Str, Value1, unit, format=fmt1

end

;------------------------------------------------------------------------
; PRO MERCURY_BUDGET
;------------------------------------------------------------------------
pro mercury_budget, file, preind=preind, $
    Hg0masstrop, Hg2masstrop, HgPmasstrop, $
    Hg0ocMass, Hg2ocMass, HgCocMass, $
    em_Hg0_anthro, em_Hg2_anthro, em_HgP_anthro, $
    em_Hg0_geo, em_Hg0_soil, em_Hg0_BB, $
    em_Hg0_land, em_Hg0_snow, em_Hg0_oc_up, $
    em_Hg0_oc_dn, em_Hg0_oc, em_total, $
    Hg0DryD, Hg2DryD, HgPDryD, Hg2wet, HgPwet, $
    pl_Hg_seasalt, dep_total, pl_Hg_Br, $
    pl_Hg_OH, pl_Hg_O3, pl_Hg_reduction, $
    t_Hg0chemtrop, t_Hg2chemtrop, t_TGMtrop

;eds 5/13/11 modified to output values
;for mercury benchmark

 ; initialize variables
 Hg0mass              = 0d0
 Hg2mass              = 0d0
 HgPmass              = 0d0
 Hg0masstrop          = 0d0
 Hg2masstrop          = 0d0
 HgPmasstrop          = 0d0
 Hg0ocMass            = 0d0
 Hg2ocMass            = 0d0
 HgCocMass            = 0d0
 Hg2wet               = 0d0
 HgPwet               = 0d0
 Hg0DryD              = 0d0
 Hg2DryD              = 0d0
 HgPDryD              = 0d0
 Hg2wet_oc            = 0d0
 HgPwet_oc            = 0d0
 Hg2DryD_oc           = 0d0
 HgPDryD_oc           = 0d0
 em_Hg0_anthro        = 0d0
 em_Hg2_anthro        = 0d0
 em_HgP_anthro        = 0d0
 em_Hg0_geo           = 0d0
 em_Hg0_oc            = 0d0
 em_Hg0_land          = 0d0
 em_Hg0_BB            = 0d0
 em_Hg0_evap          = 0d0
 em_Hg0_soil          = 0d0
 em_Hg0_snow          = 0d0
 em_Hg0_oc_up         = 0d0
 em_Hg0_oc_dn         = 0d0
 oc_HgC_sink          = 0d0
 oc_HgC_source        = 0d0
 pl_Hg_netox          = 0d0
 pl_Hg_OH             = 0d0
 pl_Hg_O3             = 0d0
 pl_Hg_seasalt        = 0d0
 pl_Hg_netox_trop     = 0d0
 pl_Hg_OH_trop        = 0d0
 pl_Hg_O3_trop        = 0d0
 pl_Hg_Br             = 0d0
 pl_Hg_Br_trop        = 0d0
 pl_Hg_reduction      = 0d0
 pl_Hg_reduction_trop = 0d0
 pl_Hg_grossox        = 0d0
 pl_Hg_grossox_trop   = 0d0
 em_anthro            = 0d0
 dep_total            = 0d0
 dep_total_oc         = 0d0
 em_total             = 0d0
 dep_total            = 0d0
 dep_total_oc         = 0d0
 t_Hg0chem            = 0d0
 t_Hg2chem            = 0d0
 t_TGM                = 0d0
 t_Hg0chemtrop        = 0d0
 t_Hg2chemtrop        = 0d0
 t_TGMtrop            = 0d0

 ; number of files
 nFiles =  n_elements(File)

 ; loop over files
 for F=0, nfiles-1L do begin
 
   ;;===============================
   ;; Determine whether the file contains the land-cycling additions
   ;; from Selin et al. 2008
   
   success = ctm_get_datablock(junk, 'HG-SRCE', File=File[F], Tracer=13 )
   If (Success) then LandModel=1L else LandModel=0L

   ;;===============================
   ;; Determine whether the file contains Hg(0)+Br diagnostic
   ;; from Selin et al. 2008
   
   success = ctm_get_datablock(junk, 'PL-HG2-$', File=File[F], Tracer=6 )
   If (Success) then BrDiagnostic=1L else BrDiagnostic=0L

   ;;===============================
   ;; Masses of atmospheric species

   Hg0mass_temp = combine_months( File=File[F], DiagN='IJ-AVG-$',Tracer=1, /avg)
   Hg2mass_temp = combine_months( File=File[F], DiagN='IJ-AVG-$',Tracer=2, /avg)
   HgPmass_temp = combine_months( File=File[F], DiagN='IJ-AVG-$',Tracer=3, /avg)

   Hg0mass =  Hg0mass + Hg0mass_temp
   Hg2mass =  Hg2mass + Hg2mass_temp
   HgPmass =  HgPmass + HgPmass_temp

   Hg0masstrop_temp = combine_months( File=File[F], DiagN='IJ-AVG-$',Tracer=1, /avg, /trop)
   Hg2masstrop_temp = combine_months( File=File[F], DiagN='IJ-AVG-$',Tracer=2, /avg, /trop)
   HgPmasstrop_temp = combine_months( File=File[F], DiagN='IJ-AVG-$',Tracer=3, /avg, /trop)

   Hg0masstrop =  Hg0masstrop + Hg0masstrop_temp
   Hg2masstrop =  Hg2masstrop + Hg2masstrop_temp
   HgPmasstrop =  HgPmasstrop + HgPmasstrop_temp

   ;;===============================
   ;; Masses of ocean species
   
   Hg0ocMass_temp = combine_months( File=File[F], DiagN='HG-SRCE',Tracer=2,/avg )
   Hg2ocMass_temp = combine_months( File=File[F], DiagN='HG-SRCE',Tracer=7,/avg )
   HgCocMass_temp = combine_months( File=File[F], DiagN='HG-SRCE',Tracer=11,/avg )

   Hg0ocMass =  Hg0ocMass + Hg0ocMass_temp
   Hg2ocMass =  Hg2ocMass + Hg2ocMass_temp
   HgCocMass =  HgCocMass + HgCocMass_temp

   ;;===============================
   ;; Atmospheric deposition fluxes 
   
   ;; Deposition over all surfaces
   Hg2WetCV = combine_months( File=File[F], DiagN='WETDCV-$',Tracer=2 )  
   Hg2WetLS = combine_months( File=File[F], DiagN='WETDLS-$',Tracer=2 )  
   Hg2wet_temp   = Hg2WetCV + Hg2WetLS

   Hg2wet =  Hg2wet + Hg2wet_temp

   HgPWetCV = combine_months( File=File[F], DiagN='WETDCV-$',Tracer=3 )  
   HgPWetLS = combine_months( File=File[F], DiagN='WETDLS-$',Tracer=3 )  
   HgPWet_temp   = HgPWetCV + HgPWetLS

   HgPWet =  HgPWet + HgPWet_temp

   Hg0DryD_temp  = combine_months( File=File[F], DiagN='DRYD-FLX',Tracer=1 )
   Hg2DryD_temp  = combine_months( File=File[F], DiagN='DRYD-FLX',Tracer=2 )
   HgPDryD_temp  = combine_months( File=File[F], DiagN='DRYD-FLX',Tracer=3 )

   Hg0DryD =  Hg0DryD + Hg0DryD_temp
   Hg2DryD =  Hg2DryD + Hg2DryD_temp
   HgPDryD =  HgPDryD + HgPDryD_temp

   ;; Deposition over water 
   ;; (NOTE THAT THIS INCLUDES DEPOSITION TO OCEAN, FRESH WATER AND SEA ICE)
   ;; Deposition over all surfaces
   Hg2WetCV_oc = combine_months( File=File[F], DiagN='WETDCV-$',Tracer=2, /Ocean )   
   Hg2WetLS_oc = combine_months( File=File[F], DiagN='WETDLS-$',Tracer=2, /Ocean )   
   Hg2wet_oc_temp   = Hg2WetCV_oc + Hg2WetLS_oc

   Hg2wet_oc =  Hg2wet_oc + Hg2wet_oc_temp

   HgPWetCV_oc = combine_months( File=File[F], DiagN='WETDCV-$',Tracer=3, /Ocean )   
   HgPWetLS_oc = combine_months( File=File[F], DiagN='WETDLS-$',Tracer=3, /Ocean )   
   HgPWet_oc_temp   = HgPWetCV_oc + HgPWetLS_oc

   HgPwet_oc =  HgPwet_oc + HgPwet_oc_temp

   Hg2DryD_oc_temp  = combine_months( File=File[F], DiagN='DRYD-FLX',Tracer=2, /Ocean )
   HgPDryD_oc_temp  = combine_months( File=File[F], DiagN='DRYD-FLX',Tracer=3, /Ocean )

   Hg2DryD_oc =  Hg2DryD_oc + Hg2DryD_oc_temp
   HgPDryD_oc =  HgPDryD_oc + HgPDryD_oc_temp

   ; (Hg0 dry deposition to ocean handled in OCEAN-HG diagnostic)

   ;;===============================
   ;; Emissions

   em_Hg0_anthro_temp = combine_months( File=File[F], DiagN='HG-SRCE',Tracer=1)
   em_Hg2_anthro_temp = combine_months( File=File[F], DiagN='HG-SRCE',Tracer=6)
   em_HgP_anthro_temp = combine_months( File=File[F], DiagN='HG-SRCE',Tracer=9)

   em_Hg0_anthro =  em_Hg0_anthro + em_Hg0_anthro_temp
   em_Hg2_anthro =  em_Hg2_anthro + em_Hg2_anthro_temp
   em_HgP_anthro =  em_HgP_anthro + em_HgP_anthro_temp

   ; net ocean emission
   em_Hg0_oc_temp   = combine_months( File=File[F], DiagN='HG-SRCE',Tracer=3)
  
   em_Hg0_oc =  em_Hg0_oc + em_Hg0_oc_temp

   ; geogenic
   em_Hg0_geo_temp  = combine_months( File=File[F], DiagN='HG-SRCE',Tracer=5)
   em_Hg0_land_temp = combine_months( File=File[F], DiagN='HG-SRCE',Tracer=4)

   em_Hg0_geo =  em_Hg0_geo + em_Hg0_geo_temp
   em_Hg0_land = em_Hg0_land + em_Hg0_land_temp
   

   If (LandModel) then begin
   	em_Hg0_BB_temp   = combine_months( File=File[F], DiagN='HG-SRCE',Tracer=13)
   	em_Hg0_evap_temp = combine_months( File=File[F], DiagN='HG-SRCE',Tracer=14)
   	em_Hg0_soil_temp = combine_months( File=File[F], DiagN='HG-SRCE',Tracer=15)
; Update tracer number (H Amos, 3 Mar 2011)
;   	em_Hg0_snow_temp = combine_months( File=File[F], DiagN='HG-SRCE',Tracer=26)
   	em_Hg0_snow_temp = combine_months( File=File[F], DiagN='HG-SRCE',Tracer=18)

        em_Hg0_BB   = em_Hg0_BB   + em_Hg0_BB_temp
        em_Hg0_evap = em_Hg0_evap + em_Hg0_evap_temp
        em_Hg0_soil = em_Hg0_soil + em_Hg0_soil_temp
        em_Hg0_snow = em_Hg0_snow + em_Hg0_snow_temp

        ; gross ocean emission
        em_Hg0_oc_up_temp = combine_months( File=File[F], DiagN='HG-SRCE',Tracer=16)
   
        em_Hg0_oc_up =  em_Hg0_oc_up + em_Hg0_oc_up_temp

        ; gross ocean uptake
        em_Hg0_oc_dn_temp = combine_months( File=File[F], DiagN='HG-SRCE',Tracer=17)
         
        em_Hg0_oc_dn =  em_Hg0_oc_dn + em_Hg0_oc_dn_temp
   Endif

   ;;===============================
   ;; Chemical and Ocean fluxes

   oc_HgC_sink_temp   = combine_months( File=File[F],DiagN='HG-SRCE', Tracer=8 )
   oc_HgC_source_temp = combine_months( File=File[F],DiagN='HG-SRCE', Tracer=12 )

   oc_HgC_sink        = oc_HgC_sink    + oc_HgC_sink_temp
   oc_HgC_source      =  oc_HgC_source + oc_HgC_source_temp

   pl_Hg_netox_temp   = combine_months( File=File[F],DiagN='PL-HG2-$', Tracer=1 )
   pl_Hg_OH_temp      = combine_months( File=File[F],DiagN='PL-HG2-$', Tracer=2 )
   pl_Hg_O3_temp      = combine_months( File=File[F],DiagN='PL-HG2-$', Tracer=3 )
   pl_Hg_seasalt_temp = combine_months( File=File[F],DiagN='PL-HG2-$', Tracer=4 )

   pl_Hg_netox        = pl_Hg_netox   + pl_Hg_netox_temp
   pl_Hg_OH           = pl_Hg_OH      + pl_Hg_OH_temp
   pl_Hg_O3           = pl_Hg_O3      + pl_Hg_O3_temp
   pl_Hg_seasalt      = pl_Hg_seasalt + pl_Hg_seasalt_temp

   ; troposphere only
   pl_Hg_netox_trop_temp   = combine_months( File=File[F],DiagN='PL-HG2-$', Tracer=1, /trop)
   pl_Hg_OH_trop_temp      = combine_months( File=File[F],DiagN='PL-HG2-$', Tracer=2, /trop)
   pl_Hg_O3_trop_temp      = combine_months( File=File[F],DiagN='PL-HG2-$', Tracer=3, /trop)

   pl_Hg_netox_trop        = pl_Hg_netox_trop + pl_Hg_netox_trop_temp
   pl_Hg_OH_trop           = pl_Hg_OH_trop    + pl_Hg_OH_trop_temp
   pl_Hg_O3_trop           = pl_Hg_O3_trop    + pl_Hg_O3_trop_temp

   if (BrDiagnostic) then begin
      pl_Hg_Br_temp       = combine_months( File=File[F], DiagN='PL-HG2-$', Tracer=6 )
      pl_Hg_Br_trop_temp  = combine_months( File=File[F], DiagN='PL-HG2-$', Tracer=6, /trop)

      pl_Hg_Br            = pl_Hg_Br      + pl_Hg_Br_temp
      pl_Hg_Br_trop       = pl_Hg_Br_trop + pl_Hg_Br_trop_temp

;      pl_Hg_reduction_temp      = pl_Hg_OH + pl_Hg_O3 + pl_Hg_Br - pl_Hg_netox
;      pl_Hg_reduction_trop_temp = pl_Hg_OH_trop + pl_Hg_O3_trop + pl_Hg_Br_trop - pl_Hg_netox_trop
      pl_Hg_reduction_temp      = pl_Hg_OH_temp + pl_Hg_O3_temp + pl_Hg_Br_temp - pl_Hg_netox_temp
      pl_Hg_reduction_trop_temp = pl_Hg_OH_trop_temp + pl_Hg_O3_trop_temp + $
                                  pl_Hg_Br_trop_temp - pl_Hg_netox_trop_temp

      pl_Hg_reduction           = pl_Hg_reduction      + pl_Hg_reduction_temp
      pl_Hg_reduction_trop      = pl_Hg_reduction_trop + pl_Hg_reduction_trop_temp

   endif else begin
;      pl_Hg_reduction_temp = pl_Hg_OH + pl_Hg_O3 - pl_Hg_netox
;      pl_Hg_reduction_trop_temp = pl_Hg_OH_trop + pl_Hg_O3_trop - pl_Hg_netox_trop
      pl_Hg_reduction_temp = pl_Hg_OH_temp + pl_Hg_O3_temp - pl_Hg_netox_temp
      pl_Hg_reduction_trop_temp = pl_Hg_OH_trop_temp + pl_Hg_O3_trop_temp - pl_Hg_netox_trop_temp

      pl_Hg_reduction           = pl_Hg_reduction      + pl_Hg_reduction_temp
      pl_Hg_reduction_trop      = pl_Hg_reduction_trop + pl_Hg_reduction_trop_temp
   endelse

;   pl_Hg_grossox_temp      = pl_Hg_netox      + pl_Hg_reduction
;   pl_Hg_grossox_trop_temp = pl_Hg_netox_trop + pl_Hg_reduction_trop
   pl_Hg_grossox_temp      = pl_Hg_netox_temp      + pl_Hg_reduction_temp
   pl_Hg_grossox_trop_temp = pl_Hg_netox_trop_temp + pl_Hg_reduction_trop_temp

   pl_Hg_grossox           = pl_Hg_grossox      + pl_Hg_grossox_temp
   pl_Hg_grossox_trop      = pl_Hg_grossox_trop + pl_Hg_grossox_trop_temp

   ;;===============================
   ;; Some aggregate quantities

   ; Total anthropogenic emissions
;   em_anthro_temp = em_Hg0_anthro + em_Hg2_anthro + em_HgP_anthro
   em_anthro_temp = em_Hg0_anthro_temp + em_Hg2_anthro_temp + em_HgP_anthro_temp

   em_anthro = em_anthro + em_anthro_temp
	
   ; Total emissions
;   em_total_temp = em_anthro + em_Hg0_geo + em_Hg0_land
   em_total_temp = em_anthro_temp + em_Hg0_geo_temp + em_Hg0_land_temp

;   em_total = em_total + em_total_temp

   if (LandModel) then begin
	; with gross ocean flux
;  	em_total_temp = em_total + em_Hg0_BB + em_Hg0_evap + em_Hg0_soil + $
;		em_Hg0_oc_up + em_Hg0_snow
  	em_total_temp = em_total_temp + em_Hg0_BB_temp + em_Hg0_evap_temp + em_Hg0_soil_temp + $
		em_Hg0_oc_up_temp + em_Hg0_snow_temp

        em_total = em_total + em_total_temp
   
  	; Total Deposition, includes gross ocean flux of Hg0
;   	dep_total_temp = Hg0DryD + Hg2DryD + HgPDryD + Hg2Wet + HgPWet + em_Hg0_oc_dn + pl_Hg_seasalt
   	dep_total_temp = Hg0DryD_temp + Hg2DryD_temp + HgPDryD_temp + Hg2Wet_temp $
                         + HgPWet_temp + em_Hg0_oc_dn_temp + pl_Hg_seasalt_temp
        
        dep_total = dep_total + dep_total_temp        

        ; Total Deposition to ocean, includes gross ocean flux of Hg0
;        dep_total_oc_temp = Hg2DryD_oc + HgPDryD_oc + Hg2Wet_oc + HgPWet_oc +$
;                            em_Hg0_oc_dn + pl_Hg_seasalt
        dep_total_oc_temp = Hg2DryD_oc_temp + HgPDryD_oc_temp + Hg2Wet_oc_temp + HgPWet_oc_temp +$
                            em_Hg0_oc_dn_temp + pl_Hg_seasalt_temp

        dep_total_oc =  dep_total_oc + dep_total_oc_temp
   endif else begin
	; net ocean flux only
;	em_total_temp = em_total + em_Hg0_oc 
	em_total_temp = em_total_temp + em_Hg0_oc_temp 

        em_total = em_total + em_total_temp        

  	; Total Deposition, excludes ocean uptake of Hg0
;   	dep_total_temp = Hg0DryD + Hg2DryD + HgPDryD + Hg2Wet + HgPWet + pl_Hg_seasalt
   	dep_total_temp = Hg0DryD_temp + Hg2DryD_temp + HgPDryD_temp +$
                         Hg2Wet_temp + HgPWet_temp + pl_Hg_seasalt_temp

        dep_total = dep_total + dep_total_temp        

        ; Total Deposition to ocean, exclude ocean uptake of Hg0
;        dep_total_oc_temp = Hg2DryD_oc + HgPDryD_oc + Hg2Wet_oc + HgPWet_oc + pl_Hg_seasalt
        dep_total_oc_temp = Hg2DryD_oc_temp + HgPDryD_oc_temp + Hg2Wet_oc_temp + $
                            HgPWet_oc_temp + pl_Hg_seasalt_temp

        dep_total_oc =  dep_total_oc + dep_total_oc_temp        
   endelse

   ;;===============================
   ;; Lifetimes
   t_Hg0chem_temp = Hg0mass / pl_Hg_grossox
   t_Hg2chem_temp = Hg2mass / pl_Hg_reduction

   t_Hg0chem = t_Hg0chem + t_Hg0chem_temp
   t_Hg2chem = t_Hg2chem + t_Hg2chem_temp
   
   t_TGM_temp = (Hg0mass + Hg2mass) / ( dep_total )

   t_TGM = t_TGM + t_TGM_temp

   ; tropospheric lifetimes
   t_Hg0chemtrop_temp = Hg0masstrop / pl_Hg_grossox_trop
   t_Hg2chemtrop_temp = Hg2masstrop / pl_Hg_reduction_trop

   t_Hg0chemtrop = t_Hg0chemtrop + t_Hg0chemtrop_temp
   t_Hg2chemtrop = t_Hg2chemtrop + t_Hg2chemtrop_temp   
   
   t_TGMtrop_temp = (Hg0masstrop + Hg2masstrop) / ( dep_total )

   t_TGMtrop = t_TGMtrop + t_TGMtrop_temp

  endfor ; end loop over n # of files

   ;;===============================
   ;; Print the budget (avg of all files)

   Print,' '
   print,' '
   print,'===================================='
   print,'          Reservoir Masses'
   print,'          ----------------'
   print, 'Troposphere+Stratosphere'
   write_line,'Hg0 atm:  ',Hg0mass / nfiles
   write_line,'Hg2 atm:  ',Hg2mass / nfiles
   write_line,'HgP atm:  ',HgPmass / nfiles
   print, ''
   print, 'Troposphere'
   write_line,'Hg0 atm:  ',Hg0masstrop / nfiles
   write_line,'Hg2 atm:  ',Hg2masstrop / nfiles
   write_line,'HgP atm:  ',HgPmasstrop / nfiles
   print,''
   write_line,'Hg0 oc:  ',Hg0ocMass / nfiles
   write_line,'Hg2 oc:  ',Hg2ocMass / nfiles
   write_line,'HgP oc:  ',HgCocMass / nfiles
   print,''
   print,''
   print,'==================================='
   print,'              Emissions'
   print,'           ----------------'
   print,'Anthropogenic'
   write_line,'   Hg0 :  ',em_Hg0_anthro / nfiles, /Rate
   write_line,'   Hg2 :  ',em_Hg2_anthro / nfiles, /Rate
   write_line,'   HgP :  ',em_HgP_anthro / nfiles, /Rate
   write_line,'  TOTAL Anthro:  ',em_anthro / nfiles, /Rate
   print,''
   write_line,'Geogenic:  ',em_Hg0_geo / nfiles, /Rate
   IF (LandModel) then begin
   print,''
   print,'Terrestrial biosphere'
   write_line,'   Soil:  ',em_Hg0_soil / nfiles, /Rate
   write_line,'   BB:  ',em_Hg0_BB/ nfiles, /Rate
   write_line,'   Evapotrans:  ',em_Hg0_evap / nfiles, /Rate
   write_line,'   Fast Recycling:  ',em_Hg0_land / nfiles, /Rate
   write_line,'   Snow  ',em_Hg0_snow / nfiles, /Rate
   write_line,'   TOTAL Terr:  ',(em_Hg0_soil+em_Hg0_BB+em_Hg0_evap+em_Hg0_land+em_Hg0_snow) / nfiles, /Rate
   print,''
   print,'Ocean Hg0 exchange'
   write_line,'  Gross evasion:  ',em_Hg0_oc_up / nfiles, /Rate
   write_line,'  Gross uptake:  ',em_Hg0_oc_dn / nfiles, /Rate
   write_line,'  NET ocean:  ',em_Hg0_oc / nfiles, /Rate
   ENDIF else begin   
   write_line,'Terr. Biosphere:  ',em_Hg0_land / nfiles, /Rate
   write_line,'Net ocean:  ',em_Hg0_oc / nfiles, /Rate
   ENDELSE
   print,''
   write_line,'TOTAL EMISSION:  ',em_total / nfiles, /Rate
   print,''
   print,'==================================='
   print,'          Depsition Fluxes'
   print,'          ----------------'
   print,'Deposition to Land and Water'
   write_line,'Hg0 dry(land):  ',Hg0DryD / nfiles, /Rate
   If (LandModel) then $
   write_line,'Hg0 dry(ocean):  ',em_Hg0_oc_dn / nfiles,/Rate
   write_line,'Hg2 dry:  ',Hg2DryD / nfiles, /Rate
   write_line,'HgP dry:  ',HgPDryD / nfiles, /Rate
   print,''
   write_line,'Hg2 wet:  ',Hg2wet / nfiles, /Rate
   write_line,'HgP wet:  ', HgPwet / nfiles, /Rate
   print,''
   print,'Deposition to Water'
   write_line,'Hg2 dry:  ',Hg2DryD_oc / nfiles, /Rate
   write_line,'HgP dry:  ',HgPDryD_oc / nfiles, /Rate
   print,''
   write_line,'Hg2 wet:  ',Hg2wet_oc / nfiles, /Rate
   write_line,'HgP wet:  ', HgPwet_oc / nfiles, /Rate
   print,''
   write_line,'Hg2 sea salt:  ',pl_Hg_seasalt / nfiles, /Rate
   print,''
   write_line,'TOTAL DEPOSITION:  ',dep_total / nfiles, /Rate
print, dep_total / nfiles
   write_line,'TOTAL WATER DEP:  ',dep_total_oc / nfiles, /Rate
   write_line,'TOTAL LAND DEP:  ',(dep_total-dep_total_oc) / nfiles, /Rate
   print,''
   print,'==================================='
   print,'          Other Fluxes'
   print,'          ----------------'
   write_line,'HgC sinking:  ',oc_HgC_sink / nfiles, /Rate
   write_line,'C sinking:  ',oc_HgC_source / nfiles, /Rate
print, 'oc_HgC_source', oc_HgC_source / nfiles
   print,''
   if (BrDiagnostic) then $
   write_line, 'Hg0+Br:  ', pl_Hg_Br / nfiles, /Rate
print, pl_Hg_Br / nfiles
   write_line,'Hg0+OH:  ',pl_Hg_OH / nfiles, /Rate
   write_line,'Hg0+O3:  ',pl_Hg_O3 / nfiles, /Rate
   write_line, 'Hg2 reduction:  ', pl_Hg_reduction / nfiles, /Rate
   print,''
   print,'==================================='
   print,'          Lifetimes'
   print,'          ----------------'
   print, 'Troposphere+Stratosphere'
   write_line,'Hg0 chemical:  ',t_Hg0chem / nfiles, /time
   write_line,'Hg2 chemical:  ',t_Hg2chem / nfiles, /time
   write_line,'Hg0+Hg2 atm:  ',t_TGM / nfiles, /time
   print,''
   print, 'Troposphere'
   write_line,'Hg0 chemical:  ',t_Hg0chemtrop / nfiles, /time
   write_line,'Hg2 chemical:  ',t_Hg2chemtrop / nfiles, /time
   write_line,'Hg0+Hg2 atm:  ',t_TGMtrop / nfiles, /time
   print,''
   print,''
   print,''


 
end
