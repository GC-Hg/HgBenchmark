function get_geos_state, Variable, ModelInfo, Month, $
                         xmid=xmid, ymid=ymid, zmid=zmid, $
                         _Extra=_Extra

   ; Available Variables:
   ; LWI, SNOMAS, FSNOW, PRESSURE, TROPOPAUSE, TP-L, TP-H, TP-P, 
   ; UWND, VWND, TMPU, TEMPERATURE, SPHU, KZZ, MOISTQ, BXHGHT, AIRMAS,
   ; AVGW, N(AIR), HFLUX, RADSWG, PREACC, PRECON, TS, RADSWT, USTAR,
   ; Z0, PBL, CLDFRC, U10M, V10M, PS-PBL, ALBC, PHIS, CLDTOP, TROPP, SLP,
   ; TSKIN, PARDF, PARDR, GWET


   ;----------------------
   ; GEOS Files
   ;----------------------

   case ModelInfo.name of
      'GEOS4': year = 2005L
      'GEOS4_30L': year = 2005L
      'GEOS5': year = 2007L
      'GEOS5_47L': year = 2007L
      'MERRA': year = 2008L
      'MERRA_47L': year = 2008L
      'MERRA2_47L': year = 2008L
      'GEOSFP_47L': year = 2008L
      else: message, 'No saved atmospheric data for GEOS type: '+ModelInfo.name
   endcase


   gc_dir = !BENCHMARK+'/GEOS-Chem_fields/'

   if ( ModelInfo.name eq 'MERRA2_47L' ) then $
      ModelInfo.name = 'MERRA_47L'
   if ( ModelInfo.name eq 'GEOSFP_47L' ) then $
      ModelInfo.name = 'MERRA_47L'
   gc_file = gc_dir + $
             'state.'+ModelInfo.name +'.'+CTM_ResExt( ModelInfo )+'.%YYYY%.bpch'
   gc_file = replace_token( gc_file, 'YYYY', strtrim(string(year), 2))
   
   
   gc_tau0 = nymd2tau( year*10000L+month*100L+1L )

   ;----------------------
   ; Special variables not in the standard files
   ;----------------------

   case strlowcase( Variable ) of

      'lwi': begin
         if ( ModelInfo.resolution[0] ne 5 ) then $
            message, 'Only 4x5 resolution available for LWI'
         restore, gc_dir+'LWI.GEOS_5.4x5.mean.sav'
         xmid=GridInfo.xmid
         ymid=GridInfo.ymid
         return, LWI[*, *, month-1L]
      end
      'snomas': begin
         if ( ModelInfo.resolution[0] ne 5 ) OR $
            not strmatch( ModelInfo.name, 'GEOS5*' ) then $
            message, 'Only GEOS5 4x5 resolution available for snomas'
         restore, gc_dir+'snowcover.GEOS_5.4x5.mean.sav'
         xmid=GridInfo.xmid
         ymid=GridInfo.ymid
         return, snomas[*, *, month-1L]
      end
      'fsnow': begin
         if ( ModelInfo.resolution[0] ne 5 ) OR $
            not strmatch( ModelInfo.name, 'GEOS5*' ) then $
            message, 'Only GEOS5 4x5 resolution available for snomas'
         restore, gc_dir+'snowcover.GEOS_5.4x5.mean.sav'
         xmid=GridInfo.xmid
         ymid=GridInfo.ymid
         ; Fraction of time during which snow depth > 1mm H2O
         return, fsnow[*, *, month-1L]
      end
      else:
   endcase



   ;----------------------
   ; Read Data from GEOS files
   ;----------------------
   
   case strlowcase( Variable ) of

      'pressure': begin
   
        ;Extract pressure from data file
         success = ctm_get_datablock( Pedges, $
                                      'PEDGE-$', $
                                      tracer=1, $
                                      filename=gc_file,               $
                                      tau0=gc_tau0, $
                                      xmid=xmid, ymid=ymid, zmid=zmid,  $
                                      gridinfo=GC_grid, $
                                      modelinfo=GC_model, $
                                      _Extra=_Extra )

         if ( ~success ) then message, 'Unable to get Pressure'

         ; Filter the pressure edges to get the pressures at grid centers
         state = fltarr( GC_grid.imx, GC_grid.jmx, GC_grid.lmx )

         for i=0L, GC_grid.imx-1L do begin
         for j=0L, GC_grid.jmx-1L do begin
    
            ; convolution with a boxcar average,
            ; drop the first element (always 0)
            state[i, j, *] = (convol( reform(Pedges[i, j, *]), $
                                     [0.5, 0.5], center=0 ))[1:*]

         endfor
         endfor
      end

      'tropopause': begin

         TracerN = 3
         DiagN = 'TR-PAUSE'

      end
      'tp-l': begin
         TracerN = 1
         DiagN = 'TR-PAUSE'
      end
      'tp-h': begin
         TracerN = 2
         DiagN = 'TR-PAUSE'
      end
      'tp-p': begin
         TracerN = 3
         DiagN = 'TR-PAUSE'
      end


      'uwnd': begin
         TracerN = 1
         DiagN = 'DAO-3D-$'
      end
      'vwnd': begin
         TracerN = 2
         DiagN = 'DAO-3D-$'
      end
      'tmpu': begin
         TracerN = 3
         DiagN = 'DAO-3D-$'
      end
      'temperature': begin
         TracerN = 3
         DiagN = 'DAO-3D-$'
      end
      'sphu': begin
         TracerN = 4
         DiagN = 'DAO-3D-$'
      end
      'kzz': begin
         TracerN = 5
         DiagN = 'DAO-3D-$'
      end
      'moistq': begin
         TracerN = 6
         DiagN = 'DAO-3D-$'
      end

      'bxhght': begin
         TracerN = 1
         DiagN = 'BXHGHT-$'
      end
      'airmas': begin
         TracerN = 2
         DiagN = 'BXHGHT-$'
      end
      'avgw': begin
         TracerN = 3
         DiagN = 'BXHGHT-$'
      end
      'n(air)': begin
         TracerN = 4
         DiagN = 'BXHGHT-$'
      end


      'hflux':begin
         TracerN = 1
         DiagN = 'DAO-FLDS'
      end
      'radswg':begin
         TracerN = 2
         DiagN = 'DAO-FLDS'
      end
      'preacc':begin
         TracerN = 3
         DiagN = 'DAO-FLDS'
      end
      'precon':begin
         TracerN = 4
         DiagN = 'DAO-FLDS'
      end
      'ts':begin
         TracerN = 5
         DiagN = 'DAO-FLDS'
      end
      'radswt':begin
         TracerN = 6
         DiagN = 'DAO-FLDS'
      end
      'ustar':begin
         TracerN = 7
         DiagN = 'DAO-FLDS'
      end
      'z0':begin
         TracerN = 8
         DiagN = 'DAO-FLDS'
      end
      'pbl':begin
         TracerN = 9
         DiagN = 'DAO-FLDS'
      end
      'cldfrc':begin
         TracerN = 10
         DiagN = 'DAO-FLDS'
      end
      'u10m':begin
         TracerN = 11
         DiagN = 'DAO-FLDS'
      end
      'v10m':begin
         TracerN = 12
         DiagN = 'DAO-FLDS'
      end
      'ps-pbl':begin
         TracerN = 13
         DiagN = 'DAO-FLDS'
      end
      'albc':begin
         TracerN = 14
         DiagN = 'DAO-FLDS'
      end
      'phis':begin
         TracerN = 15
         DiagN = 'DAO-FLDS'
      end
      'cldtop':begin
         TracerN = 16
         DiagN = 'DAO-FLDS'
      end
      'tropp':begin
         TracerN = 17
         DiagN = 'DAO-FLDS'
      end
      'slp':begin
         TracerN = 18
         DiagN = 'DAO-FLDS'
      end
      'tskin':begin
         TracerN = 19
         DiagN = 'DAO-FLDS'
      end
      'pardf':begin
         TracerN = 20
         DiagN = 'DAO-FLDS'
      end
      'pardr':begin
         TracerN = 21
         DiagN = 'DAO-FLDS'
      end
      'gwet':begin
         TracerN = 22
         DiagN = 'DAO-FLDS'
      end

   endcase



   if strlowcase( Variable ) ne 'pressure' then begin
      ;Extract pressure from data file
      success = ctm_get_datablock( State, $
                                   DiagN, $
                                   tracer=TracerN, $
                                   filename=gc_file,               $
                                   tau0=gc_tau0, $
                                   xmid=xmid, ymid=ymid, zmid=zmid,  $
                                   gridinfo=GC_grid, $
                                   modelinfo=GC_model,  $
                                   _Extra=_Extra )

      if ( ~success ) then message, 'Unable to get '+Variable
   endif


   return, State
end
