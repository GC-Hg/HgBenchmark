pro plot_seasons_sites, FileName=FileName, $
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
   
   DataDir = !BENCHMARK+'/data/' ;eds 5/11/11

   Species = 'TGM'
   DiagN = 'IJ-AVG-$'

   if Keyword_set( psFileName ) then $
      PS = 1L $
   else $
      PS = 0L
 
   if ( not Keyword_set( psFileName ) ) then $
         psFileName = 'seasons.'+species+'.ps' 
 
   LOG = Keyword_set( Log )
 
   if ( not Keyword_set( Range ) ) then $
      Range = [4, 400]
      unit = 'ppqv'

   ; File with monthly TGM data
   TGMfile = DataDir + 'TGMSiteMonthly.csv' ;eds 5/11/11
   Hg0file = DataDir + 'Hg0SiteMonthly.csv'

   ; Conversion factor ng/m3 -> ppqv @273K, 1013hPa 
   ngm3_ppqv = 112
   pptv_ngm3 = 8.93D0 ;eds 5/11/11

   ;=======================================
   ; Read TGM data
   ;=======================================
   
   data = get_hg_obs( file=TGMfile )

   dataHg0 = get_hg_obs( file= Hg0File ) ;eds 5/11/11

   TGMsite = [data.siteID, dataHg0.siteID]
   TGMlat = [data.lat, dataHg0.lat]
   TGMlon = [data.lon, dataHg0.lon]
   TGMdat = [data.HG,  dataHg0.HG]
   TGMstd = [data.std, dataHg0.std]

   ; Array that is 1 for Hg0 observations and 0 for TGM
   IDHg0 = fltarr( n_elements( TGMsite ) )
   IDHg0[n_elements(data.siteID): *] = 1

   ; Convert ng/m3 -> ppqv ;eds 5/11/11
   if (keyword_set(ppq)) then begin
      TGMdat = TGMdat * ngm3_ppqv
      TGMstd = TGMstd * ngm3_ppqv
   endif

   ; Sort by latitude, N->S
   ii = reverse( sort( TGMlat ) )
   TGMsite=TGMsite[ii]
   TGMlon=TGMlon[ii]
   TGMlat=TGMlat[ii]
   TGMdat=TGMdat[ii,*]
   TGMstd=TGMstd[ii,*]
   IDHg0=IDHg0[ii]

   ;=======================================
   ; Read BPCH data
   ;=======================================

   nFiles = n_elements( FileName )

   ; Tracer
   ctm_get_data, Hg0DataInfo, DiagN, FileName=FileName[0], Tracer=1L


   ; Number of time steps in the DataInfo structures (should all be same)
   n_times = n_elements( Hg0DataInfo )

   ; check whether there are 12 times in the file (assume they are months)
   if ( n_times NE 12 ) then message, 'File does not contain 12 monthly means'

   ; Get grid info
   getmodelandgridinfo, Hg0DataInfo[0], ModelInfo, GridInfo

   ; Number of sites
   nsites = n_elements( TGMsite )

   ; Initialize array for concentration at each site
   TGMmod = fltarr( nsites, 12 )
   Hg0mod = fltarr( nsites, 12 )
   TGMref = fltarr( nsites, 12 ) ;eds 5/11/11
   Hg0ref = fltarr( nsites, 12 )


   for F=0L, nFiles-1L do begin

   ; Tracer
   ctm_get_data, Hg0DataInfo, DiagN, FileName=FileName[F], Tracer=1L
   ctm_get_data, Hg2DataInfo, DiagN, FileName=FileName[F], Tracer=2L


   for S=0L, nsites-1L do begin
      
      ; Get index of site, Fortran notation
      ctm_index, ModelInfo, I, J, $
                 center=[TGMlat[S], TGMlon[S]], /non_interactive

      ; Convert Fortran index convention to IDL
      I = I-1L
      J = J-1L

      ; Zeppelin is at altitude...
      ; Also several other sites ;sjs, 4/22/15
      case TGMSite[S] of
         'Zeppelin' : lev=3
         'Andoya' : lev=2
         'MtBachelor' : lev=16
         'Waliguan' : lev=1
         'NamCoLake' : lev=18
         'MtLulin' : lev=16
         'MaunaLoa' : lev=18
         else : lev=0
      endcase

      for iMonth=0L, n_times-1L do begin

         ; TGM concentration in the model, pptv
         ; hmh 7/28/15 removed Fg to properly use gas-phase Hg2 tracer as of v9-02
         TGMmod[S, iMonth] = TGMmod[S, iMonth] + $
                  ( (*( Hg0Datainfo[iMonth].Data))[I, J, lev] + $
                    (*( Hg2Datainfo[iMonth].Data))[I, J, lev] )
         ; Hg0 concentration in the model, pptv
         HG0mod[S, iMonth] = HG0mod[S, iMonth] + $
                             (*( Hg0Datainfo[iMonth].Data))[I, J, lev]


      endfor
   endfor
   endfor

   TGMmod = TGMmod / nFiles
   HG0mod = HG0mod / nFiles

   if (keyword_set(ppq)) then begin
      ; Convert ppt -> ppq
      TGMmod = TGMmod * 1e3
      Hg0mod = Hg0mod * 1e3
   endif else begin
      ; Convert ppt -> ng/m3 ;eds 5/11/11
      TGMmod = TGMmod * pptv_ngm3
      Hg0mod = Hg0mod * pptv_ngm3
   endelse

   ;DO THE SAME FOR REFERENCE MODEL RUN
   ;eds 5/11/11

   if (keyword_set(reference)) then begin

   nFiles = n_elements( reference )

   ; Tracer
   ctm_get_data, Hg0DataInfo, DiagN, FileName=Reference[0], Tracer=1L

   ; Number of time steps in the DataInfo structures (should all be same)
   n_times = n_elements( Hg0DataInfo )

   ; check whether there are 12 times in the file (assume they are months)
   if ( n_times NE 12 ) then message, 'File does not contain 12 monthly means'

   ; Get grid info
   getmodelandgridinfo, Hg0DataInfo[0], ModelInfo, GridInfo

   for F=0L, nFiles-1L do begin

   ; Tracer
   ctm_get_data, Hg0DataInfo, DiagN, FileName=Reference[F], Tracer=1L
   ctm_get_data, Hg2DataInfo, DiagN, FileName=Reference[F], Tracer=2L


   for S=0L, nsites-1L do begin

      ; Get index of site, Fortran notation
      ctm_index, ModelInfo, I, J, $
                 center=[TGMlat[S], TGMlon[S]], /non_interactive

      ; Convert Fortran index convention to IDL
      I = I-1L
      J = J-1L

      ; Zeppelin is at altitude...
      ; Also several other sites ;sjs, 4/22/15
      case TGMSite[S] of
         'Zeppelin' : lev=3
         'Andoya' : lev=2
         'MtBachelor' : lev=16
         'Waliguan' : lev=1
         'NamCoLake' : lev=18
         'MtLulin' : lev=16
         'MaunaLoa' : lev=18
         else : lev=0
      endcase

      for iMonth=0L, n_times-1L do begin


         ; TGM concentration in the model, pptv
         ; hmh 7/28/15 removed Fg to properly use gas-phase Hg2 tracer as of v9-02
         TGMref[S, iMonth] = TGMref[S, iMonth] + $
                  ( (*( Hg0Datainfo[iMonth].Data))[I, J, lev] + $
                    (*( Hg2Datainfo[iMonth].Data))[I, J, lev] )
         ; Hg0 concentration in the model, pptv
         HG0ref[S, iMonth] = HG0ref[S, iMonth] + $
                             (*( Hg0Datainfo[iMonth].Data))[I, J, lev]

      endfor
   endfor
   endfor

   TGMref = TGMref / nFiles
   HG0ref = HG0ref / nFiles

   if (keyword_set(ppq)) then begin
      ; Convert pptv -> ppqv
      TGMref = TGMref * 1d3
      Hg0ref = Hg0ref * 1d3
   endif else begin
      ; Convert pptv -> ng/m3
      TGMref = TGMref * pptv_ngm3
      Hg0ref = Hg0ref * pptv_ngm3
   endelse

   endif

   ; clear memory
   ctm_cleanup


   ;=======================================
   ; Plotting
   ;=======================================

   If Keyword_Set( PS ) then $
      ps_setup, /open, file=psFileName, xsize=10, ysize=7, /landscape

   multipanel, nplots=nsites, margin=0.02, omargin=[0.03,0.0,0.0,0.0], $
               pos=p
 
   ; First letter of each month
   monthstr=['J','F','M','A','M','J','J','A','S','O','N','D']   


   for S=0L, nsites-1L do begin

      p = getpos( 0.63,  pos=p, margin=0 )

      ; Use higher range for sites with high mean observation or model
      if (keyword_set(ppq)) then begin
         yrange = [100, 300]
         ytitle1 = 'Hg(0) [ppqv]'
         ytitle2 = 'TGM [ppqv]'
      endif else begin
         yrange = [1.0, 2.5] ;eds 5/11/11
         ytitle1 = 'Hg(0) [ng/m!u3!n]'
         ytitle2 = 'TGM [ng/m!u3!n]'
      endelse

      ; Larger yrange for some sites ;sjs, 4/23/15
      if (TGMsite[S] eq 'ShangriLa') then yrange = [1.0, 3.2]
      
      ; Use smaller range for Southern Hemisphere
      if (keyword_set(ppq)) then begin
         yrange = (TGMlat[S] lt 0) ? [0, 200] : yrange 
      endif else begin
         yrange = (TGMlat[S] lt 0) ? [0.5, 2.0] : yrange ;eds 5/11/11
      endelse

      ; Plot Observations
     ;eds 5/11/11 cleaned up to make easier to see Hg(0) or TGM
      if IDHg0[S] eq 1 then begin

      plot, TGMdat[S,*], /color, /nodata, $
            ytitle=ytitle1, $ 
            xrange=[-0.5,11.5], yrange=yrange, /xstyle, /ystyle, $ 
      xticks=11, xtickv=indgen(12), xtickname=monthstr, $
      yminor=2, ticklen=0.01, thick=3.0, charsize=1.2, $
            title=TGMsite[S], pos=p
      oplot, TGMdat[S,*], color=!myct.gray67, thick=3
      errplot, TGMdat[S,*]-TGMstd[S,*], TGMdat[S,*]+TGMstd[S,*], $
               color=!myct.gray67, thick=3
      oplot, Hg0mod[S,*], color=3, thick=3
      oplot, Hg0ref[S,*], color=6, thick=3

      endif else begin

      plot, TGMdat[S,*], /color, /nodata, $
            ytitle=ytitle2, $
            xrange=[-0.5,11.5], yrange=yrange, /xstyle, /ystyle, $
      xticks=11, xtickv=indgen(12), xtickname=monthstr, $
      yminor=2, ticklen=0.01, thick=3.0, charsize=1.2, $
            title=TGMsite[S], pos=p
      oplot, TGMdat[S,*], color=!myct.black, thick=3
      errplot, TGMdat[S,*]-TGMstd[S,*], TGMdat[S,*]+TGMstd[S,*], $
               color=!myct.black, thick=3
      ; These should be TGM, not Hg0 (jaf, 7/7/11)
      ;oplot, Hg0mod[S,*], color=2, thick=3
      ;oplot, Hg0ref[S,*], color=4, thick=3
      oplot, TGMmod[S,*], color=2, thick=3
      oplot, TGMref[S,*], color=4, thick=3

      endelse

      if ( S ne nsites-1L ) then $
         multipanel, /advance, pos=p

   endfor
   
   multipanel, /off

   ; Make legend ;modified eds 5/11/11
   standard_label = ['Obs TGM', 'Obs Hg(0)', 'New Model TGM', 'New Model Hg(0)']
   ref_label = ['Obs TGM', 'Obs Hg(0)', 'New Model TGM', 'New Model Hg(0)', $
      'Reference Model TGM', 'Reference Model Hg(0)']

   if (keyword_set(reference)) then begin
   legend, label=ref_label, $
      lcolor=[1, !myct.gray67, 2, 3, 4, 6], line=fltarr(6), thick=3, $
      halign=0.90, valign=-0.10, charsize=0.75
   endif else begin
   legend, label=standard_label, $                                                  
      lcolor=[1, !myct.gray67, 2, 3], line=fltarr(4), thick=3, $
      halign=0.90, valign=-0.10, charsize=0.75
   endelse

   If Keyword_Set( PS ) then $
      ps_setup, /close

end
