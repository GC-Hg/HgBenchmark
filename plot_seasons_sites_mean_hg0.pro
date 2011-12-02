pro plot_seasons_sites_mean_hg0, FileName=FileName, $
                                 Reference=Reference, $
                                 psFileName=psFileName, $
                                 PS=PS, $
                                 DataRange=Range, $
                                 PageTitle=PageTitle, $
                                 PPQ=PPQ, $
                                 _Extra=_Extra

   ;------------------------------------------------;
   ; jaf 5/17/11 copied from plot_seasons_sites_mean;
   ; to plot Hg0 in mercury model benchmarking      ;
   ;------------------------------------------------;

   ;=======================================
   ; Setup
   ;=======================================
   
   DataDir = !BENCHMARK+'/data/' ;eds 5/11/11

   Species = 'Hg0'
   DiagN = 'IJ-AVG-$'

   if Keyword_set( psFileName ) then $
      PS = 1L $
   else $
      PS = 0L
 
   if ( not Keyword_set( psFileName ) ) then $
         psFileName = 'seasons.'+species+'.ps' 
 
   LOG = Keyword_set( Log )
 
   if (not keyword_set( PPQ )) then $
      ppq = 0L

   ;eds
   if ( not Keyword_set( Range ) ) then begin
      if (keyword_set(ppq)) then begin
         nh_range = [100, 250] ;northern hemisphere
         sh_range = [50, 200]  ;southern hemisphere
         unit = 'ppqv'
         ytitle = 'Hg0 [ppqv]'
      endif else begin
         nh_range = [1.0, 2.5]
         sh_range = [0.5, 2.0]
         unit = 'ng/m!u3!n'
         ytitle = 'Hg0 [ng/m!u3!n]'
      endelse
   endif else begin
         nh_range = range
         sh_range = range
         unit = ''
         ytitle = 'Hg0'
   endelse

   ; File with monthly Hg0 data
   Hg0file = DataDir + 'Hg0SiteMonthly.csv'

   ; Conversion factor ng/m3 -> ppqv @273K, 1013hPa 
   ngm3_ppqv = 112
   pptv_ngm3 = 8.93D0 ;eds 5/11/11

   ;=======================================
   ; Read Hg0 data
   ;=======================================
   
   data = get_hg_obs( file=Hg0file )

   Hg0site = [data.siteID]
   Hg0lat = [data.lat]
   Hg0lon = [data.lon]
   Hg0dat = [data.HG]
   Hg0std = [data.std]

   ; Convert ng/m3 -> ppqv
   if (keyword_set(ppq)) then begin ;eds
      Hg0dat = Hg0dat * ngm3_ppqv
      Hg0std = Hg0std * ngm3_ppqv
   endif

   ; Sort by latitude, N->S
   ii = reverse( sort( Hg0lat ) )
   Hg0site=Hg0site[ii]
   Hg0lon=Hg0lon[ii]
   Hg0lat=Hg0lat[ii]
   Hg0dat=Hg0dat[ii,*]
   Hg0std=Hg0std[ii,*]

   SitesToAverage = ['PensacolaOLF','NewcombNY','Kuujjuarapik','CheekaPeak',$
                     'PacMonadnock','AthensOH']

   ;=======================================
   ; Read BPCH data
   ;=======================================

   nSites = n_elements( Hg0site )
   nFiles = n_elements( FileName )

   Hg0mod = fltarr( nsites, 12 )
   GRIDindex  = lonarr( nsites, 12 )

   for F=0L, nFiles-1L do begin

      ctm_get_data, Hg0DataInfo, DiagN, FileName=FileName[F], Tracer=1L

      ; Number of time steps in the DataInfo structures (should all be same)
      n_times = n_elements( Hg0DataInfo )

      ; check whether there are 12 times in the file (assume they are months)
      if ( n_times NE 12 ) then $
         message, 'File does not contain 12 monthly means'

      ; Get grid info
      getmodelandgridinfo, Hg0DataInfo[0], ModelInfo, GridInfo

      for S=0L, nSites-1L do begin
 
         ; Get index of site, Fortran notation
         ctm_index, ModelInfo, I, J, $
                    center=[Hg0lat[S], Hg0lon[S]], /non_interactive
         
         ; Convert Fortran index convention to IDL
         I = I-1L
         J = J-1L

         GRIDindex[S] = I * 10000L + J

         ; Zeppelin is at altitude...
         if (Hg0Site[S] eq 'Zeppelin') then lev=3 else lev=0

         for iMonth=0L, n_times-1L do begin

            ; Hg0 concentration in the model, pptv
            HG0mod[S, iMonth] = HG0mod[S, iMonth] + $
                             (*( Hg0Datainfo[iMonth].Data))[I, J, lev]


         endfor
      endfor
   endfor

   HG0mod = HG0mod / nFiles

   if (keyword_set(ppq)) then begin
      ; Convert pptv -> ppqv
      Hg0mod = Hg0mod * 1e3
   endif else begin
      ; Convert pptv -> ng/m3 ;eds
      Hg0mod = Hg0mod * pptv_ngm3
   endelse

   ; clear memory
   ctm_cleanup

   ;DO THE SAME FOR REFERENCE MODEL RUN
   ;eds 5/11/11

   if (keyword_set(reference)) then begin

   Hg0ref = fltarr( nsites, 12 )

   nFiles = n_elements( Reference )

   for F=0L, nFiles-1L do begin

      ctm_get_data, Hg0DataInfo, DiagN, FileName=Reference[F], Tracer=1L

      ; Number of time steps in the DataInfo structures (should all be same)
      n_times = n_elements( Hg0DataInfo )

      ; check whether there are 12 times in the file (assume they are months)
      if ( n_times NE 12 ) then $
         message, 'File does not contain 12 monthly means'

      ; Get grid info
      getmodelandgridinfo, Hg0DataInfo[0], ModelInfo, GridInfo

      for S=0L, nSites-1L do begin

         ; Get index of site, Fortran notation
         ctm_index, ModelInfo, I, J, $
                    center=[Hg0lat[S], Hg0lon[S]], /non_interactive

         ; Convert Fortran index convention to IDL
         I = I-1L
         J = J-1L

         GRIDindex[S] = I * 10000L + J

         ; Zeppelin is at altitude...
         if (Hg0Site[S] eq 'Zeppelin') then lev=3 else lev=0

         for iMonth=0L, n_times-1L do begin
            ; Hg0 concentration in the model, pptv
            HG0ref[S, iMonth] = HG0ref[S, iMonth] + $
                             (*( Hg0Datainfo[iMonth].Data))[I, J, lev]


         endfor
      endfor
   endfor

   HG0ref = HG0ref / nFiles

   ; Convert pptv -> ng/m3
   Hg0ref = Hg0ref * pptv_ngm3

   endif

   ; clear memory
   ctm_cleanup


   ;=======================================
   ; Average sites
   ;=======================================

   ; Number of sites to average
   nAvg = n_elements( SitesToAverage )

   INDEXmidlat = lonarr( nAvg )
   GRIDindex_midlat = lonarr( nAvg )
   Hg0mod_midlat = fltarr( nAvg, 12 )
   Hg0ref_midlat = fltarr( nAvg, 12 ) ;eds 5/11/11

   for S=0L, nAvg-1L do begin
     
      SS = where( Hg0site eq SitesToAverage[S], ct )
         
      if (ct eq 0) then begin
         print,  'Could not find site ', SitesToAverage[S]
         continue
      endif

      GRIDindex_midlat[S] = GRIDindex[SS]
      INDEXmidlat[S] = SS
      Hg0mod_midlat[S, *] = Hg0mod[SS, *]
      if (keyword_set(reference)) then begin
         Hg0ref_midlat[S, *] = Hg0ref[SS, *] ;eds 5/11/11
      endif

   endfor

   ii = sort( GRIDindex_midlat )


   Hg0dat_midlat = Hg0dat[INDEXmidlat, *]
   Hg0mod_midlat = Hg0mod_midlat[ uniq( GRIDindex_midlat, $
                                        sort( GRIDindex_midlat) ), * ]
   if (keyword_set(reference)) then begin
   Hg0ref_midlat = Hg0ref_midlat[ uniq( GRIDindex_midlat, $
                                        sort( GRIDindex_midlat) ), * ] ;eds 5/11/11
   endif


   ;=======================================
   ; Plotting
   ;=======================================

   If Keyword_Set( PS ) then $
      ps_setup, /open, file=psFileName, xsize=6, ysize=4, /landscape

   multipanel, nplots=4,margin=0.08, pos=p
 
   ; First letter of each month
   monthstr=['J','F','M','A','M','J','J','A','S','O','N','D']   



   ;------------------------
   ; Northern mid-latitudes
   ;------------------------
   plot, mean2( Hg0dat_midlat, 1 ), /color, /nodata, $
            ytitle=ytitle,     $
            xrange=[-0.5,11.5], yrange=nh_range, /xstyle, /ystyle, $ 
            xticks=11, xtickv=indgen(12), xtickname=monthstr, $
            yminor=2, ticklen=0.01, thick=3.0,  $
            title='Mid-latitudes', pos=p

   oplot, mean2(Hg0dat_midlat, 1), color=!myct.gray50, thick=3
   
;   for S=0L, nAvg-1L do $
;      oplot, TGMdat_midlat[S, * ], color=S + 1

   errplot, mean2( Hg0dat_midlat, 1) - stddev2( Hg0dat_midlat, 1), $
            mean2( Hg0dat_midlat, 1) + stddev2( Hg0dat_midlat, 1), $
            color=!myct.gray50, thick=3

   oplot, mean2( Hg0mod_midlat, 1 ), color=3, thick=3
   errplot, mean2( Hg0mod_midlat, 1) - stddev2(Hg0mod_midlat, 1), $
            mean2( Hg0mod_midlat, 1) + stddev2(Hg0mod_midlat, 1), $
            color=3, thick=3

   ;eds 5/11/11
   if (keyword_set(reference)) then begin
   oplot, mean2( Hg0ref_midlat, 1 ), color=6, thick=3
   errplot, mean2( Hg0ref_midlat, 1) - stddev2(Hg0ref_midlat, 1), $
            mean2( Hg0ref_midlat, 1) + stddev2(Hg0ref_midlat, 1), $
            color=6, thick=3
   endif

   ;eds 5/11/11
   if (keyword_set(ppq)) then begin
      xyouts, 6, 240, 'Observations', color=!myct.gray50, charsize=1
      xyouts, 6, 230, 'New Model Version', color=3, charsize=1
      if (keyword_set(reference)) then begin
         xyouts, 6, 220, 'Old Model Version', color=6, charsize=1
      endif
   endif else begin
      xyouts, 6, 2.4, 'Observations', color=!myct.gray50, charsize=1
      xyouts, 6, 2.3, 'New Model Version', color=3, charsize=1
      if (keyword_set(reference)) then begin
         xyouts, 6, 2.2, 'Old Model Version', color=6, charsize=1
      endif
   endelse

   ;------------------------
   ; Arctic
   ;------------------------

   sites = ['Alert', 'Zeppelin', 'Amderma']
   
   multipanel, /advance, pos=p
   
   plot, Hg0dat[0, *], /nodata, /color, thick=3, title='Arctic', $
         xrange=[-0.5,11.5], xticks=11, xtickv=indgen(12), xtickname=monthstr, $
         yrange=nh_range, yminor=2, /xstyle, /ystyle, ytitle=ytitle, $ ;eds 5/11/11
         ticklen=0.01, pos=p

   Hg0dat_arctic = fltarr( 3, 12 )
   Hg0mod_arctic = fltarr( 3, 12 )
   Hg0ref_arctic = fltarr( 3, 12 ) ;eds 5/11/11
   
   for S=0L, n_elements( sites ) -1L do begin
      i = where( Hg0site eq sites[S] )
;      oplot, TGMdat[i[0], *], /color
;      oplot, TGMmod[i[0], *], color=2
      Hg0mod_arctic[S, *] = Hg0mod[i[0], *]
      Hg0dat_arctic[S, *] = Hg0dat[i[0], *]
      if (keyword_set(reference)) then begin    ;eds 5/11/11
         Hg0ref_arctic[S, *] = Hg0ref[i[0], *]
      endif
   endfor

   oplot, mean2( Hg0mod_arctic, 1), color=3, thick=3
   oplot, mean2( Hg0dat_arctic, 1), color=!myct.gray50, thick=3
   errplot, mean2( Hg0dat_arctic, 1) - stddev2( Hg0dat_arctic, 1), $
            mean2( Hg0dat_arctic, 1) + stddev2( Hg0dat_arctic, 1), $
            color=!myct.gray50, thick=3

   oplot, mean2( Hg0mod_arctic, 1 ), color=3, thick=3
   errplot, mean2( Hg0mod_arctic, 1) - stddev2(Hg0mod_arctic, 1), $
            mean2( Hg0mod_arctic, 1) + stddev2(Hg0mod_arctic, 1), $
            color=3, thick=3

   ;eds 5/11/11
   if (keyword_set(reference)) then begin
   oplot, mean2( Hg0ref_arctic, 1 ), color=6, thick=3
   errplot, mean2( Hg0ref_arctic, 1) - stddev2(Hg0ref_arctic, 1), $
            mean2( Hg0ref_arctic, 1) + stddev2(Hg0ref_arctic, 1), $
            color=6, thick=3
   endif

   multipanel, /off

   If Keyword_Set( PS ) then $
      ps_setup, /close

end
