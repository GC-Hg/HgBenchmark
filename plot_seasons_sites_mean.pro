pro plot_seasons_sites_mean, FileName=FileName, $
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
 
   if (not keyword_set( PPQ )) then $
      ppq = 0L

   ;eds
   if ( not Keyword_set( Range ) ) then begin
      if (keyword_set(ppq)) then begin
         nh_range = [100, 250] ;northern hemisphere
         sh_range = [50, 200]  ;southern hemisphere
         unit = 'ppqv'
         ytitle = 'TGM [ppqv]'
      endif else begin
         nh_range = [1.0, 2.5]
         sh_range = [0.5, 2.0]
         unit = 'ng/m!u3!n'
         ytitle = 'TGM [ng/m!u3!n]'
      endelse
   endif else begin
         nh_range = range
         sh_range = range
         unit = ''
         ytitle = 'TGM'
   endelse

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

   dataHg0 = get_hg_obs( file=Hg0file ) ;eds 5/11/11

   TGMsite = [data.siteID, dataHg0.siteID]
   TGMlat = [data.lat, dataHg0.lat]
   TGMlon = [data.lon, dataHg0.lon]
   TGMdat = [data.HG,  dataHg0.HG]
   TGMstd = [data.std, dataHg0.std]

   ; Array that is 1 for Hg0 observations and 0 for TGM
   IDHg0 = fltarr( n_elements( TGMsite ) )
   IDHg0[n_elements(data.siteID): *] = 1

   ; Convert ng/m3 -> ppqv
   if (keyword_set(ppq)) then begin ;eds
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

   SitesToAverage = ['Andoya', 'Pallas', 'Birkenes', 'Lista', 'Roervik/Raaoe', $
                     'Zingst', 'MaceHead', 'Langenbruegge', 'CheekaPeak', $
                     'St.Anicet', 'St.Andrews', 'Kejimkujik', 'AthensOH', $
                     'PensacolaOLF', 'ReifelIsland' ]

   ;=======================================
   ; Read BPCH data
   ;=======================================

   nSites = n_elements( TGMsite )
   nFiles = n_elements( FileName )

   TGMmod = fltarr( nsites, 12 )
   Hg0mod = fltarr( nsites, 12 )
   GRIDindex  = lonarr( nsites, 12 )

   for F=0L, nFiles-1L do begin

      ctm_get_data, Hg0DataInfo, DiagN, FileName=FileName[F], Tracer=1L
      ctm_get_data, Hg2DataInfo, DiagN, FileName=FileName[F], Tracer=2L

      ; Gas fraction, if it exists
      ctm_get_data, FgDataInfo, 'PL-HG2-$', FileName=FileName[F], Tracer=9L

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
                    center=[TGMlat[S], TGMlon[S]], /non_interactive
         
         ; Convert Fortran index convention to IDL
         I = I-1L
         J = J-1L

         GRIDindex[S] = I * 10000L + J

         ; Zeppelin is at altitude...
         if (TGMSite[S] eq 'Zeppelin') then lev=3 else lev=0

         for iMonth=0L, n_times-1L do begin

            ; Read or set HgII gas fraction
            if ( n_elements(FgDataInfo) gt 0 ) then $
               Fg=(*( FgDataInfo[iMonth].Data))[I,J,lev] else $
               Fg=0.5

            ; TGM concentration in the model, pptv
            TGMmod[S, iMonth] = TGMmod[S, iMonth] + $
                     ( (*( Hg0Datainfo[iMonth].Data))[I, J, lev] + $
                       Fg* (*( Hg2Datainfo[iMonth].Data))[I, J, lev] )
            ; Hg0 concentration in the model, pptv
            HG0mod[S, iMonth] = HG0mod[S, iMonth] + $
                             (*( Hg0Datainfo[iMonth].Data))[I, J, lev]


         endfor
      endfor
   endfor

   TGMmod = TGMmod / nFiles
   HG0mod = HG0mod / nFiles

   if (keyword_set(ppq)) then begin
      ; Convert pptv -> ppqv
      TGMmod = TGMmod * 1e3
      Hg0mod = Hg0mod * 1e3
   endif else begin
      ; Convert pptv -> ng/m3 ;eds
      TGMmod = TGMmod * pptv_ngm3
      Hg0mod = Hg0mod * pptv_ngm3
   endelse

   ; clear memory
   ctm_cleanup

   ;DO THE SAME FOR REFERENCE MODEL RUN
   ;eds 5/11/11

   if (keyword_set(reference)) then begin

   TGMref = fltarr( nsites, 12 )
   Hg0ref = fltarr( nsites, 12 )

   nFiles = n_elements( Reference )

   for F=0L, nFiles-1L do begin

      ctm_get_data, Hg0DataInfo, DiagN, FileName=Reference[F], Tracer=1L
      ctm_get_data, Hg2DataInfo, DiagN, FileName=Reference[F], Tracer=2L

      ; Gas fraction, if it exists
      ctm_get_data, FgDataInfo, 'PL-HG2-$', FileName=FileName[F], Tracer=9L

      ; Number of time steps in the DataInfo structures (should all be same)
      n_times = n_elements( Hg0DataInfo )

      ; check whether there are 12 times in the file (assume they are months)
      if ( n_times NE 12 ) then $
         message, 'File does not contain 12 monthly means'

      ; Get grid info
      getmodelandgridinfo, Hg0DataInfo[0], ModelInfo, GridInfo

      for S=0L, nSites-1L do begin

         ; Zeppelin is at altitude...
         if (TGMSite[S] eq 'Zeppelin') then lev=3 else lev=0

         ; Get index of site, Fortran notation
         ctm_index, ModelInfo, I, J, $
                    center=[TGMlat[S], TGMlon[S]], /non_interactive

         ; Convert Fortran index convention to IDL
         I = I-1L
         J = J-1L

         GRIDindex[S] = I * 10000L + J

         for iMonth=0L, n_times-1L do begin

            ; Read or set HgII gas fraction
            if ( n_elements(FgDataInfo) gt 0 ) then $
               Fg=(*( FgDataInfo[iMonth].Data))[I,J,lev] else $
               Fg=0.5

            ; TGM concentration in the model, pptv
            TGMref[S, iMonth] = TGMref[S, iMonth] + $
                     ( (*( Hg0Datainfo[iMonth].Data))[I, J, lev] + $
                       Fg*(*( Hg2Datainfo[iMonth].Data))[I, J, lev] )
            ; Hg0 concentration in the model, pptv
            HG0ref[S, iMonth] = HG0ref[S, iMonth] + $
                             (*( Hg0Datainfo[iMonth].Data))[I, J, lev]


         endfor
      endfor
   endfor

   TGMref = TGMref / nFiles
   HG0ref = HG0ref / nFiles

   ; Convert pptv -> ng/m3
   TGMref = TGMref * pptv_ngm3
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
   TGMmod_midlat = fltarr( nAvg, 12 )
   TGMref_midlat = fltarr( nAvg, 12 ) ;eds 5/11/11

   for S=0L, nAvg-1L do begin
     
      SS = where( TGMsite eq SitesToAverage[S], ct )
         
      if (ct eq 0) then begin
         print,  'Could not find site ', SitesToAverage[S]
         continue
      endif

      GRIDindex_midlat[S] = GRIDindex[SS]
      INDEXmidlat[S] = SS
      TGMmod_midlat[S, *] = TGMmod[SS, *]
      if (keyword_set(reference)) then begin
         TGMref_midlat[S, *] = TGMref[SS, *] ;eds 5/11/11
      endif

   endfor

   ii = sort( GRIDindex_midlat )


   TGMdat_midlat = TGMdat[INDEXmidlat, *]
   TGMmod_midlat = TGMmod_midlat[ uniq( GRIDindex_midlat, $
                                        sort( GRIDindex_midlat) ), * ]
   if (keyword_set(reference)) then begin
   TGMref_midlat = TGMref_midlat[ uniq( GRIDindex_midlat, $
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
   plot, mean2( TGMdat_midlat, 1 ), /color, /nodata, $
            ytitle=ytitle,     $
            xrange=[-0.5,11.5], yrange=nh_range, /xstyle, /ystyle, $ 
            xticks=11, xtickv=indgen(12), xtickname=monthstr, $
            yminor=2, ticklen=0.01, thick=3.0,  $
            title='Mid-latitudes', pos=p

   oplot, mean2(TGMdat_midlat, 1), /color, thick=3
   
;   for S=0L, nAvg-1L do $
;      oplot, TGMdat_midlat[S, * ], color=S + 1

   errplot, mean2( TGMdat_midlat, 1) - stddev2( TGMdat_midlat, 1), $
            mean2( TGMdat_midlat, 1) + stddev2( TGMdat_midlat, 1), $
            /color, thick=3

   oplot, mean2( TGMmod_midlat, 1 ), color=2, thick=3
   errplot, mean2( TGMmod_midlat, 1) - stddev2(TGMmod_midlat, 1), $
            mean2( TGMmod_midlat, 1) + stddev2(TGMmod_midlat, 1), $
            color=2, thick=3

   ;eds 5/11/11
   if (keyword_set(reference)) then begin
   oplot, mean2( TGMref_midlat, 1 ), color=4, thick=3
   errplot, mean2( TGMref_midlat, 1) - stddev2(TGMref_midlat, 1), $
            mean2( TGMref_midlat, 1) + stddev2(TGMref_midlat, 1), $
            color=4, thick=3
   endif

   ;eds 5/11/11
   if (keyword_set(ppq)) then begin
      xyouts, 6, 240, 'Observations', color=1, charsize=1
      xyouts, 6, 230, 'New Model Version', color=2, charsize=1
      if (keyword_set(reference)) then begin
         xyouts, 6, 220, 'Old Model Version', color=4, charsize=1
      endif
   endif else begin
      xyouts, 6, 2.4, 'Observations', color=1, charsize=1
      xyouts, 6, 2.3, 'New Model Version', color=2, charsize=1
      if (keyword_set(reference)) then begin
         xyouts, 6, 2.2, 'Old Model Version', color=4, charsize=1
      endif
   endelse

   ;------------------------
   ; Arctic
   ;------------------------

   sites = ['Alert', 'Zeppelin', 'Amderma']
   
   multipanel, /advance, pos=p
   
   plot, TGMdat[0, *], /nodata, /color, thick=3, title='Arctic', $
         xrange=[-0.5,11.5], xticks=11, xtickv=indgen(12), xtickname=monthstr, $
         yrange=nh_range, yminor=2, /xstyle, /ystyle, ytitle=ytitle, $ ;eds 5/11/11
         ticklen=0.01, pos=p

   TGMdat_arctic = fltarr( 3, 12 )
   TGMmod_arctic = fltarr( 3, 12 )
   TGMref_arctic = fltarr( 3, 12 ) ;eds 5/11/11
   
   for S=0L, n_elements( sites ) -1L do begin
      i = where( TGMsite eq sites[S] )
;      oplot, TGMdat[i[0], *], /color
;      oplot, TGMmod[i[0], *], color=2
      TGMmod_arctic[S, *] = TGMmod[i[0], *]
      TGMdat_arctic[S, *] = TGMdat[i[0], *]
      if (keyword_set(reference)) then begin    ;eds 5/11/11
         TGMref_arctic[S, *] = TGMref[i[0], *]
      endif
   endfor

   oplot, mean2( TGMmod_arctic, 1), color=2, thick=3
   oplot, mean2( TGMdat_arctic, 1), /color, thick=3
   errplot, mean2( TGMdat_arctic, 1) - stddev2( TGMdat_arctic, 1), $
            mean2( TGMdat_arctic, 1) + stddev2( TGMdat_arctic, 1), $
            /color, thick=3

   oplot, mean2( TGMmod_arctic, 1 ), color=2, thick=3
   errplot, mean2( TGMmod_arctic, 1) - stddev2(TGMmod_arctic, 1), $
            mean2( TGMmod_arctic, 1) + stddev2(TGMmod_arctic, 1), $
            color=2, thick=3

   ;eds 5/11/11
   if (keyword_set(reference)) then begin
   oplot, mean2( TGMref_arctic, 1 ), color=4, thick=3
   errplot, mean2( TGMref_arctic, 1) - stddev2(TGMref_arctic, 1), $
            mean2( TGMref_arctic, 1) + stddev2(TGMref_arctic, 1), $
            color=4, thick=3
   endif


   ;------------------------
   ; Asia
   ;------------------------

;   multipanel, /advance, pos=p
;   
;   i = where( TGMsite eq 'ChangbaiMt' )
;   plot, TGMdat[i, *], /color, thick=3, title='Changbai', $
;         xrange=[-0.5,11.5], xticks=11, xtickv=indgen(12), xtickname=monthstr, $
;         yrange=[200, 500],  yminor=2, /xstyle, /ystyle, ytitle='Hg, ppq', $
;         ticklen=0.01, pos=p
;
;   oplot, TGMmod[i, *], color=2, thick=3

   ;------------------------
   ; Southern hemisphere- Cape Point
   ;------------------------

   multipanel, /advance, pos=p
   
   i = where( TGMsite eq 'CapePointClean' )
   plot, TGMdat[i, *], /color, thick=3, title='Cape Point', $
         xrange=[-0.5,11.5], xticks=11, xtickv=indgen(12), xtickname=monthstr, $
         yrange=sh_range, yminor=2, /xstyle, /ystyle, ytitle=ytitle, $ ;eds 5/11/11
         ticklen=0.01, pos=p

   oplot, TGMmod[i, *], color=2, thick=3

   ;eds 5/11/11
   if (keyword_set(reference)) then begin
   oplot, TGMref[i, *], color=4, thick=3
   endif


   ;------------------------
   ; Southern hemisphere- Neumayer
   ;------------------------

   multipanel, /advance, pos=p
   
   i = where( TGMsite eq 'Neumayer' )
   plot, TGMdat[i, *], /color, thick=3, title='Neumayer Station', $
         xrange=[-0.5,11.5], xticks=11, xtickv=indgen(12), xtickname=monthstr, $
         yrange=sh_range, yminor=2, /xstyle, /ystyle, ytitle=ytitle, $ /eds 5/11/11
         ticklen=0.01, pos=p

   oplot, TGMmod[i, *], color=2, thick=3

   ;eds 5/11/11
   if (keyword_set(reference)) then begin
   oplot, TGMref[i, *], color=4, thick=3 
   endif            

   multipanel, /off

   If Keyword_Set( PS ) then $
      ps_setup, /close

end
