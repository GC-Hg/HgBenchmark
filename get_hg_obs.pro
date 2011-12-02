function get_hg_obs, file=HGfile

   if (not Keyword_set( HGfile ) ) then $
      HGfile = '~/data/HgGeneral/TGMSiteMonthly.csv'

   ;=======================================
   ; Read HG data
   ;=======================================
   
   HGsite = ''
   HGlat = 0.
   HGlon = 0.
   HGmon = 0L
   HGyrs = ''
   HGdat = 0.
   HGstd = 0.


   transread_delim, lun,filename=HGfile, skiplines=3, /debug, delim=',', $
              HGsite, HGlat, HGlon, HGmon, HGyrs, HGdat, HGstd, $
              format='(A0,2F0.0,A0,2F0.0)'


   ;=======================================
   ; Process HG data
   ;=======================================

   ; Identify the indices of unique sites
   ii =  uniq( HGsite, sort(HGsite) )

   ; Names of unique sites
   site_uniq = HGsite[ ii ]
   lat_uniq = HGlat[ii]
   lon_uniq = HGlon[ii]
   yrs_uniq = HGyrs[ii]
   nsites = n_elements( site_uniq )
   
   ; Initialize
   dat    = fltarr( nsites, 12 )
   datstd = fltarr( nsites, 12 )

   for S=0L, nsites-1L do begin
   for M=0L, 11L  do begin

      ; Locate data for given site and month
      i = where( strmatch( HGsite, site_uniq[S]) and $
                 (HGmon eq M+1L), ct )

      ; Put the data in an array
      if (ct eq 1) then begin
         dat[S,M] = HGdat[i]
         datstd[S,M] = HGstd[i]
      endif else begin
         print, 'Did not find data for site'+site_uniq[S]
      endelse

   endfor
   endfor
   
   ; Rename everything
   HGsite = site_uniq
   HGlat = lat_uniq
   HGlon = lon_uniq
   HGdat = dat
   HGstd = datstd
   HGyrs = yrs_uniq

   ; Resort everything N -> S
   ii = reverse( sort( HGlat ) )
   HGsite=HGsite[ii]
   HGlon=HGlon[ii]
   HGlat=HGlat[ii]
   HGdat=HGdat[ii,*]
   HGstd=HGstd[ii,*]
   HGyrs=HGyrs[ii]


   ; Replace missing data with NaN
   ii=where( HGdat eq -9999, ct )
   if (ct ge 1) then begin
      HGdat[ii] =!values.f_nan
   endif
   ii=where( HGstd eq -9999, ct )
   if (ct ge 1) then begin
      HGstd[ii] =!values.f_nan
   endif

   data={siteID: HGsite, $
         lat: HGlat, $
         lon: HGlon, $
         HG: HGdat, $
         std: HGstd, $
         years: HGyrs }

return, data

end
