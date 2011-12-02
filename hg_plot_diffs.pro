pro hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
    cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
    perc_max=perc_max, csfac=csfac, tcsfac=tcsfac, cfill=cfill, log=log, $
    sample=sample, color=color, cbformat=cbformat

   ;------------------------------------------------;
   ; eds 5/11/12 quick script to do neat difference ;
   ; plots for use in mercury model benchmarking    ;
   ;------------------------------------------------;

   ; KEYWORD OPTIONS ;

   if ( not Keyword_set( new_title ) ) then $
      new_title = 'New Model Version'

   if ( not Keyword_set( old_title ) ) then $
      old_title = 'Old Model Version'

   if ( not Keyword_set( cbunit ) ) then $
      cbunit = ' '

   if ( not Keyword_set( abs_min ) ) then $
      abs_min = 0

   if ( not Keyword_set( abs_max ) ) then $
      abs_max = max([new_data, old_data])

   if ( not Keyword_set( diff_max ) ) then $
      diff_max = max(abs(new_data-old_data))

   if ( not Keyword_set( perc_max ) ) then $
      perc_max = 1d2

   if ( not Keyword_set( csfac ) ) then $
      csfac = 1

   if ( not Keyword_set( tcsfac ) ) then $
      tcsfac = 1

   if ( not Keyword_set( cfill ) ) then $
      cfill = 0L

   if ( not Keyword_set( log ) ) then $
      log = 0L

   if ( not Keyword_set( sample ) ) then $
      sample = 0L

   if ( not Keyword_set( cbformat ) ) then $
      cbformat = '(I10.0)'


   diff_min = -diff_max
   perc_min = -perc_max

   ; 2D DATA ONLY ;

   s=size(new_data)
   if s[0] eq 3 then begin
      new_data=new_data[*,*,0]
   endif
   s=size(old_data)
   if s[0] eq 3 then begin
      old_data=old_data[*,*,0]
   endif


   ; DIFFERENCE ;

   absdiff = new_data - old_data
   percdiff = 1d2 * absdiff / old_data

   zeros = where(absdiff eq 0)
   if zeros[0] ne -1 then begin
      percdiff(zeros) = 0
   endif

   ; PLOTTING ;

   multipanel, rows=2, cols=2

   if keyword_set(color) then begin
      if color eq 'white' then begin
         myct, /whgrylrd, ncolors=17
      endif
   endif else begin
      myct, 33, ncolors=17
   endelse

   tvmap, new_data, title=new_title, $
        /iso, /coasts, /cbar, cbunit=cbunit, div=5, $
        mindata=abs_min, maxdata=abs_max, /triangle, csfac=csfac, $
        tcsfac=tcsfac, /nogxlabels, /nogylabels, cfill=cfill, $
        log=log, sample=sample, cbformat=cbformat, botoutofrange=!myct.bottom

   tvmap, old_data, title=old_title, $
        /iso, /coasts, /cbar, cbunit=cbunit, div=5, $
        mindata=abs_min, maxdata=abs_max, /triangle, csfac=csfac, $
        tcsfac=tcsfac, /nogxlabels, /nogylabels, cfill=cfill, $
        log=log, sample=sample, cbformat=cbformat, botoutofrange=!myct.bottom

   myct, /diff
   tvmap, absdiff, title='Absolute Difference', $
        /iso, /coasts, /cbar, cbunit=cbunit, div=5, $
        mindata=diff_min, maxdata=diff_max, /triangle, $
        /nogxlabels, /nogylabels, cfill=cfill, csfac=csfac, $
        tcsfac=tcsfac, sample=sample, botoutofrange=!myct.bottom

   tvmap, percdiff, title='Percent Difference', $
       	/iso, /coasts, /cbar, cbunit='%', div=5, $
        mindata=perc_min, maxdata=perc_max, /triangle, $
        /nogxlabels, /nogylabels, cfill=cfill, csfac=csfac, $
        tcsfac=tcsfac, sample=sample, botoutofrange=!myct.bottom


end
