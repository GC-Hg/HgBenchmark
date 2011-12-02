;----------------------------------------------------------
; helper function: splitter
;
; splits up the strings into double precision data
;----------------------------------------------------------

function splitter, lines=lines, delim=delim

   n = n_elements(lines)

   for i=0, n-1 do begin
      temp = strsplit(lines[i], delim, /extract,/preserve_null)
      ; set dimensions for array on
      ; first pass
      if ( i eq 0 ) then data = strarr(n_elements(temp), n)
      data[*, i] = temp
   endfor

   ; return output to calling function
   return, data

end
;----------------------------------------------------------------

FUNCTION READER2, FILE, NSKIP=NSKIP, DELIM=DELIM, $
                  DBL=DBL, _EXTRA=EXTRA_KEYWORDS
;----------------------------------------------------------------
;
;
;----------------------------------------------------------------
; FILE = string entry, the name of the file you want to read
;
; NSKIP (optional) = number of lines to skip, use to avoid headers
;
; DELIM = set delimiter type instead of passing a format
;----------------------------------------------------------------
; Justin's Modifications:
;
; (1) using a call to skip_lun to allow for skipping lines at
;     the beginning of a data file. 
;     a) the number of lines or data
;        to be skipped are passed as NSKP.
;     b) /lines is the default: NSKIP corresponding to number of 
;        lines to skip. Pass an extra_keyword to override.
;----------------------------------------------------------------
;
; Examples:
;
; (1)    file1 = 'newint.dat'
;        ; reading in
;        fmt_int = '( f8.5, 2(1x, E11.5) )'
;        record1 = zgrid:0d0, Pgrid:0d0, Tgrid:0d0        
;        interp = READ_FMT(file1, fmt=fmt_int, record=record1)


   ;===================
   ; Reading in Data
   ;===================

   ;; Check Arguments
   if(n_elements(file) eq 0) then $
     message,  'Argument FILE is undefined'
   if(strlen(DELIM) eq 0) then $
      message, 'Set the delimeter'

   ;;=====================
   ;; How many lines or
   ;; data points to
   ;; skip
   ;;=====================

   if(n_elements(nskip) eq 0) then begin
      nskip = 0
      print, 'no lines skipped for '+ file
   endif else begin
      print, nskip, ' line(s) skipped for ' + file
   endelse

   ;;=====================
   ;; Open the input file
   ;;=====================
   openr, lun, strtrim(file, 2), /get_lun

   ; number of lines in the file
   nlines = file_lines(strtrim(file, 2))

   ;; if niskip GT 0 then skip lines
   ;; -- the default is to skip nskip number
   ;;    of lines. can be overridden with an
   ;;    extra keyword.
   if(nskip gt 0) then begin
      skip_lun, lun, nskip, /lines, $
        _extra=extra_keywords
      nlines = temporary(nlines) - 1
   endif

   ; create a string array for each line
   lines = strarr(nlines)
   ; read each line as a string
   readf, lun, lines
   ; close the lun
   free_lun, lun

   ; retrieve the data in dble from
   ; the splitter function
   data = splitter(lines=lines, delim=delim)

   ; set the format for data
   ; default is floating point
   if (keyword_set(dbl)) then begin
      data = double(data)
   endif else begin
      data = float(data)
   endelse

   ; return the output to calling program
   return, data

END




