;; This function calculates the mean of an array
;; If the array is entirely numerical values, the results is the same
;; as mean(Array).
;; If the array has some NaNa and some numerical values, the result is
;; the same as mean( Array, /NaN )
;; If the array contains only NaNs, then the result is NaN. This is
;; different than mean( Array, /NaN ), which would return 0.0 in this case.


function mean_nan, X, Dim, _Extra=_Extra

   ; Return up a level if an error occurs
   On_Error, 2

   ; Keyword defaults
   if ( N_Elements( Dim ) eq 0 ) then Dim = 0

   ; Return mean of the X along DIM (exclude NaN values)
   mn = Total( X, Dim, /NaN, _EXTRA=_Extra ) / $
        ( Total( Finite( X ), Dim ) > 1 );+ Total( Finite( X, /Inf ), Dim ) )

   i=where( total( finite( x, /inf ), dim ) ge 1, ct )
   if (ct ge 1) then $
      mn[i] = !values.f_infinity

   ; Size of X
   s = size( X, /dim )
   N = n_elements( X )

   if (Dim eq 0) then begin
      if (Total( Finite( X, /NaN ) ) eq N) then $
         mn =!values.d_nan
   endif else begin
      ; locations where array is all NaN
      i = where( Total( Finite( X, /NaN ), Dim ) eq s[Dim-1], ct)
      if (ct ge 1) then $
         mn[i] = !values.f_nan
   endelse

  return, mn
end



;; OLD VERSION (3/14/2009)
;; function mean_nan, Array, Dim, _Extra=_Extra

;;   If total( finite( Array ) ) eq 0 then begin

;;       mn = !Values.D_NaN

;;   endif else begin

;;       mn = mean( Array, /NaN, _Extra=_Extra )  

;;   endelse

;;   return, mn
;; end
