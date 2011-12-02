; FUNCTION TAPPLY reproduces the behavior of the R function tapply
; TAPPLY applies the specified function, to all elements of ARRAY
; which have the same GROUP value. The function returns an array with
; as many elements as unique values within GROUP.
;
; INPUTS:
;   1. ARRAY       - array of arbitrary size
;   2. GROUP       - array of same dimension as ARRAY while classifies
;                    elements of ARRAY
;   3. FUNCTIONSTR - string naming an IDL function to apply to ARRAY
;   4. _EXTRA      - any additional keywords are passed to the
;                    specified function
;
; OUTPUT:
;   1. RESULT      - array resulting from the function application
;   2. GROUPVALUES - array containing the unique values of GROUP, in
;                    the same order as RESULT


function tapply, array, group, functionstr, groupvalues=groupvalues, $
                 _EXTRA=_EXTRA
   
   ; Find the unique values in the classification array GROUP
   groupvalues = group[ uniq( group, sort( group ) ) ]

   ; Find the type of the input array
   type = size( array, /type )

   ; Make an array with the same type as the input array
   result = make_array( n_elements( groupvalues ),  type=type )

   ; Loop over the number of unique values
   for i=0, n_elements( groupvalues ) - 1L do begin

      ; Find which elements share the same value
      index = where( group eq groupvalues[i] )
      
      ; Apply the given function to the common elements
      result[i] = call_function( functionstr, array[index], _EXTRA=_EXTRA)
      
   endfor
   
   ; Return the result
   return, result


end
