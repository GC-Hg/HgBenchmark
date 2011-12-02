; $Id$
;-----------------------------------------------------------------------
;+
; NAME:
;        TVMAP_REGION
;
; PURPOSE:
;        Wrapper routine for TVMAP and CTM_OVERLAY.
;        Sets up a nice Lambert conformal
;        projection of Europe, North America, or other. 
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        TVMAP_REGION, Data, XArr, YArr [, TrackD, TrackX, TrackY]
;                      [Keywords]
;
; INPUTS:
;        The inputs are exactly the same as TVMAP and CTM_OVERLAY. 
;        The program will invoke TVMAP if there are three input
;        parameters and CTM_OVERLAY if there are 6. The only
;        difference is the REGION keyword.
;
; KEYWORD PARAMETERS:
;        REGION : string specififying plot region. 
;                 Options: CONUS, EUROPE, NAMERICA, JAPAN, AFRICA
;        _EXTRA : All keywords are passed through to TVMAP or CTM_OVERLAY
;
; OUTPUTS:
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;        cdh, 10 Mar 2009: VERSION 1.00
;
;-
; Copyright (C) 2009, Christopher Holmes, Harvard University
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to cdh@io.as.harvard.edu
; with subject "IDL routine tvmap_europe"
;-----------------------------------------------------------------------


pro tvmap_region, Data, XArr, YArr, $
                  TrackD, TrackX, TrackY, $
                  Region=Region, $
                  Isotropic=Isotropic, $
                  Lambert=Lambert, $
                  Orthographic=Orthographic, $
                  NoGXLabels=NoGXLabels, $
                  NoGYLabels=NoGYLabels, $
                  _Extra=_Extra


   ; Regional plotting limits
   case strlowcase( Region ) of
      'japan':begin
         Limit  = [35, 122, 47, 135, 35, 145, 30, 135]
         MParam = [37, 135, 0]
         Isotropic = 1L
         Orthographic = 0L
         Lambert = 1L
         NoGXLabels = 1L
         NoGYLabels = 1L
      end
      'europe':begin
         Limit  = [40, -15, 75, 20, 40, 40, 30, 20]
         MParam = [57, 12, 0]
         Isotropic = 1L
         Orthographic = 0L
         Lambert = 1L
         NoGXLabels = 1L
         NoGYLabels = 1L
      end
      'namerica':begin
         Limit  = [40, -140, 85, -142, 40, -50, 10, -100]
         MParam = [45, -97, 0]
         Isotropic = 1L
         Orthographic = 0L
         Lambert = 1L
         NoGXLabels = 1L
         NoGYLabels = 1L
      end
      'conus':begin
         Limit  = [40, -130, 45, -142, 40, -65, 24, -100]
         MParam = [37, -97, 0]         
         Isotropic = 1L
         Orthographic = 0L
         Lambert = 1L
         NoGXLabels = 1L
         NoGYLabels = 1L
      end
      'canada': begin
         Limit  = [50,150,90,250,50,350,50,250]
         MParam = [90,250,0]
         Isotropic = 1L
         Orthographic = 1L
         Lambert = 0L
         NoGXLabels = 0L
         NoGYLabels = 1L        
      end
      'africa': begin
         Limit  = [0,-20,40,20,0,55,-40,20]
         MParam = [0,17.5,0]
         Isotropic = 1L
         Orthographic = 0L
         Lambert = 1L
         NoGXLabels = 1L
         NoGYLabels = 1L        
      end
      '':
   endcase

   ; If there are 6 arguments, use CTM_OVERLAY, otherwise TVMAP
   if (n_params() eq 6) then begin
 
     ctm_overlay, Data, XArr, Yarr, $
                  TrackD, TrackX, TrackY, $
                  Limit=Limit, MParam=Mparam, Lambert=Lambert, $
                  Isotropic=Isotropic, Orthographic=Orthographic, $
                  NoGXLabels=NoGXLabels, NoGYLabels=NoGYLabels, $
                  _Extra=_Extra     
 
   endif else begin

      tvmap, Data, XArr, Yarr, $
             Limit=Limit, MParam=Mparam, Lambert=Lambert, $
             Isotropic=Isotropic, Orthographic=Orthographic, $
             NoGXLabels=NoGXLabels, NoGYLabels=NoGYLabels, $
             _Extra=_Extra
 
   endelse

end
