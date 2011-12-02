; $Id$
;-----------------------------------------------------------------------
;+
; NAME:
;        PS_SETUP
;
; PURPOSE:
;        This program should simplify opening, using, and closing
;        PostScript files. The program is designed to provide fast and 
;        basic PostScript support rather than fully customized
;        configurations of PostScript files. PS_SETUP can
;        open, close and preview files. It asks for user confirmation
;        before overwriting a file. By default it loads Helvetica fonts in
;        a size that works for most applications. It also sets page margins 
;        which word well for tall and narrow plot areas. PS_SETUP previews the
;        PostScript file after closing it. PS_SETUP saves the prior
;        plotting state before opening a file, and restores it after
;        closing the file. e.g. If the system was plotting to an
;        X-window before opening a file, the system will plot to
;        X-windows after PS_SETUP has closed the file. Prior font
;        setting are restored as well.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        PS_SETUP[, Keywords]
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;        FILENAME - (string) specify the PostScript filename when
;                   opening a file. Must be used with /OPEN
;        OPEN     - (boolean) Set this keyword to open the file. Must
;                   be used with FILENAME
;        CLOSE    - (boolean) Set this keyword to close the file and
;                   preview it
;        NOVIEW   - (boolean) Set this keyword to prevent viewing the
;                   file after it is closed. Can only be used with
;                   /CLOSE
;        OVERWRITE - (boolean) Set to overwrite a preexisting file
;                    named FILENAME without runtime user input. Use
;                    with /OPEN.
;        XSIZE    - (float) Horizontal size of the plot area in
;                   inches. Use with /OPEN. Default = 6.5
;        YSIZE    - (float) Vertical size of the plot area in
;                   inches. Use with /OPEN. Default = 9
;        XOFFSET  - (float) Horizontal offset of the plot area from
;                   the edge of the page in inches. Use with /OPEN. Default = 1
;        YOFFSET  - (float) Vertical offset of the plot area from
;                   the edge of the page in inches. Use with /OPEN. Default = 1
;        LANDSCAPE - (boolean) Set this keyword to use landscape page
;                    orientation. Use with /OPEN. Default = PORTRAIT
;        PORTRAIT  - (boolean) Set this keyword to use portrait page
;                    orientation. Use with /OPEN. Default = PORTRAIT
;        CHARSIZE - (float) Size of characters. Use with
;                   /OPEN. Default = 1.2
;        PSOPEN   - (boolean) Returns 1 if PostScript file
;                   successfully opened, 0 otherwise.
;        _EXTRA   - Other Keywords are passed to DEVICE
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
;        PS_SETUP, filename='file.ps', /open, /landscape
;        PLOT, findgen(10), /color
;        PS_SETUP, /close
;
; MODIFICATION HISTORY:
;        cdh, 07 Aug 2007: VERSION 1.00
;
;-
; Copyright (C) 2007, Christopher Holmes, Harvard University
; This software is provided as is without any warranty
; whatsoever. It may be freely used, copied or distributed
; for non-commercial purposes. This copyright notice must be
; kept with any copy of this software. If this software shall
; be used commercially or sold as part of a larger package,
; please contact the author.
; Bugs and comments should be directed to cdh@io.as.harvard.edu
; with subject "IDL routine ps_setup"
;-----------------------------------------------------------------------


pro ps_setup, Filename=Filename, Open=Open, Close=Close, noView=noView, $
      xsize=xsize, ysize=ysize, yoffset=yoffset, xoffset=xoffset, $
      landscape=landscape, portrait=portrait, PSOpen=PSOpen, $
      charsize=charsize, Overwrite=Overwrite, thin=thin, _Extra=_Extra
 
   ;====================================================================  
   ; Set up a system variable with status of Postscript file
   ;====================================================================  
 
   ; Save current plot settings in a system variable to restore later
   ; Test to see whether MY_P already exists
   DefSysV, '!MY_PSInfo', Exists=Exists
      
   ; If !MY_PSInfo doesn't exist, then define it
   if ( not Exists ) then begin

      ; Make Structure with necessary information
       PSInfo = CREATE_STRUCT( 'PSOpen', 0, 'PSFile', '', 'PSave', !P, $
                               'XSave', !X, 'YSave', !Y,  $
                               'xsize',  0., 'ysize',  0.,  $
                               'xoffset',0., 'yoffset',0. ) 
 
       DefSysV, '!MY_PSInfo', PSInfo

   endif

   ; Default Value
   psOpen = 0
      
   ;====================================================================  
   ; Set up PostScript, if OPEN Keyword is set and Filename given
   ;====================================================================  
 
   ; Check whether a postscript filename is given
   if Keyword_Set( Open ) and Keyword_Set( Filename ) then  begin
 
      ; Check whether file exists
      FileExists = File_Test( Filename )
      
      ; Default is to ask user to overwrite a file
      if not Keyword_Set( Overwrite ) then Overwrite = 0

      if ( FileExists and not Overwrite ) then begin
         
         ; Print a warning
         print, ''
         print, 'WARNING! Output file already exists: ', Filename
         
         ; Initialize variable as string
         overwrite = ''
         
         ; Prompt user whether to continue
         read, overwrite, prompt='Overwrite existing file? (default: no) '
 
         ; Check if user said yes, exit otherwise
         if ( (overwrite ne 'y'  ) and $ 
              (overwrite ne 'Y'  ) and $
              (overwrite ne 'yes') ) then return
         
      endif
 
      ; Plot area dimensions
      if not Keyword_Set( xsize   ) then xsize = 7
      if not Keyword_Set( ysize   ) then ysize = 9.5
      if not Keyword_Set( yoffset ) then yoffset = 0.75
      if not Keyword_Set( xoffset ) then xoffset = 0.75
      
      ; Store plot dimensions
      !MY_PSInfo.xsize = xsize
      !MY_PSInfo.ysize = ysize
      !MY_PSInfo.xoffset = xoffset
      !MY_PSInfo.yoffset = yoffset
 
      ; Set output plot and device settings
      set_plot, 'ps' 
      device, /color, bits=8, filename=Filename, $
        /inches, xsize=xsize, ysize=ysize, yoffset=yoffset, $
        xoffset=xoffset, /portrait, _Extra=_Extra
 
      ; Use PS Fonts
      !P.font = 0
 
      ; Default font size
      if not Keyword_Set( charsize ) then charsize = 1.2
      !P.charsize = charsize

      if not Keyword_Set( thin ) then begin
         ; Increase thickness of axis lines and plot lines
         !X.Thick = 3
         !Y.Thick = 3
         !P.Thick = 3
      endif
 
      ; Use Helvetica Font
      device, /helvetica, /isolatin1
 
      ; Set default plotting margins
      std_margins

      ; Set flags for open psfile
      !MY_PSInfo.PSOpen = 1
      PSOpen = 1
 
      ; Store open filename
      !MY_PSInfo.PSFile = Filename
 
   endif
 
   ; Display a non-fatal error if OPEN is set, but no filename is given 
   if Keyword_Set( OPEN ) and not Keyword_Set( Filename ) then begin
 
      print, ''
      print, 'No Filename! Cannot Open Postscript!! '
      print, ''
 
      ; Set flags for closed file
      !MY_PSInfo.PSOpen = 0
      PSOpen = 0
 
   endif


   ;====================================================================  
   ; Change Page orientation
   ;====================================================================  

   if ( Keyword_Set( Landscape ) and !MY_PSInfo.PSOpen ) then begin
     device, /inches, xsize=!MY_PSInfo.xsize, ysize=!MY_PSInfo.ysize, $
       xoffset=!MY_PSInfo.xoffset, yoffset=!MY_PSInfo.yoffset
     device, /landscape
   endif

   if ( Keyword_Set( Portrait ) and !MY_PSInfo.PSOpen ) then begin
     device, /portrait, /inches,$ 
       xsize=!MY_PSInfo.xsize, ysize=!MY_PSInfo.ysize, $
       yoffset=!MY_PSInfo.yoffset
   endif

 
   ;====================================================================  
   ; Close PostScript if CLOSE keyword is set
   ;====================================================================  
 
   if Keyword_Set( CLOSE ) and ( !MY_PSInfo.PSOpen ) then begin
 
      ; Restore System Plot State
      !P = !MY_PSInfo.PSave
      !X = !MY_PSInfo.XSave
      !Y = !MY_PSInfo.YSave
      
      ; Close PS file
      device, /close
      set_plot, 'x'
         
      ; Re-orient the landscape view
      if ( Keyword_Set( Landscape ) ) then FixPs, !MY_PSInfo.PSFile

      ; View the PS file, unless NOVIEW is set
      if not Keyword_set( noView ) then begin
         

         IF ( (!Version.OS_Name eq 'Mac OS X') OR $
              (!Version.OS_Name eq 'linux'   ) )Then $
            spawn, 'gv '+!MY_PSInfo.psFile $
         Else $
            spawn, 'ghostview -swap '+!MY_PSInfo.psFile

      endif
 
      ; Set Flags for closed PS file
      !MY_PSInfo.PSOpen = 0
      PSOpen = 0
 
   endif
 
   PSOpen = !MY_PSInfo.PSOpen

end
