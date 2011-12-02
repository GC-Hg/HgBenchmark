; $Id: mercury_benchmark.pro, v1.0 2011/05/10 eds $
;-----------------------------------------------------------------------
;+
; NAME:
;        MERCURY_BENCHMARK
;
; PURPOSE:
;        Plots model output with comparison to data and previous model 
;        versions for use in evaluating and benchmarking model development.
;
; CATEGORY:
;
; CALLING SEQUENCE:
;        MERCURY_BENCHMARK, FILENAME=FILENAME, REFERENCE=REFERENCE, $
;           PSFILENAME=PSFILENAME, PPQ=PPQ, CHEMISTRY=CHEMISTRY
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;        FILENAME : BPCH file for new model version output
;        REFERENCE : BPCH file for reference or previous model version.
;           If not supplied, will use model output from Amos et al. 2011.
;      	 PSFILENAME : assigned name for output postscript file.
;        PPQ : keyword to do all figures in ppqv instead of ng/m3.
;        CHEMISTRY : set to 'Br' (default) or 'OHO3'
;      	 PREINDUSTRIAL : flag to evaluate a preindustrial spin-up 
;            (in development)
;      	 GTMM :	flag to	evaluate a coupled GTMM	run (in development)
;
; OUTPUTS:
;        Postscript file with figures and model budget
;
; SUBROUTINES:
;        Internal subroutines:
;        ------------------------------------------------------;        
;
;        External subroutines:
;        ------------------------------------------------------
;        PS_SETUP (cdh)
;        PLOT_SURFACE_MEAN_TGM (cdh)
;        GET_HG_OBS (cdh)
;        CTM_OVERLAY (modified version from cdh)
;        TVMAP_REGION (cdh)
;        TRANSREAD_DELIM (nasa)
;        TRANSREAD (nasa)
;        TAPPLY (apl)
;        MAKEN (apl)
;        MEAN2 (apl)
;        GET_MEAN_CONCENTRATION (cdh)
;        HG_PLOT_DIFFS (eds)
;        PLOT_SURFACE_MEAN_HG2HGP (cdh)
;        PLOT_SEASONS_SITES_MEAN (cdh)
;        PLOT_SEASONS_SITES (cdh)
;        PLOT_GRADIENT_TGM (cdh)
;        PLOT_ACEASIA (cdh)
;        GET_TOTAL_WETDEP (cdh)
;        PLOT_USA_WETDEP (cdh)
;        PLOT_EUROPE_WETDEP (cdh)
;        GET_TOTAL_EMISSION (cdh)
;        GET_TOTAL_OCEAN_MASS (eds)
;        READER2 ()
;        PLOT_OCEAN_GEM_DATA (eds)
;        PLOT_OCEAN_AQU_DATA (eds)
;        
;
; REQUIREMENTS:
;        Benchmark data needs to be located in folder data/ one directory
;        down from current directory.
;
; NOTES:
;       (1) Need for benchmarking mercury model development discussed in 
;           working group at 5th International GEOS-Chem Meeting 5/4/11. 
;           Bess Corbitt (eds) assembled first version using, modifying, and
;           supplimenting pre-existing routines from Chris Holmes (cdh) and 
;           other Team Hg members.
;           
; EXAMPLE:
;        MERCURY_BENCHMARK, FILENAME='ctm.v80302.bpch', $
;           REFERENCE='ctm.v90101.bpch', PSFILENAME='benchmark_v90101.ps'
;
; MODIFICATION HISTORY:
;        eds, 10 May 2011: VERSION 1.0
;        jaf,  4 Oct 2011: Cleaned-up version for distribution outside
;                          Harvard
;-----------------------------------------------------------------------

PRO MERCURY_BENCHMARK, FILENAME=FILENAME, REFERENCE=REFERENCE, $
       PSFILENAME=PSFILENAME, PPQ=PPQ

   ; SET-UP ;

   ; Conversion factors
   pptv_ngm3 = 8.93d0
   ngm3_ppqv = 112d0

   if (not keyword_set(ppq)) then $
       ppq = 0L

   if (not keyword_set(Chemistry)) then $
       Chemistry = 'Br'

   if (not keyword_set(Preind)) then $
       Preind = 0L

   if (not keyword_set(Reference)) then $
       Reference = 'default.4x5.ctm.bpch'

   ;assign generic output filename if not provided
   if (not (keyword_set(psfilename))) then $
      psfilename = 'mercury_benchmark.ps'

   ;open postscript file
   ps_setup, /open, file=psfilename, xsize=10, ysize=6, psopen=psopen, $
      /landscape

   ;----------------------------------;
   ;                                  ;
   ;   ATMOSPHERIC CONCENTRATIONS     ;
   ;                                  ;
   ;----------------------------------;


   ; SURFACE TGM ;

   ;plot modeled & observed global surface TGM 
   plot_surface_mean_TGM, filename=filename, /iso, $
      pagetitle='New Model Version: Surface TGM', $
      ppq=ppq
   ; also plot reference for quantitative comparison with sites
   plot_surface_mean_TGM, filename=reference, /iso, $
      pagetitle='Old Model Version: Surface TGM', $
      ppq=ppq

   ;get surface TGM for new model version 
   new_tgm = get_mean_concentration(filename=filename, $
                species=['Hg0', 'Hg2'],/tgm)
   ;get surface TGM for old model version 
   old_tgm = get_mean_concentration(filename=reference, $
                species=['Hg0', 'Hg2'],/tgm)
   if (keyword_set(ppq)) then begin
      new_tgm = new_tgm*1d3 ;pptv->ppqv
      old_tgm = old_tgm*1d3 
   endif else begin
      new_tgm = new_tgm*pptv_ngm3
      old_tgm = old_tgm*pptv_ngm3
   endelse

   absdiff = new_tgm - old_tgm
   percdiff = 1d2*absdiff/old_tgm
   zeros = where(absdiff eq 0)
   if zeros[0] ne -1 then begin
      percdiff(zeros) = 0 
   endif

   if (keyword_set(ppq)) then begin
      clev = [maken(100, 200, 11), maken(250, 500, 6)]
      cbunit1 = 'ppqv !C Not Linear'
      cbunit2 = 'ppqv'
      abs_max = 30
   endif else begin
      clev = [maken(0.75, 1.75, 11), maken(2.0, 3.5, 6)]
      cbunit1 = 'ng/m!u3!n !C Not Linear'
      cbunit2 = 'ng/m!u3!n'
      abs_max = 0.3
   endelse

   multipanel, rows=2, cols=2
   myct, 33, ncolors=17
   tvmap, new_tgm[*,*,0], title='New Model Version: Surface TGM', $
        /iso, /coasts, /cbar, cbunit=cbunit1, $
        c_levels=clev, /triangle, csfac=0.65, tcsfac=1.5, $
        /nogxlabels, /nogylabels, botoutofrange=!myct.bottom

   tvmap, old_tgm[*,*,0], title='Reference Model Version: Surface TGM', $
        /iso, /coasts, /cbar, cbunit=cbunit1, $
        c_levels=clev, /triangle, csfac=0.65, tcsfac=1.5, $
       	/nogxlabels, /nogylabels, botoutofrange=!myct.bottom

   myct, /diff
   tvmap, absdiff[*,*,0], title='Absolute Difference', $
        /iso, /coasts, /cbar, cbunit=cbunit2, div=5, $
        mindata=-abs_max, maxdata=abs_max, /triangle, $
        csfac=0.65, tcsfac=1.5, $
       	/nogxlabels, /nogylabels, botoutofrange=!myct.bottom

   tvmap, percdiff[*,*,0], title='Percent Difference', $
        /iso, /coasts, /cbar, cbunit='%', div=5, $
        mindata=-20, maxdata=20, /triangle, $
        csfac=0.65, tcsfac=1.5, $
       	/nogxlabels, /nogylabels, botoutofrange=!myct.bottom

   multipanel, /off

   ; SURFACE RGM+TPM ;

   ;plot modeled & observed global surface rgm + hgp
   plot_surface_mean_hg2hgp, filename=filename, /iso, $
      pagetitle='New Model Version: Surface Hg(II)+Hg(P)', $
      ppq=ppq

   ;get surface TGM for new and model versions
   new_hg2 = get_mean_concentration(filename=filename, $
                species=['Hg2', 'HgP'])
   old_hg2 = get_mean_concentration(filename=reference, $
                species=['Hg2', 'HgP'])
   if (keyword_set(ppq)) then begin
      new_hg2 = new_hg2*1d3 ;pptv->ppqv
      old_hg2 = old_hg2*1d3
   endif else begin
      new_hg2 = new_hg2*pptv_ngm3*1d3 ;pg/m3
      old_hg2 = old_hg2*pptv_ngm3*1d3 ;pg/m3
   endelse

   new_data = new_hg2
   old_data = old_hg2
   new_title = 'New Model Version: Surface Hg(II)+Hg(P)'
   old_title = 'Old Model Version: Surface Hg(II)+Hg(P)'
   if keyword_set(ppq) then begin
      cbunit = 'ppqv'
      abs_max = 15
   endif else begin
      cbunit = 'pg/m!u3!n'
      abs_max = 150
   endelse
   diff_max = 50
   perc_max = 100

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max

   ; SEASONAL CYCLE ;

   ;mean northern hemisphere mid-latitude stations
   plot_seasons_sites_mean, filename=filename, reference=reference, ppq=ppq

   ;mean northern hemisphere mid-latitude stations Hg0
   plot_seasons_sites_mean_hg0, filename=filename, reference=reference, ppq=ppq

   ;all stations
   plot_seasons_sites, filename=filename, reference=reference, ppq=ppq


   ; MERIDIONAL GRADIENT ;

   plot_gradient_tgm, filename=filename, reference=reference, ppq=ppq


   ; VERTICAL PROFILE ;
   ; Remove vertical profile comparisons for now as we only have one
   ; data set and this really requires better sampling along flight
   ; track as opposed to averaging over large region. (jaf, 10/11/11)
   ; Note: this routine doesn't yet allow variable HgII gas fraction,
   ; so TGM results should be viewed with caution
   ;plot_aceasia, FileName=FileName, Reference=Reference, ppq=ppq

   ;Note: add plotting routines for INTEX-B, ARCTAS, & CARIBIC
   ;when have the data from cdh

   ;----------------------------------;
   ;                                  ;
   ;   DEPOSITION                     ;
   ;                                  ;
   ;----------------------------------;

   ; WET DEPOSITION

   ;plot usa wetdep with mdn data
   plot_usa_wetdep, FileName=FileName

   ;plot seasonal usa wetdep with mdn data
   plot_usa_wetdep_seasonal,  FileName =  FileName,  Reference= Reference

   ;plot europe wetdep with emep data
   plot_europe_wetdep, FileName=FileName, /nearest_year

   ;get data & grid info
   ;
   ;assume all files in FileName have the same model 
   ; and grid info (H Amos, 29 May 2011)
   ctm_get_data, NewDataInfo, FileName=FileName[0]
   getmodelandgridinfo, NewDataInfo[0], NewModelInfo, NewGridInfo
   ctm_get_data, OldDataInfo, FileName=Reference[0]
   getmodelandgridinfo, OldDataInfo[0], OldModelInfo, OldGridInfo

   ;get new and old model wet dep
   new_wd = get_total_wetdep( FileName = FileName, species=['Hg2', 'HgP'], $
                              GridInfo=NewGridInfo, Year=Year, total=new_tot_wd )
   old_wd = get_total_wetdep( FileName = Reference, species=['Hg2', 'HgP'], $
                              GridInfo=OldGridInfo, Year=Year, total=old_tot_wd )

   new_data = new_wd
   old_data = old_wd
   new_title = 'New Model Version: Total Wet Dep'
   old_title = 'Old Model Version: Total Wet Dep'
   cbunit = 'ug/m!u2!n/y'
   abs_min = 0
   abs_max = 20
   diff_max = 5
   perc_max = 50
   color = 'white'

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max, color=color


  ; DRY DEPOSITION ;

   ;get new and old model hg0 dry dep
   new_dd_hg0 = get_total_drydep( Filename=Filename, $
                   Species='Hg0', total=new_tot_dd_hg0)
   old_dd_hg0 = get_total_drydep( Filename=Reference, $
                   Species='Hg0', total=old_tot_dd_hg0)

   new_data = new_dd_hg0
   old_data = old_dd_hg0
   new_title = 'New Model Version: Hg(0) Dry Dep'
   old_title = 'Old Model Version: Hg(0) Dry Dep'
   cbunit = 'ug/m!u2!n/y'
   abs_min = 0
   abs_max = 20
   diff_max = 5
   perc_max = 50
   color = 'white'

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max, color=color

   ;get new and old model hg2+hgp dry dep
   new_dd_hg2 = get_total_drydep( Filename=Filename, $
                   Species=['Hg2','HgP'], total=new_tot_dd_hg2)
   old_dd_hg2 = get_total_drydep( Filename=Reference, $
                   Species=['Hg2','HgP'], total=old_tot_dd_hg2)

   new_data = new_dd_hg2
   old_data = old_dd_hg2
   new_title = 'New Model Version: Hg(II)+Hg(P) Dry Dep'
   old_title = 'Old Model Version: Hg(II)+Hg(P) Dry Dep'
   cbunit = 'ug/m!u2!n/y'
   abs_min = 0
   abs_max = 10
   diff_max = 2.5
   perc_max = 50
   color = 'white'

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max, color=color

   ;get new and old model sea salt uptake
   new_dd_ss = get_total_seasalthg( Filename=Filename, total=new_tot_ss)
   old_dd_ss = get_total_seasalthg( Filename=Reference, total=old_tot_ss)

   new_data = new_dd_ss
   old_data = old_dd_ss
   new_title = 'New Model Version: Sea Salt Uptake'
   old_title = 'Old Model Version: Sea Salt Uptake'
   cbunit = 'ug/m!u2!n/y'
   abs_min = 0
   abs_max = 10
   diff_max = 2.5
   perc_max = 100
   cfill = 1L
   color = 'white'

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max, cfill=cfill, color=color

   ;----------------------------------;
   ;                                  ;
   ;   EMISSIONS                      ;
   ;                                  ;
   ;----------------------------------;

   ; ANTHRO - HG0 ;

   new_anthro_hg0 = get_total_emission(filename=filename, $
                source='anthro', species=['Hg0'], $
                total=new_tot_anthro_hg0)

   old_anthro_hg0 = get_total_emission(filename=reference, $
               	source='anthro', species=['Hg0'], $
                total=old_tot_anthro_hg0)

   new_data = new_anthro_hg0
   old_data = old_anthro_hg0
   new_title = 'New Model Version: Anthro Emissions - Hg(0)'
   old_title = 'Old Model Version: Anthro Emissions - Hg(0)'
   cbunit = 'kg/y'
   abs_min = 0
   abs_max = 100
   diff_max = 20
   perc_max = 50
   log = 1L
   sample = 1L
   color = 'white'
   cbformat = '(E10.0)'

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max, log=log, sample=sample, color=color, cbformat=cbformat


   ; ANTHRO - HG2 & HGP ; 

   new_anthro_hg2 = get_total_emission(filename=filename, $
                source='anthro', species=['Hg2', 'HgP'], $
                total=new_tot_anthro_hg2)

   old_anthro_hg2 = get_total_emission(filename=reference, $
                source='anthro', species=['Hg2', 'HgP'], $
                total=old_tot_anthro_hg2)

   new_data = new_anthro_hg2
   old_data = old_anthro_hg2
   new_title = 'New Model Version: Anthro Emissions - Hg(II)+Hg(P)'
   old_title = 'Old Model Version: Anthro Emissions - Hg(II)+Hg(P)'
   cbunit = 'kg/y'
   abs_min = 0
   abs_max = 100
   diff_max = 20
   perc_max = 50
   log = 1L
   sample = 1L
   color = 'white'
   cbformat = '(E10.0)'

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max, log=log, sample=sample, color=color, cbformat=cbformat


   ; DIRECT TERRESTRIAL ;

   new_land = get_total_emission(filename=filename, $
              source=['geo','bioburn','soil'], $
              total=new_tot_land)

   old_land = get_total_emission(filename=reference, $
              source=['geo','bioburn','soil'], $
              total=old_tot_land)

   ;for mercury budget summary
   new_geo = get_total_emission(filename=filename, $
              source=['geo'], $
              total=new_tot_geo)

   old_geo = get_total_emission(filename=reference, $
              source=['geo'], $
              total=old_tot_geo)

   new_bb = get_total_emission(filename=filename, $
              source=['bioburn'], $
              total=new_tot_bb)

   old_bb = get_total_emission(filename=reference, $
              source=['bioburn'], $
              total=old_tot_bb)

   new_soil = get_total_emission(filename=filename, $
              source=['soil'], $
              total=new_tot_soil)

   old_soil = get_total_emission(filename=reference, $
              source=['soil'], $
              total=old_tot_soil)

   new_data = new_land
   old_data = old_land
   new_title = 'New Model Version: Direct Terrestrial - Geo, BB, & Soil'
   old_title = 'Old Model Version: Direct Terrestrial - Geo, BB, & Soil'
   cbunit = 'kg/y'
   abs_min = 0
   abs_max = 25
   diff_max = 10
   perc_max = 50
   sample = 1L
   color = 'white'

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max, sample=sample, color=color


   ; TERRESTRIAL PROMPT REEMISSION ;

   new_reemis = get_total_emission(filename=filename, $
              source=['rapid','snow'], $
              total=new_tot_reemis)

   old_reemis = get_total_emission(filename=reference, $
              source=['rapid','snow'], $
              total=old_tot_reemis)

   ;for mercury budget summary
   new_recy = get_total_emission(filename=filename, $
              source=['rapid'], $
              total=new_tot_recy)

   old_recy = get_total_emission(filename=reference, $
              source=['rapid'], $
              total=old_tot_recy)

   new_snow = get_total_emission(filename=filename, $
              source=['snow'], $
              total=new_tot_snow)

   old_snow = get_total_emission(filename=reference, $
              source=['snow'], $
              total=old_tot_snow)

   new_data = new_reemis
   old_data = old_reemis
   new_title = 'New Model Version: Prompt Reemission - Land & Snow'
   old_title = 'Old Model Version: Prompt Reemission - Land & Snow'
   cbunit = 'kg/y'
   abs_min = 0
   abs_max = 15
   diff_max = 5
   perc_max = 50
   sample = 1L
   color = 'white'

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max, sample=sample, color=color

   ;----------------------------------;
   ;                                  ;
   ;   OCEAN EXCHANGE                 ;
   ;                                  ;
   ;----------------------------------;

   ; COMPARISON TO DATA ;

   plot_ocean_gem_data, filename=filename, ppq=ppq
   
   plot_ocean_aqu_data, filename=filename


   ; OCEAN HG0 ;

   new_hg0aq = get_total_ocean_mass(filename=filename, $
                species='hg0aq', $
                total=new_tot_hg0aq)

   old_hg0aq = get_total_ocean_mass(filename=reference, $
                species='hg0aq', $
                total=old_tot_hg0aq)

   new_data = new_hg0aq
   old_data = old_hg0aq
   new_title = 'New Model Version: Ocean Hg(0) Mass'
   old_title = 'Old Model Version: Ocean Hg(0) Mass'
   cbunit = 'kg'
   abs_min = 0
   abs_max = 600
   diff_max = 200
   perc_max = 50
   color = 'white'
   cfill = 1L

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max, color=color, cfill=cfill

   ; OCEAN HG(II) ;

   new_hg2aq = get_total_ocean_mass(filename=filename, $
               	species='hg2aq', $
               	total=new_tot_hg2aq)

   old_hg2aq = get_total_ocean_mass(filename=reference, $
                species='hg2aq', $
               	total=old_tot_hg2aq)

   new_data = new_hg2aq
   old_data = old_hg2aq
   new_title = 'New Model Version: Ocean Hg(II) Mass'
   old_title = 'Old Model Version: Ocean Hg(II) Mass'
   cbunit = 'kg'
   abs_min = 0
   abs_max = 6000
   diff_max = 2000
   perc_max = 50
   color = 'white'
   cfill = 1L

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max, color=color, cfill=cfill

   ; OCEAN HG(P) ;

   new_hgPaq = get_total_ocean_mass(filename=filename, $
               	species='hgpaq', $
               	total=new_tot_hgPaq)

   old_hgPaq = get_total_ocean_mass(filename=reference, $
                species='hgpaq', $
               	total=old_tot_hgPaq)

   new_data = new_hgPaq
   old_data = old_hgPaq
   new_title = 'New Model Version: Ocean Particulate Mass'
   old_title = 'Old Model Version: Ocean Particulate Mass'
   cbunit = 'kg'
   abs_min = 0
   abs_max = 600
   diff_max = 200
   perc_max = 50
   color = 'white'
   cfill = 1L

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max, color=color, cfill=cfill

   ; OCEAN TOTAL HG ;

   new_hgTaq = get_total_ocean_mass(filename=filename, $
               	species='hgtaq', $
               	total=new_tot_hgTaq)

   old_hgTaq = get_total_ocean_mass(filename=reference, $
                species='hgtaq', $
               	total=old_tot_hgTaq)

   new_data = new_hgTaq
   old_data = old_hgTaq
   new_title = 'New Model Version: Ocean Total Inorganic Hg Mass'
   old_title = 'Old Model Version: Ocean Total Inorganic Hg Mass'
   cbunit = 'kg'
   abs_min = 0
   abs_max = 6000
   diff_max = 2000
   perc_max = 50
   color = 'white'
   cfill = 1L

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max, color=color, cfill=cfill


   ; GROSS EVASION ;

   new_fxup = get_total_emission(filename=filename, $
                source='fxup', $
                total=new_tot_fxup)

   old_fxup = get_total_emission(filename=reference, $
                source='fxup', $
                total=old_tot_fxup)

   new_data = new_fxup
   old_data = old_fxup
   new_title = 'New Model Version: Gross Ocean Evasion'
   old_title = 'Old Model Version: Gross Ocean Evasion'
   cbunit = 'kg/y'
   abs_min = 0
   abs_max = 50
   diff_max = 10
   perc_max = 50
   color = 'white'
   cfill = 1L

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max, color=color, cfill=cfill


   ; GROSS HG0 UPTAKE ;

   new_fxdn = get_total_emission(filename=filename, $
                source='fxdn', $
                total=new_tot_fxdn)

   old_fxdn = get_total_emission(filename=reference, $
                source='fxdn', $
                total=old_tot_fxdn)

   new_data = new_fxdn      
   old_data = old_fxdn      
   new_title = 'New Model Version: Gross Ocean Hg(0) Uptake'
   old_title = 'Old Model Version: Gross Ocean Hg(0) Uptake'
   cbunit = 'kg/y'
   abs_min = 0
   abs_max = 20
   diff_max = 5
   perc_max = 50
   color = 'white'
   cfill = 1L

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max, color=color, cfill = cfill


   ; NET OCEAN EVASION ;

   new_fxnet = get_total_emission(filename=filename, $
                source='ocean_net', $
                total=new_tot_fxnet)

   old_fxnet = get_total_emission(filename=reference, $
                source='ocean_net', $
                total=old_tot_fxnet)

   new_data = new_fxnet      
   old_data = old_fxnet      
   new_title = 'New Model Version: Net Ocean Evasion'
   old_title = 'Old Model Version: Net Ocean Evasion'
   cbunit = 'kg/y'
   abs_min = 0
   abs_max = 40
   diff_max = 10
   perc_max = 50
   color = 'white'
   cfill = 1L

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max, color=color, cfill=cfill


   ; OCEAN HG SINKING ;

   new_sink = get_total_emission(filename=filename, $
                source='sink', species='Hg2', $
                total=new_tot_sink)

   old_sink = get_total_emission(filename=reference, $
                source='sink', species='Hg2', $
                total=old_tot_sink)

   new_data = new_sink
   old_data = old_sink
   new_title = 'New Model Version: Ocean Hg Sinking'
   old_title = 'Old Model Version: Ocean Hg Sinking'
   cbunit = 'kg/y'
   abs_min = 0
   abs_max = 50
   diff_max = 10
   perc_max = 50
   color = 'white'
   cfill = 1L

   hg_plot_diffs, new_data, old_data, new_title=new_title, old_title=old_title, $
   cbunit=cbunit, abs_min=abs_min, abs_max=abs_max, diff_max=diff_max, $
   perc_max=perc_max, color=color, cfill=cfill

   ;----------------------------------;
   ;                                  ;
   ;   CHEMISTRY                      ;
   ;                                  ;
   ;----------------------------------;

   plot_zonal_redox, filename=filename, $
       /noplot, chemistry=chemistry, $
       new_oxbr, new_oxoh, new_oho3, $
       new_red, new_netox, new_xmid, new_ymid

   plot_zonal_redox, filename=reference, $
       /noplot, chemistry=chemistry, $
       old_oxbr, old_oxoh, old_oho3, $
       old_red, old_netox, old_xmid, old_ymid

   ; Net Oxidation

   if (chemistry eq 'Br') then begin

   absdiff = new_oxbr - old_oxbr
   percdiff = 1d2*absdiff/old_oxbr
   zeros = where(absdiff eq 0)
   if zeros[0] ne -1 then begin
      percdiff(zeros) = 0 
   endif

   multipanel, rows=2, cols=2
   myct, /whgrylrd, ncolors=17
   tvplot, new_oxbr, new_xmid, new_ymid, /sample, $
           title='New Model Version: Zonal Gross Ox by Br', $
           /ystyle, mindata=0, maxdata=40, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='kg/m!u3!n/y'

   tvplot, old_oxbr, new_xmid, new_ymid, /sample, $
           title='Old Model Version: Zonal Gross Ox by Br', $
           /ystyle, mindata=0, maxdata=40, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='kg/m!u3!n/y'

   myct, /diff, ncolors=17
   tvplot, absdiff, new_xmid, new_ymid, /sample, $
           title='Absolute Difference', $
           /ystyle, mindata=-5, maxdata=5, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='kg/m!u3!n/y'

   tvplot, percdiff, new_xmid, new_ymid, /sample, $
           title='Percent Difference', $
           /ystyle, mindata=-20, maxdata=20, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='%'

   multipanel, /off

 


   endif else begin

   absdiff = new_oxoh - old_oxoh
   percdiff = 1d2*absdiff/old_oxoh
   zeros = where(absdiff eq 0)
   if zeros[0] ne -1 then begin
      percdiff(zeros) = 0 
   endif

   multipanel, rows=2, cols=2
   myct, /whgrylrd, ncolors=17
   tvplot, new_oxoh, new_xmid, new_ymid, /sample, $
           title='New Model Version: Zonal Gross Ox by OH', $
           /ystyle, mindata=0, maxdata=40, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='kg/m!u3!n/y'

   tvplot, old_oxoh, new_xmid, new_ymid, /sample, $
           title='Old Model Version: Zonal Gross Ox by OH', $
       	   /ystyle, mindata=0, maxdata=40, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $ 
       	   cbunit='kg/m!u3!n/y'

   myct, /diff, ncolors=17
   tvplot, absdiff, new_xmid, new_ymid, /sample, $
           title='Absolute Difference', $
       	   /ystyle, mindata=-5, maxdata=5, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $ 
       	   cbunit='kg/m!u3!n/y'

   tvplot, percdiff, new_xmid, new_ymid, /sample, $
           title='Percent Difference', $
       	   /ystyle, mindata=-20, maxdata=20, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $ 
       	   cbunit='%'

   multipanel, /off


   absdiff = new_oxo3 - old_oxo3
   percdiff = 1d2*absdiff/old_oxo3
   zeros = where(absdiff eq 0)
   if zeros[0] ne -1 then begin
      percdiff(zeros) = 0 
   endif

   multipanel, rows=2, cols=2
   myct, /whgrylrd, ncolors=17
   tvplot, new_oxo3, xmid, ymid, /sample, $
           title='New Model Version: Zonal Gross Ox by O!d3!n', $
           /ystyle, mindata=0, maxdata=40, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='kg/m!u3!n/y'

   tvplot, old_oxo3, xmid, ymid, /sample, $
           title='Old Model Version: Zonal Gross Ox by O!d3!n', $
           /ystyle, mindata=0, maxdata=40, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='kg/m!u3!n/y'

   myct, /diff, ncolors=17
   tvplot, absdiff, xmid, ymid, /sample, $
           title='Absolute Difference', $
           /ystyle, mindata=-5, maxdata=5, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='kg/m!u3!n/y'

   tvplot, percdiff, xmid, ymid, /sample, $
           title='Percent Difference', $
           /ystyle, mindata=-20, maxdata=20, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='%'

   multipanel, /off


   endelse

   absdiff = new_netox - old_netox
   percdiff = 1d2*absdiff/old_netox
   zeros = where(absdiff eq 0)
   if zeros[0] ne -1 then begin
      percdiff(zeros) = 0 
   endif

   multipanel, rows=2, cols=2
   myct, /whgrylrd, ncolors=17
   tvplot, new_netox, xmid, ymid, /sample, $
           title='New Model Version: Zonal Net Oxidation', $
           /ystyle, mindata=0, maxdata=40, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='kg/m!u3!n/y'

   tvplot, old_netox, xmid, ymid, /sample, $
           title='Old Model Version: Zonal Net Oxidation', $
           /ystyle, mindata=0, maxdata=40, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='kg/m!u3!n/y'

   myct, /diff, ncolors=17
   tvplot, absdiff, xmid, ymid, /sample, $
           title='Absolute Difference', $
           /ystyle, mindata=-5, maxdata=5, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='kg/m!u3!n/y'

   tvplot, percdiff, xmid, ymid, /sample, $
           title='Percent Difference', $
           /ystyle, mindata=-100, maxdata=100, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='%'

   multipanel, /off

   absdiff = new_red - old_red
   percdiff = 1d2*absdiff/old_red
   zeros = where(absdiff eq 0)
   if zeros[0] ne -1 then begin
      percdiff(zeros) = 0 
   endif

   multipanel, rows=2, cols=2
   myct, /whgrylrd, ncolors=17
   tvplot, new_red, xmid, ymid, /sample, $
           title='New Model Version: Zonal Gross Reduction', $
           /ystyle, mindata=0, maxdata=40, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='kg/m!u3!n/y'

   tvplot, old_red, xmid, ymid, /sample, $
           title='Old Model Version: Zonal Gross Reduction', $
           /ystyle, mindata=0, maxdata=40, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='kg/m!u3!n/y'

   myct, /diff, ncolors=17
   tvplot, absdiff, xmid, ymid, /sample, $
           title='Absolute Difference', $
           /ystyle, mindata=-5, maxdata=5, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='kg/m!u3!n/y'

   tvplot, percdiff, xmid, ymid, /sample, $
           title='Percent Difference', $
           /ystyle, mindata=-100, maxdata=100, $
           xrange=[-90, 90], /xstyle, $
           xtitle='Latitude', ytitle='Altitude', $
           csfac=1, tcsfac=1, /cbar, div=5, $
           cbunit='%'

   multipanel, /off



   ;----------------------------------;
   ;                                  ;
   ;   GLOBAL BUDGET                  ;
   ;                                  ;
   ;----------------------------------;

    mercury_budget, filename, preind=preind, $
    new_Hg0masstrop, new_Hg2masstrop, new_HgPmasstrop, $
    new_Hg0ocMass, new_Hg2ocMass, new_HgCocMass, $
    new_em_Hg0_anthro, new_em_Hg2_anthro, new_em_HgP_anthro, $
    new_em_Hg0_geo, new_em_Hg0_soil, new_em_Hg0_BB, $
    new_em_Hg0_land, new_em_Hg0_snow, new_em_Hg0_oc_up, $
    new_em_Hg0_oc_dn, new_em_Hg0_oc, new_em_total, $
    new_Hg0DryD, new_Hg2DryD, new_HgPDryD, new_Hg2wet, new_HgPwet, $
    new_pl_Hg_seasalt, new_dep_total, new_pl_Hg_Br, $
    new_pl_Hg_OH, new_pl_Hg_O3, new_pl_Hg_reduction, $
    new_t_Hg0chemtrop, new_t_Hg2chemtrop, new_t_TGMtrop

    mercury_budget, reference, preind=preind, $
    old_Hg0masstrop, old_Hg2masstrop, old_HgPmasstrop, $
    old_Hg0ocMass, old_Hg2ocMass, old_HgCocMass, $
    old_em_Hg0_anthro, old_em_Hg2_anthro, old_em_HgP_anthro, $
    old_em_Hg0_geo, old_em_Hg0_soil, old_em_Hg0_BB, $
    old_em_Hg0_land, old_em_Hg0_snow, old_em_Hg0_oc_up, $
    old_em_Hg0_oc_dn, old_em_Hg0_oc, old_em_total, $
    old_Hg0DryD, old_Hg2DryD, old_HgPDryD, old_Hg2wet, old_HgPwet, $
    old_pl_Hg_seasalt, old_dep_total, old_pl_Hg_Br, $
    old_pl_Hg_OH, old_pl_Hg_O3, old_pl_Hg_reduction, $
    old_t_Hg0chemtrop, old_t_Hg2chemtrop, old_t_TGMtrop

   multipanel, rows=1, cols=1

   dummy=fltarr(72,46)
   myct,/whgrylrd,n_colors=17
   tvmap, dummy, /noborder, /nogx, /nogy

   ; number of files 
   ofiles = n_elements( Reference)
   nfiles = n_elements( FileName )

   xyouts, 0.6, 9.5, 'OLD MODEL VERSION', color=1
   xyouts, 0.6, 9.0, 'TROPOSPHERIC MASS', color=1
   xyouts, 0.6, 8.7, string('Hg!u0!n:', string(old_Hg0masstrop/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 0.6, 8.4, string('Hg!u2!n:', string(old_Hg2masstrop/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 0.6, 8.1, string('Hg!uP!n:', string(old_HgPmasstrop/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 0.6, 7.5, 'SURFACE OCEAN MASS', color=1
   xyouts, 0.6, 7.2, string('Hg!u0!n:', string(old_Hg0ocmass/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 0.6, 6.9, string('Hg!u2!n:', string(old_Hg2ocmass/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 0.6, 6.6, string('Hg!uC!n:', string(old_HgCocmass/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 0.6, 6.0, 'EMISSIONS', color=1
   xyouts, 0.6, 5.7, string('Hg!u0!n anthro:', string(old_em_Hg0_anthro/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 0.6, 5.4, string('Hg!u2!n anthro:', string(old_em_Hg2_anthro/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 0.6, 5.1, string('Hg!uP!n anthro:', string(old_em_HgP_anthro/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 0.6, 4.8, string('Hg!u0!n geo:', string(old_em_Hg0_geo/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 0.6, 4.5, string('Hg!u2!n soil:', string(old_em_Hg0_soil/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 0.6, 4.2, string('Hg!uP!n bb:', string(old_em_Hg0_BB/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 0.6, 3.9, string('Hg!u0!n land re:', string(old_em_Hg0_land/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 0.6, 3.6, string('Hg!u0!n snow:', string(old_em_Hg0_snow/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 0.6, 3.3, string('Hg!u0!n oc evasion:', string(old_em_Hg0_oc_up/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 0.6, 3.0, string('TOTAL EMISSIONS:', string(old_em_total/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 4.0, 9.0, 'DEPOSITION', color=1
   xyouts, 4.0, 8.7, string('Hg!u0!n dd:', string(old_Hg0DryD/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 4.0, 8.4, string('Hg!u2!n dd:', string(old_Hg2DryD/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 4.0, 8.1, string('Hg!uP!n dd:', string(old_HgPDryD/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 4.0, 7.8, string('Hg!u2!n wd:', string(old_Hg2Wet/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 4.0, 7.5, string('Hg!uP!n wd:', string(old_HgPWet/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 4.0, 7.2, string('Hg!u0!n oc uptake:', string(old_em_Hg0_oc_dn/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 4.0, 6.9, string('Hg!u2!n seasalt:', string(old_pl_Hg_seasalt/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 4.0, 6.6, string('TOTAL DEPOSITION:', string(old_dep_total/1d3/ofiles,format='(I10.0)')), color=1
   xyouts, 4.0, 6.0, 'REDOX', color=1
   if (chemistry eq 'Br') then begin
   xyouts, 4.0, 5.7, string('Gross Ox by Br:', string(old_pl_Hg_Br/1d3/ofiles,format='(I10.0)')),color=1
   endif else begin
   xyouts, 4.0, 5.7, string('Gross Ox by OH+O!d3!n:', string((old_pl_Hg_OH+old_pl_Hg_O3)/1d3/ofiles,format='(I10.0)')),color=1
   endelse
   xyouts, 4.0, 5.4, string('Gross Reduction:', string(old_pl_Hg_reduction/1d3/ofiles,format='(I10.0)')),color=1
   xyouts, 4.0, 5.1, string('Net Oxidation', string((old_pl_Hg_Br-old_pl_Hg_reduction)/1d3/ofiles,format='(I10.0)')),color=1


   xyouts, 3.0, 9.5, 'NEW MODEL VERSION', color=2
   xyouts, 2.1, 8.7, string(string(new_Hg0masstrop/1d3/nfiles,format='(I10.0)'), '  Mg'), color=2
   xyouts, 2.1, 8.4, string(string(new_Hg2masstrop/1d3/nfiles,format='(I10.0)'), '  Mg'), color=2
   xyouts, 2.1, 8.1, string(string(new_HgPmasstrop/1d3/nfiles,format='(I10.0)'), '  Mg'), color=2
   xyouts, 2.1, 7.2, string(string(new_Hg0ocmass/1d3/nfiles,format='(I10.0)'), '  Mg'), color=2
   xyouts, 2.1, 6.9, string(string(new_Hg2ocmass/1d3/nfiles,format='(I10.0)'), '  Mg'), color=2
   xyouts, 2.1, 6.6, string(string(new_HgCocmass/1d3/nfiles,format='(I10.0)'), '  Mg'), color=2
   xyouts, 2.1, 5.7, string(string(new_em_Hg0_anthro/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 2.1, 5.4, string(string(new_em_Hg2_anthro/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 2.1, 5.1, string(string(new_em_HgP_anthro/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 2.1, 4.8, string(string(new_em_Hg0_geo/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 2.1, 4.5, string(string(new_em_Hg0_soil/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 2.1, 4.2, string(string(new_em_Hg0_BB/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 2.1, 3.9, string(string(new_em_Hg0_land/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 2.1, 3.6, string(string(new_em_Hg0_snow/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 2.7, 3.3, string(string(new_em_Hg0_oc_up/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 3.0, 3.0, string(string(new_em_total/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 5.5, 8.7, string(string(new_Hg0DryD/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 5.5, 8.4, string(string(new_Hg2DryD/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 5.5, 8.1, string(string(new_HgPDryD/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 5.5, 7.8, string(string(new_Hg2Wet/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 5.5, 7.5, string(string(new_HgPWet/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 6.0, 7.2, string(string(new_em_Hg0_oc_dn/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 6.0, 6.9, string(string(new_pl_Hg_seasalt/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   xyouts, 6.6, 6.6, string(string(new_dep_total/1d3/nfiles,format='(I10.0)'), '  Mg/y'), color=2
   if (chemistry eq 'Br') then begin
   xyouts, 6.5, 5.7, string(string(new_pl_Hg_Br/1d3/nfiles,format='(I10.0)'), '  Mg/y'),color=2
   endif else begin
   xyouts, 6.5, 5.7, string(string((new_pl_Hg_OH+new_pl_Hg_O3)/1d3/nfiles,format='(I10.0)'), '  Mg/y'),color=2
   endelse
   xyouts, 6.5, 5.4, string(string(new_pl_Hg_reduction/1d3/nfiles,format='(I10.0)'), '  Mg/y'),color=2
   xyouts, 6.5, 5.1, string(string((new_pl_Hg_Br-new_pl_Hg_reduction)/1d3/nfiles,format='(I10.0)'), '  Mg/y'),color=2


   multipanel, /off


   ps_setup, /close, /landscape


end
