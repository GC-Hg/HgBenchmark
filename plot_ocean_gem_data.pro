pro plot_ocean_gem_data, filename=filename, ppq=ppq

;eds 5/12/11 script to plot ocean data for
;mercury benchmarking, based on anls routines

;set-up

DataDir = !BENCHMARK+'/data/'

;conversion factor
pptv_ngm3 = 8.93D0
ngm3_ppqv = 112D0

;get data

DJFfile = DataDir + 'overlay_ocean_GEM_DJF.csv'
MAMfile = DataDir + 'overlay_ocean_GEM_MAM.csv'
JJAfile = DataDir + 'overlay_ocean_GEM_JJA.csv'
SONfile = DataDir + 'overlay_ocean_GEM_SON.csv'

Array_DJF = reader2( DJFfile, delim=';', dbl=1)
Array_MAM = reader2( MAMfile, delim=';', dbl=1)
Array_JJA = reader2( JJAfile, delim=';', dbl=1)
Array_SON = reader2( SONfile, delim=';', dbl=1)

Lon_DJF = Array_DJF[3,*]
Lat_DJF = Array_DJF[2,*]
Data_DJF = Array_DJF[4,*]
Lon_MAM = Array_MAM[3,*]
Lat_MAM	= Array_MAM[2,*]
Data_MAM = Array_MAM[4,*]
Lon_JJA = Array_JJA[3,*]
Lat_JJA	= Array_JJA[2,*]
Data_JJA = Array_JJA[4,*]
Lon_SON = Array_SON[3,*]
Lat_SON	= Array_SON[2,*]
Data_SON = Array_SON[4,*]

if (keyword_set(ppq)) then begin
   ;convert ng/m3 -> ppqv
   Data_DJF=Data_DJF*ngm3_ppqv
   Data_MAM=Data_MAM*ngm3_ppqv
   Data_JJA=Data_JJA*ngm3_ppqv
   Data_SON=Data_SON*ngm3_ppqv
endif

;get model 

Model_DJF = get_mean_concentration( FileName=FileName, $
            Species='Hg0', Months=[1,2,12], $
            GridInfoOut=GridInfo )
Model_MAM = get_mean_concentration( FileName=FileName, $
            Species='Hg0', Months=[3,4,5], $
            GridInfoOut=GridInfo )
Model_JJA = get_mean_concentration( FileName=FileName, $
            Species='Hg0', Months=[6,7,8], $
            GridInfoOut=GridInfo )
Model_SON = get_mean_concentration( FileName=FileName, $
            Species='Hg0', Months=[9,10,11], $
            GridInfoOut=GridInfo )

;surface level only
if keyword_set(ppq) then begin
   ;convert pptv -> ppqv
   Model_DJF = Model_DJF[*,*,0]*1d3
   Model_MAM = Model_MAM[*,*,0]*1d3
   Model_JJA = Model_JJA[*,*,0]*1d3
   Model_SON = Model_SON[*,*,0]*1d3
endif else begin
   ;convert pptv -> ng/m3
   Model_DJF = Model_DJF[*,*,0]*pptv_ngm3
   Model_MAM = Model_MAM[*,*,0]*pptv_ngm3
   Model_JJA = Model_JJA[*,*,0]*pptv_ngm3
   Model_SON = Model_SON[*,*,0]*pptv_ngm3
endelse

;plot

xmid = GridInfo.xmid
ymid = GridInfo.ymid

Region = ''
Log = 0L

if (keyword_set(ppq)) then begin
   clev = [maken(100, 200, 11), maken(250, 500, 6)]
   unit = 'ppqv'
endif else begin
   clev = [maken(0.75, 1.75, 11), maken(2.0, 3.5, 6)]
   unit = 'ng/m!u3!n'
endelse

multipanel, rows=2, cols=2

myct, 33, ncolors=17

PageTitle='MBL Hg(0) December - February'

tvmap_region, Region=Region, $
  Model_DJF, xmid, ymid, $
  Data_DJF, Lon_DJF, Lat_DJF, t_symbol=1, $
  c_levels=clev, $
  title=PageTitle, /ystyle, $
  csfac=0.65, tcsfac=1.5, /continents, Log=Log, $
  /nogx, /nogy, /iso, $
  /robinson, /horizon, $
  /cbar, div=5, /triangle, $
  unit=unit, botoutofrange=!myct.bottom, $
  _Extra=_Extra

PageTitle='MBL Hg(0) March - May'

tvmap_region, Region=Region, $
  Model_MAM, xmid, ymid, $
  Data_MAM, Lon_MAM, Lat_MAM, t_symbol=1, $
  c_levels=clev, $
  title=PageTitle, /ystyle, $
  csfac=0.65, tcsfac=1.5, /continents, Log=Log, $
  /nogx, /nogy, /iso, $
  /robinson, /horizon, $
  /cbar, div=5, /triangle, $
  unit=unit, botoutofrange=!myct.bottom, $
  _Extra=_Extra

PageTitle='MBL Hg(0) June - August'

tvmap_region, Region=Region, $
  Model_JJA, xmid, ymid, $
  Data_JJA, Lon_JJA, Lat_JJA, t_symbol=1, $
  c_levels=clev, $
  title=PageTitle, /ystyle, $
  csfac=0.65, tcsfac=1.5, /continents, Log=Log, $
  /nogx, /nogy, /iso, $
  /robinson, /horizon, $
  /cbar, div=5, /triangle, $
  unit=unit, botoutofrange=!myct.bottom, $
  _Extra=_Extra

PageTitle='MBL Hg(0) September - November'

tvmap_region, Region=Region, $
  Model_SON, xmid, ymid, $
  Data_SON, Lon_SON, Lat_SON, t_symbol=1, $
  c_levels=clev, $
  title=PageTitle, /ystyle, $
  csfac=0.65, tcsfac=1.5, /continents, Log=Log, $
  /nogx, /nogy, /iso, $
  /robinson, /horizon, $
  /cbar, div=5, /triangle, $
  unit=unit, botoutofrange=!myct.bottom, $
  _Extra=_Extra

multipanel, /off

end
