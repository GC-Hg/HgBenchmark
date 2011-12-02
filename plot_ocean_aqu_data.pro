pro plot_ocean_aqu_data, filename=filename

;set-up

DataDir = !BENCHMARK+'/data/'

;get data

DJFfile = DataDir + 'overlay_ocean_new_winter_Hg0.csv'
MAMfile = DataDir + 'overlay_ocean_spring_Hg0.csv'
JJAfile = DataDir + 'overlay_ocean_new_summer_Hg0.csv'
SONfile = DataDir + 'overlay_ocean_new_fall_Hg0.csv'

Array_DJF = reader2( DJFfile, delim=';', dbl=1)
Array_MAM = reader2( MAMfile, delim=';', dbl=1)
Array_JJA = reader2( JJAfile, delim=';', dbl=1)
Array_SON = reader2( SONfile, delim=';', dbl=1)

Hg0_Lon_DJF = Array_DJF[3,*]
Hg0_Lat_DJF = Array_DJF[2,*]
Hg0_DJF = Array_DJF[4,*]
Hg0_Lon_MAM = Array_MAM[3,*]
Hg0_Lat_MAM	= Array_MAM[2,*]
Hg0_MAM = Array_MAM[4,*]
Hg0_Lon_JJA = Array_JJA[3,*]
Hg0_Lat_JJA	= Array_JJA[2,*]
Hg0_JJA = Array_JJA[4,*]
Hg0_Lon_SON = Array_SON[3,*]
Hg0_Lat_SON	= Array_SON[2,*]
Hg0_SON = Array_SON[4,*]

DJFfile = DataDir + 'overlay_ocean_new_winter_Hgtot.csv'
MAMfile = DataDir + 'overlay_ocean_spring_Hgtot.csv'
JJAfile = DataDir + 'overlay_ocean_new_summer_Hgtot.csv'
SONfile = DataDir + 'overlay_ocean_new_fall_Hgtot.csv'

Array_DJF = reader2( DJFfile, delim=';', dbl=1)
Array_MAM = reader2( MAMfile, delim=';', dbl=1)
Array_JJA = reader2( JJAfile, delim=';', dbl=1)
Array_SON = reader2( SONfile, delim=';', dbl=1)

Hgtot_Lon_DJF = Array_DJF[3,*]
Hgtot_Lat_DJF = Array_DJF[2,*]
Hgtot_DJF = Array_DJF[4,*]
Hgtot_Lon_MAM = Array_MAM[3,*]
Hgtot_Lat_MAM = Array_MAM[2,*]
Hgtot_MAM = Array_MAM[4,*]
Hgtot_Lon_JJA = Array_JJA[3,*]
Hgtot_Lat_JJA = Array_JJA[2,*]
Hgtot_JJA = Array_JJA[4,*]
Hgtot_Lon_SON = Array_SON[3,*]
Hgtot_Lat_SON = Array_SON[2,*]
Hgtot_SON = Array_SON[4,*]


;get model 

Hg0_Model_DJF = get_total_ocean_mass( FileName=FileName, $
            Species='hg0aq', Months=[1,2,12], $
            GridInfoOut=GridInfo )
Hg0_Model_MAM = get_total_ocean_mass( FileName=FileName, $
            Species='hg0aq', Months=[3,4,5], $
            GridInfoOut=GridInfo )
Hg0_Model_JJA = get_total_ocean_mass( FileName=FileName, $
            Species='hg0aq', Months=[6,7,8], $
            GridInfoOut=GridInfo )
Hg0_Model_SON = get_total_ocean_mass( FileName=FileName, $
            Species='hg0aq', Months=[9,10,11], $
            GridInfoOut=GridInfo )
Hgtot_Model_DJF = get_total_ocean_mass( FileName=FileName, $
            Species='hgtaq', Months=[1,2,12], $
            GridInfoOut=GridInfo )
Hgtot_Model_MAM = get_total_ocean_mass( FileName=FileName, $
            Species='hgtaq', Months=[3,4,5], $
            GridInfoOut=GridInfo )
Hgtot_Model_JJA = get_total_ocean_mass( FileName=FileName, $
            Species='hgtaq', Months=[6,7,8], $
            GridInfoOut=GridInfo )
Hgtot_Model_SON = get_total_ocean_mass( FileName=FileName, $
            Species='hgtaq', Months=[9,10,11], $
            GridInfoOut=GridInfo )



;convert from kg to kg/m3
Case GridInfo.DJ of
2: mldfile = DataDir + 'MLD_DReqDT.geos.2x25'
4: mldfile = DataDir + 'MLD_DReqDT.geos.4x5'
endcase


s01=ctm_get_datablock(mld01,'bxhght-$', tracer=5, filename=mldfile,tau0=nymd2tau(1985*10000L+0101))
s02=ctm_get_datablock(mld02,'bxhght-$', tracer=5, filename=mldfile,tau0=nymd2tau(1985*10000L+0201))
s03=ctm_get_datablock(mld03,'bxhght-$', tracer=5, filename=mldfile,tau0=nymd2tau(1985*10000L+0301))
s04=ctm_get_datablock(mld04,'bxhght-$', tracer=5, filename=mldfile,tau0=nymd2tau(1985*10000L+0401))
s05=ctm_get_datablock(mld05,'bxhght-$', tracer=5, filename=mldfile,tau0=nymd2tau(1985*10000L+0501))
s06=ctm_get_datablock(mld06,'bxhght-$', tracer=5, filename=mldfile,tau0=nymd2tau(1985*10000L+0601))
s07=ctm_get_datablock(mld07,'bxhght-$', tracer=5, filename=mldfile,tau0=nymd2tau(1985*10000L+0701))
s08=ctm_get_datablock(mld08,'bxhght-$', tracer=5, filename=mldfile,tau0=nymd2tau(1985*10000L+0801))
s09=ctm_get_datablock(mld09,'bxhght-$', tracer=5, filename=mldfile,tau0=nymd2tau(1985*10000L+0901))
s10=ctm_get_datablock(mld10,'bxhght-$', tracer=5, filename=mldfile,tau0=nymd2tau(1985*10000L+1001))
s11=ctm_get_datablock(mld11,'bxhght-$', tracer=5, filename=mldfile,tau0=nymd2tau(1985*10000L+1101))
s12=ctm_get_datablock(mld12,'bxhght-$', tracer=5, filename=mldfile,tau0=nymd2tau(1985*10000L+1201))

MLD_DJF = (mld12 + mld01 + mld02) / 3d0
MLD_MAM	= (mld03 + mld04 + mld05) / 3d0
MLD_JJA	= (mld06 + mld07 + mld08) / 3d0
MLD_SON	= (mld09 + mld10 + mld11) / 3d0

F_Land = get_ctm_landfraction(GridInfo)
F_Ocean = 1D0 - F_Land
Area = ctm_boxsize(Gridinfo, /m2)

s=size(mld01)
V_DJF=fltarr(s[1],s[2])
V_MAM=fltarr(s[1],s[2])
V_JJA=fltarr(s[1],s[2])
V_SON=fltarr(s[1],s[2])

;get volume of grid boxes
ok = where(F_Ocean gt 0 and MLD_DJF gt 0)
V_DJF(ok) = MLD_DJF(ok) * Area(ok) * F_Ocean(ok)
ok = where(F_Ocean gt 0 and MLD_MAM gt 0)
V_MAM(ok) = MLD_MAM(ok)	* Area(ok) * F_Ocean(ok)
ok = where(F_Ocean gt 0 and MLD_JJA gt 0)
V_JJA(ok) = MLD_JJA(ok)	* Area(ok) * F_Ocean(ok)
ok = where(F_Ocean gt 0 and MLD_SON gt 0)
V_SON(ok) = MLD_SON(ok)	* Area(ok) * F_Ocean(ok)

;divide model mass by box volume

Hg0_kgm3_DJF = fltarr(s[1],s[2])
Hg0_kgm3_MAM = fltarr(s[1],s[2])
Hg0_kgm3_JJA = fltarr(s[1],s[2])
Hg0_kgm3_SON = fltarr(s[1],s[2])
Hgtot_kgm3_DJF = fltarr(s[1],s[2])
Hgtot_kgm3_MAM = fltarr(s[1],s[2])
Hgtot_kgm3_JJA = fltarr(s[1],s[2])
Hgtot_kgm3_SON = fltarr(s[1],s[2])

ok2 = where(V_DJF gt 0)
Hg0_kgm3_DJF(ok2) = Hg0_model_DJF(ok2)/V_DJF(ok2)
ok2 = where(V_MAM gt 0)
Hg0_kgm3_MAM(ok2) = Hg0_model_MAM(ok2)/V_MAM(ok2)
ok2 = where(V_JJA gt 0)
Hg0_kgm3_JJA(ok2) = Hg0_model_JJA(ok2)/V_JJA(ok2)
ok2 = where(V_SON gt 0)
Hg0_kgm3_SON(ok2) = Hg0_model_SON(ok2)/V_SON(ok2)
ok2 = where(V_DJF gt 0)
Hgtot_kgm3_DJF(ok2) = Hgtot_model_DJF(ok2)/V_DJF(ok2)
ok2 = where(V_MAM gt 0)
Hgtot_kgm3_MAM(ok2) = Hgtot_model_MAM(ok2)/V_MAM(ok2)
ok2 = where(V_JJA gt 0)
Hgtot_kgm3_JJA(ok2) = Hgtot_model_JJA(ok2)/V_JJA(ok2)
ok2 = where(V_SON gt 0)
Hgtot_kgm3_SON(ok2) = Hgtot_model_SON(ok2)/V_SON(ok2)


;convert kg/m3 to picomoles/liter
;(kg/m3)*(1e3g/kg)*(1m/1e2cm)^3*(1mol/200.59g)
;(1cm3/1cm)*(1e3ml/1l)*(1e12pM/1M)
;-->1e12/200.59
;1000L/m3, 1D12 pM/M, 200.59 g/mol, 1000g/kg
Hg0_pM_DJF = (Hg0_kgm3_DJF*1D12)/(200.59D0)
Hg0_pM_MAM = (Hg0_kgm3_MAM*1D12)/(200.59D0)
Hg0_pM_JJA = (Hg0_kgm3_JJA*1D12)/(200.59D0)
Hg0_pM_SON = (Hg0_kgm3_SON*1D12)/(200.59D0)
Hgtot_pM_DJF = (Hgtot_kgm3_DJF*1D12)/(200.59D0)
Hgtot_pM_MAM = (Hgtot_kgm3_MAM*1D12)/(200.59D0)
Hgtot_pM_JJA = (Hgtot_kgm3_JJA*1D12)/(200.59D0)
Hgtot_pM_SON = (Hgtot_kgm3_SON*1D12)/(200.59D0)

;plot

xmid = GridInfo.xmid
ymid = GridInfo.ymid

Region = ''
Log = 0L
unit = 'pM'

hg0mn = 0
hg0mx = 0.3
hgTmn = 0
hgTmx = 2.5

multipanel, rows=2, cols=2

myct, /whgrylrd, ncolors=17

PageTitle='Ocean Hg(0) December - February'

tvmap_region, Region=Region, $
  Hg0_pM_DJF, xmid, ymid, $
  Hg0_DJF, Hg0_Lon_DJF, Hg0_Lat_DJF, t_symbol=1, $
  title=PageTitle, /ystyle, $
  csfac=1, tcsfac=1, /continents, Log=Log, $
  /nogx, /nogy, /iso, $
  /robinson, /horizon, $
  /cbar, div=5, /triangle, $
  mindata=hg0mn, maxdata=hg0mx, $
  unit=unit, /cfill, botoutofrange=!myct.bottom, $
  _Extra=_Extra

PageTitle='Ocean Hg(0) March - May'

tvmap_region, Region=Region, $
  Hg0_pM_MAM, xmid, ymid, $
  Hg0_MAM, Hg0_Lon_MAM, Hg0_Lat_MAM, t_symbol=1, $
  title=PageTitle, /ystyle, $
  csfac=1, tcsfac=1, /continents, Log=Log, $
  /nogx, /nogy, /iso, $
  /robinson, /horizon, $
  /cbar, div=5, /triangle, $
  mindata=hg0mn, maxdata=hg0mx, $
  unit=unit, /cfill, botoutofrange=!myct.bottom, $
  _Extra=_Extra

PageTitle='Ocean Hg(0) June - August'

tvmap_region, Region=Region, $
  Hg0_pM_JJA, xmid, ymid, $
  Hg0_JJA, Hg0_Lon_JJA, Hg0_Lat_JJA, t_symbol=1, $
  title=PageTitle, /ystyle, $
  csfac=1, tcsfac=1, /continents, Log=Log, $
  /nogx, /nogy, /iso, $
  /robinson, /horizon, $
  /cbar, div=5, /triangle, $
  mindata=hg0mn, maxdata=hg0mx, $
  unit=unit, /cfill, botoutofrange=!myct.bottom, $
  _Extra=_Extra

PageTitle='Ocean Hg(0) September - November'

tvmap_region, Region=Region, $
  Hg0_pM_SON, xmid, ymid, $
  Hg0_SON, Hg0_Lon_SON, Hg0_Lat_SON, t_symbol=1, $
  title=PageTitle, /ystyle, $
  csfac=1, tcsfac=1, /continents, Log=Log, $
  /nogx, /nogy, $
  /robinson, /horizon, $
  /cbar, div=5, /triangle, $
  unit=unit, /cfill, botoutofrange=!myct.bottom, $
  mindata=hg0mn, maxdata=hg0mx, $
  _Extra=_Extra

multipanel, /off

multipanel, rows=2, cols=2

myct, /whgrylrd, ncolors=17

PageTitle='Ocean Total Hg December - February'

tvmap_region, Region=Region, $
  Hgtot_pM_DJF, xmid, ymid, $
  Hgtot_DJF, Hgtot_Lon_DJF, Hgtot_Lat_DJF, t_symbol=1, $
  title=PageTitle, /ystyle, $
  csfac=1, tcsfac=1, /continents, Log=Log, $
  /nogx, /nogy, /iso, $
  /robinson, /horizon, $
  /cbar, div=5, /triangle, $
  unit=unit, /cfill, botoutofrange=!myct.bottom, $
  mindata=hgTmn, maxdata=hgTmx, $
  _Extra=_Extra

PageTitle='Ocean Total Hg March - May'

tvmap_region, Region=Region, $
  Hgtot_pM_MAM, xmid, ymid, $
  Hgtot_MAM, Hgtot_Lon_MAM, Hgtot_Lat_MAM, t_symbol=1, $
  title=PageTitle, /ystyle, $
  csfac=1, tcsfac=1, /continents, Log=Log, $
  /nogx, /nogy, /iso, $
  /robinson, /horizon, $
  /cbar, div=5, /triangle, $
  unit=unit, /cfill, botoutofrange=!myct.bottom, $
  mindata=hgTmn, maxdata=hgTmx, $
  _Extra=_Extra

PageTitle='Ocean Total Hg June - August'

tvmap_region, Region=Region, $
  Hgtot_pM_JJA, xmid, ymid, $
  Hgtot_JJA, Hgtot_Lon_JJA, Hgtot_Lat_JJA, t_symbol=1, $
  title=PageTitle, /ystyle, $
  csfac=1, tcsfac=1, /continents, Log=Log, $
  /nogx, /nogy, /iso, $
  /robinson, /horizon, $
  /cbar, div=5, /triangle, $
  unit=unit, /cfill, botoutofrange=!myct.bottom, $
  mindata=hgTmn, maxdata=hgTmx
  _Extra=_Extra

PageTitle='Ocean Total Hg September - November'

tvmap_region, Region=Region, $
  Hgtot_pM_SON, xmid, ymid, $
  Hgtot_SON, Hgtot_Lon_SON, Hgtot_Lat_SON, t_symbol=1, $
  title=PageTitle, /ystyle, $
  csfac=1, tcsfac=1, /continents, Log=Log, $
  /nogx, /nogy, /iso, $
  /robinson, /horizon, $
  /cbar, div=5, /triangle, $
  unit=unit, /cfill, botoutofrange=!myct.bottom, $
  mindata=hgTmn, maxdata=hgTmx
  _Extra=_Extra


multipanel, /off


end
