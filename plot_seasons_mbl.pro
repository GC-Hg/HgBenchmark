pro plot_seasons_mbl, filename=filename



   tvmap_region, Region=Region, $
          data_mean, xmid, ymid, $
          CRUISETGM, CRUISElon, CRUISElat, t_symbol=1, $
          c_levels=clev, $
          title=PageTitle, /ystyle, $
          margin=[0.015,  0.01, 0.015, 0.02],  $
          csfac=1.15, /continents, Log=Log, $
          /nogx, /nogy, $
          /noadvance, /robinson, /horizon, $
          /cbar, cbposition=[1.02, 0.2, 1.04, 0.8], /vertical, /triangle, $
          unit=unit, $
          _Extra=_Extra

end
