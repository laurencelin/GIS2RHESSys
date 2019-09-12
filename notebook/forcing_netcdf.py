import pandas as pd
from netCDF4 import Dataset, date2num
from datetime import datetime, timedelta
import numpy as np
import os


class ForcingNetCDF:

    def __init__(self):
        print("Start to create NetCDF from forcing")


    def nc_list(self):
        output_path = os.getcwd() + '/input_files'
        name_list = os.listdir(output_path)
        full_list = [os.path.join(output_path, i) for i in name_list]
        run_suffix = '.nc'
        output_list = [x for x in full_list if run_suffix in x]
        return output_list

    def forcing_t(self, output_list, variable_lists, start_date, end_date):
        df = pd.DataFrame()
        for i, nc in enumerate(output_list):
            value_list = []
            for variable in variable_lists:
                value = Dataset(nc)[variable][:].mean()
                value_list.append(value)
            df[i] = value_list
        df.index = variable_lists
        df = df.T
        date_rng = pd.date_range(start=start_date, end=end_date, freq='H')
        date = date_rng[0:len(date_rng) - 1]
        df = df.set_index(date, "date")
        return df

    def add_wind(self, df):
        wind_speed = df['UGRD10m_110_HTGL'].values ** 2 + df['VGRD10m_110_HTGL'].values ** 2
        wind_speed_f = np.sqrt(wind_speed)
        df['wind_speed'] = wind_speed_f.tolist()
        return df

    def create_new_netcdf(self, file_name):
        # Create a netCDF file
        forcing_data = Dataset(file_name, "w", format="NETCDF3_CLASSIC")
        return forcing_data

    def write_attri(self, nc):
        # Create attributes in a netCDF file
        nc.dataset_orig_path = ""
        nc.history = ""
        nc.NCO = "4.6.0"

    def write_dim(self, nc, hru_num):
        # Create Dimension in a netCDF file
        hru = nc.createDimension("hru", hru_num)
        time = nc.createDimension("time", None)

    def write_hruid(self, nc, hru_id):
        hruId = nc.createVariable("hruId", "i4", ("hru",))
        hruId[:] = hru_id

    def write_latlon(self, nc, rd_lat, rd_lon):
        latitude = nc.createVariable("latitude", "f8", ("hru",))
        latitude[:] = rd_lat
        longitude = nc.createVariable("longitude", "f8", ("hru",))
        longitude[:] = rd_lon

    def write_datastep(self, nc, data_step='3600.0'):
        data_step = nc.createVariable("data_step", "f8", )
        data_step.units = 'seconds'
        data_step.long_name = "data step length in seconds"
        data_step[:] = data_step

    def write_sim_time(self, nc, df, start_date_time):
        time = nc.createVariable("time", "f8", ("time"))
        time.units = 'days since ' + start_date_time + ':00'
        time.long_name = "Observation time"
        time.calendar = "standard"
        time.units = 'days since ' + start_date_time + ':00'
        dates = []
        for n in range(len(df)):
            dates.append(datetime(int(start_date_time[0:4]), int(start_date_time[5:7]), int(start_date_time[8:10])) + n * timedelta(hours=1))
        time[:] = date2num(dates, units=time.units, calendar=time.calendar)

    def insert_LWRadAtm(self, hru_id, nc, df, name='DLWRFsfc_110_SFC'):
        LWRadAtm = nc.createVariable("LWRadAtm", "f8", ("time", "hru",), fill_value=-999.0)
        LWRadAtm.units = 'W m-2'
        LWRadAtm.long_name = "downward longwave radiation at the upper boundary"
        LWRadAtm.v_type = 'scalarv'
        if len(hru_id) == 1:
            LWRadAtm[:] = df[[name]].values
        elif len(hru_id) > 1:
            for i, value in enumerate(hru_id):
                LWRadAtm[:, i] = df[[name]].values
        else:
            raise ValueError('The number of HRUs defines greater or equal than 1')

    def insert_SWRadAtm(self, hru_id, nc, df, name='DSWRFsfc_110_SFC'):
        SWRadAtm = nc.createVariable("SWRadAtm", "f8", ("time", "hru",), fill_value=-999.0)
        SWRadAtm.units = 'W m-2'
        SWRadAtm.long_name = "downward shortwave radiation at the upper boundary"
        SWRadAtm.v_type = 'scalarv'
        if len(hru_id) == 1:
            SWRadAtm[:] = df[[name]].values
        elif len(hru_id) > 1:
            for i, value in enumerate(hru_id):
                SWRadAtm[:, i] = df[[name]].values
        else:
            raise ValueError('The number of HRUs defines greater or equal than 1')

    def insert_airpres(self, hru_id, nc, df, name='PRESsfc_110_SFC'):
        airpres = nc.createVariable("airpres", "f8", ("time", "hru",), fill_value=-999.0)
        airpres.units = 'Pa'
        airpres.long_name = "air pressure at the measurement height"
        airpres.v_type = 'scalarv'
        if len(hru_id) == 1:
            airpres[:] = df[[name]].values
        elif len(hru_id) > 1:
            for i, value in enumerate(hru_id):
                airpres[:, i] = df[[name]].values
        else:
            raise ValueError('The number of HRUs defines greater or equal than 1')

    def insert_airtemp(self, hru_id, nc, df, name='TMP2m_110_HTGL'):
        airtemp = nc.createVariable("airtemp", "f8", ("time", "hru",), fill_value=-999.0)
        airtemp.units = 'K'
        airtemp.long_name = "air temperature at the measurement height"
        airtemp.v_type = 'scalarv'
        if len(hru_id) == 1:
            airtemp[:] = df[[name]].values
        elif len(hru_id) > 1:
            for i, value in enumerate(hru_id):
                airtemp[:, i] = df[[name]].values
        else:
            raise ValueError('The number of HRUs defines greater or equal than 1')

    def insert_pptrate(self, hru_id, nc, df, name='APCPsfc_110_SFC_acc1h'):
        pptrate = nc.createVariable("pptrate", "f8", ("time", "hru",), fill_value=-999.0)
        pptrate.units = 'kg m-2 s-1'
        pptrate.long_name = "Precipitation rate"
        pptrate.v_type = 'scalarv'
        if len(hru_id) == 1:
            pptrate[:] = df[[name]].values
        elif len(hru_id) > 1:
            for i, value in enumerate(hru_id):
                pptrate[:, i] = df[[name]].values
        else:
            raise ValueError('The number of HRUs defines greater or equal than 1')

    def insert_spechum(self, hru_id, nc, df, name='SPFH2m_110_HTGL'):
        spechum = nc.createVariable("spechum", "f8", ("time", "hru",), fill_value=-999.0)
        spechum.units = 'g g-1'
        spechum.long_name = "specific humidity at the measurement height"
        spechum.v_type = 'scalarv'
        if len(hru_id) == 1:
            spechum[:] = df[[name]].values
        elif len(hru_id) > 1:
            for i, value in enumerate(hru_id):
                spechum[:, i] = df[[name]].values
        else:
            raise ValueError('The number of HRUs defines greater or equal than 1')

    def insert_windspd(self, hru_id, nc, df, name='wind_speed'):
        windspd = nc.createVariable("windspd", "f8", ("time", "hru",), fill_value=-999.0)
        windspd.units = 'm s-1'
        windspd.long_name = "wind speed at the measurement height"
        windspd.v_type = 'scalarv'
        if len(hru_id) == 1:
            windspd[:] = df[[name]].values
        elif len(hru_id) > 1:
            for i, value in enumerate(hru_id):
                windspd[:, i] = df[[name]].values
        else:
            raise ValueError('The number of HRUs defines greater or equal than 1')