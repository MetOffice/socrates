# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************
'''
Plots a Socrates format solar spectrum against the solar spectrum
resolved by a spectral file.
'''

import sys
import numpy as np
import matplotlib.pyplot as plt
import re

if __name__ == '__main__':
    if (len(sys.argv) > 2):
        filename1 = sys.argv[1]
        filename2 = sys.argv[2]
    elif (len(sys.argv) > 1):
        filename1 = sys.argv[1]
    else:
        raise RuntimeError('please enter a solar spectrum file name')

l_binned = False
linenum = 0
scale_wv=1.0
scale_irr=1.0
with open(filename1, 'r') as file:
    for line in file:
        linenum += 1
        if (line.find("*SCALE_DATA") == 0):
            cols=file.readline().strip().split()
            scale_wv=float(cols[0])
            scale_irr=float(cols[1])
        if (line.find("*BEGIN_DATA") == 0):
            datastart = linenum
        if (line.find("*BEGIN_BINNED_DATA") == 0):
            datastart = linenum
            l_binned = True
        if (line.find("*END") == 0):
            dataend = linenum

datalen = dataend-datastart-1
if (l_binned):
    wavelength1 = np.zeros(datalen*2)
    irradiance1 = np.zeros(datalen*2)
else:
    wavelength1 = np.zeros(datalen)
    irradiance1 = np.zeros(datalen)

total_irradiance=0.0
with open(filename1) as file:
    for line in file:
        if (line.find("*BEGIN_DATA") == 0):
            total_irradiance=1361.0
            for n in range(0, datalen):
                cols=file.readline().strip().split()
                wavelength1[n]=float(cols[0])*scale_wv
                irradiance1[n]=float(cols[1])*scale_irr
        if (line.find("*BEGIN_BINNED_DATA") == 0):
            for n in range(0, datalen):
                cols=file.readline().strip().split()
                wavelength1[n*2]=float(cols[0])*scale_wv
                wavelength1[n*2+1]=float(cols[1])*scale_wv
                irradiance1[n*2]=float(cols[2])*scale_irr
                irradiance1[n*2+1]=irradiance1[n*2]
                total_irradiance += irradiance1[n*2]*(wavelength1[n*2+1]-wavelength1[n*2])

wavelength1 = wavelength1*1.0e9

l_sub_bands = False
if (len(sys.argv) > 2):
    with open(filename2, 'r') as file:
        for line in file:
            if (line.find("Number of spectral bands =") == 0):
                cols=line.strip().split()
                datalen=int(cols[5])
            if (line.find("Number of spectral sub-bands =") == 0):
                cols=line.strip().split()
                datalen=int(cols[5])
                l_sub_bands = True
    
    wavelength2 = np.zeros(datalen*2)
    irradiance2 = np.zeros(datalen*2)
    
    if (l_sub_bands):
        band = np.zeros(datalen, dtype=int)
        kterm = np.zeros(datalen, dtype=int)
        with open(filename2) as file:
            for line in file:
                if (line.find("*BLOCK: TYPE =   17:") == 0):
                    file.readline()
                    file.readline()
                    file.readline()
                    file.readline()
                    for n in range(0, datalen):
                        cols=file.readline().strip().split()
                        band[n]=int(cols[1])
                        kterm[n]=int(cols[2])
                        wavelength2[n*2]=float(cols[3])
                        wavelength2[n*2+1]=float(cols[4])
        with open(filename2) as file:
            wavelength3 = np.zeros(max(band)*2)
            for line in file:
                if (line.find("*BLOCK: TYPE =    1:") == 0):
                    file.readline()
                    file.readline()
                    file.readline()
                    for n in range(0, max(band)):
                        cols=file.readline().strip().split()
                        wavelength3[n*2]=float(cols[1])
                        wavelength3[n*2+1]=float(cols[2])
                if (line.find("*BLOCK: TYPE =    2:") == 0):
                    file.readline()
                    file.readline()
                    for b in range(0, max(band)):
                        cols=file.readline().strip().split()
                        for n in range(0, datalen):
                            if (band[n] == b+1):
                                irradiance2[n*2]=float(cols[1])*total_irradiance/(wavelength2[n*2+1]-wavelength2[n*2])
                                irradiance2[n*2+1]=irradiance2[n*2]

        with open(filename2+'_k') as file_k:
            for line_k in file_k:
                if (line_k.find("*BLOCK: sub-band mapping") == 0):
                    test_end=file_k.readline()
                    cols_k=file_k.readline().strip().replace(',',' ').split()
                    file_k.readline()
                    for n in range(0, datalen):
                        if (test_end.find("*END") == 0):
                            break
                        while (int(cols_k[1]) < band[n]):
                            for k in range(0, int(cols_k[5])+1):
                                test_end=file_k.readline()
                            if (test_end.find("*END") == 0):
                                break
                            cols_k=file_k.readline().strip().replace(',',' ').split()
                            file_k.readline()
                        if (kterm[n] > 0):
                            sub_band_data=file_k.readline().strip().split()
                            irradiance2[n*2]=irradiance2[n*2]*float(sub_band_data[2])
                            irradiance2[n*2+1]=irradiance2[n*2]
                            if (int(sub_band_data[0]) == int(cols_k[5])):
                                test_end=file_k.readline()
                                cols_k=file_k.readline().strip().replace(',',' ').split()
                                file_k.readline()

    else:
        with open(filename2) as file:
            for line in file:
                if (line.find("*BLOCK: TYPE =    1:") == 0):
                    file.readline()
                    file.readline()
                    file.readline()
                    for n in range(0, datalen):
                        cols=file.readline().strip().split()
                        wavelength2[n*2]=float(cols[1])
                        wavelength2[n*2+1]=float(cols[2])
                if (line.find("*BLOCK: TYPE =    2:") == 0):
                    file.readline()
                    file.readline()
                    for n in range(0, datalen):
                        cols=file.readline().strip().split()
                        irradiance2[n*2]=float(cols[1])*total_irradiance/(wavelength2[n*2+1]-wavelength2[n*2])
                        irradiance2[n*2+1]=irradiance2[n*2]

    wavelength2 = wavelength2*1.0e9

fig=plt.figure(figsize=(10,6))
ax = fig.add_subplot(111)
ax.plot(wavelength1,irradiance1, color='blue')
if (len(sys.argv) > 2):
    ax.plot(wavelength2,irradiance2, color='green')
ax.set_xlabel('Wavelength (nm)')
ax.set_ylabel('Irradiance (W m$^{-2}$ m$^{-1}$)')
#ax.set_xlim([1, 1830])
ax.set_xlim([0, 500])
ax.set_ylim([1e2, 1e10])
#ax.set_ylim([None, None])
ax.set_yscale('log')
ax.set_title('Solar spectrum')
#leg=plt.legend()
#leg.set_draggable(True)
plt.tight_layout()
plt.show()
