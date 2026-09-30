# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************
'''
Plots a comparison of refractive index data.
'''

from argparse import ArgumentParser
import numpy as np
import matplotlib.pyplot as plt


def load_data(filename):

    wavelengths = []
    nvalues = []
    kvalues = []

    with open(filename, "r") as file:

        data = False
        for line in file:
            line = line.strip()
            if line == "*BEGIN_DATA":
                data = True
                continue
            elif line == "*END":
                break
            elif data:
                columns = line.split()
                wavelengths.append(float(columns[0]))
                nvalues.append(float(columns[1]))
                kvalues.append(float(columns[2]))

    return np.array(wavelengths), np.array(nvalues), np.array(kvalues)


if __name__ == "__main__":

    parser = ArgumentParser(usage="%(prog)s filename [filename ...]")
    parser.add_argument(
        "filename",
        type=str,
        nargs="+",
        help="refractive index data file to plot")
    args = parser.parse_args()

    if len(args.filename) > 7:
        parser.error("maximum number of files is seven")

    fig = plt.figure()
    ax1 = fig.add_subplot(121)
    ax2 = fig.add_subplot(122)
    colours = ["blue", "green", "red", "cyan", "magenta", "orange", "black"]

    for i, filename in enumerate(args.filename):
        wavelengths, nvalues, kvalues = load_data(filename)
        ax1.plot(wavelengths, nvalues, color=colours[i], label=filename)
        ax2.plot(wavelengths, kvalues, color=colours[i], label=filename)

    ax1.set_xscale("log")
    ax1.set_yscale("log")
    ax1.set_title("n")
    plt.legend()

    ax2.set_xscale("log")
    ax2.set_yscale("log")
    ax2.set_title("k")
    plt.tight_layout()
    plt.show()
