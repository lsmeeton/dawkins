import numpy as np
import matplotlib.pyplot as plt
import itertools as it

import argparse

parser = argparse.ArgumentParser(description="arguments for reading an rdf .dat file and plotting a graph")

parser.add_argument('--signalLength',
                    type=int,
                    default=1024,
                    dest='signalLength',
                    help='Number of sample points in signal')

parser.add_argument('--stepLength',
                    type=int,
                    default=128,
                    dest='stepLength',
                    help='Number of steps in signal')

parser.add_argument('--maxHeight',
                    type=float,
                    default=1.0,
                    dest='maxHeight',
                    help='Max amplitude of signal')

parser.add_argument('--noiseParameter',
                    type=float,
                    default=0.05,
                    dest='noiseParameter',
                    help='Control parameter for random gaussian noise')

parser.add_argument('-o',
                    type=str,
                    default=None,
                    dest='outputFileName',
                    help='Location of output file name. Default writes to console.')


args = parser.parse_args()


signalLength = args.signalLength
stepLength = args.stepLength
steps = signalLength // stepLength
maxHeight = args.maxHeight
noiseParameter = args.noiseParameter
outputFileName = args.outputFileName

# Generate a noise-free random staircase signal

signal = np.zeros(signalLength)

for step in range(steps):
    amp = maxHeight * (np.random.random() - 0.5)
    for element in range(stepLength):
        signal[element + (step * stepLength)] = amp

# Generate noise
noise = np.random.normal(loc = 0, scale = noiseParameter, size = (signalLength))

plt.plot(np.array([i for i in range(signalLength)]),signal,
         np.array([i for i in range(signalLength)]),signal + noise)
plt.show()

# Write to file
s = "Data, Signal\n"
s += "".join(["%2.6f, %2.6f\n"%(signal,noisey_signal) for signal, noisey_signal in it.zip_longest(signal, signal + noise)])
              
if outputFileName:
    with open(outputFileName, 'w') as f:
        f.write(s)
else:
    print(s)






