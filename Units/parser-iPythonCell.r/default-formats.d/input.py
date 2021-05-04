# Derived from https://github.com/universal-ctags/ctags/issues/2978
# submitted by @gerazov.
import numpy as np
from matplotlib import pyplot as plt
from scipy.io import wavfile
import os

# %% generate sound
f = 12000
fs = 44100
t = np.arange(0, 1, 1/fs)
sound = np.sin(2*np.pi*f * t)

# %% plot sound
plt.plot(t, sound)

# %% play sound
wavfile.write('sound.wav', fs, np.int16(sound * 2**15))
os.system('play sound.wav')

#%% no space
a=1

# %%% extra percent chars with space
b=1

#%%% extra percent chars without space
c=1

def f():
    #%% no space with prefix
    pass

def g():
    # %%% extra percent chars with space and prefix
    pass

def h():
    #%%% extra percent chars without space with prefix
    pass

# <codecell>triangle without space
A=1

# <codecell> triangle with space
B=1

def F():
    # <codecell> triangle with space and prefix
    pass

## IGNORE ME

