# %% 0. Import necessary packages and functions 

import numpy as np
import scipy as sp
from scipy import optimize
from scipy.stats import norm
import matplotlib.pyplot as plt 
from statistics import NormalDist
# 
def gauss(x, A, x0, sigma):
    return A * np.exp(-(x - x0) ** 2 / (2 * sigma ** 2))

def Gauss(x, A, B):
    y = A*np.exp(-1*B*x**2)
    return y

def bimodal(x,A1,mu1,sigma1,A2,mu2,sigma2):
    return gauss(x, A1, mu1, sigma1) + gauss(x, A2, mu2, sigma2)


# %% 1. make bimodal distribution for L and R 

# make VOTstep (from 0 to 13, or 1 to 14)
VOTsteps = np.arange(0,14,1)
#center it (just in case i need this)
cVOTsteps = VOTsteps - np.mean(VOTsteps)
#make L and R distributions
Ldistribution = np.array([0,1,5,10,5,2,2,5,10,5,2,1,1,0])
Rdistribution = np.array([0,1,1,2,5,10,5,2,2,5,10,5,1,0])
# multiply by 6 for all stim 
Ldistribution = Ldistribution*6
Rdistribution = Rdistribution*6
numstim = sum(Ldistribution)
#we're fitting one category at a time, so we need to eliminate one category
flatcategory = np.array([0,0,0,0,0,0,0,0])

#plot full distributions to make sure it looks right!  
plt.plot(VOTsteps,Ldistribution)
plt.plot(VOTsteps,Rdistribution)
# %% make gaussian for each category

#Left /b/ category
Ldistb = Ldistribution[np.arange(0,6,1)] 
Ldistb = np.append(Ldistb,flatcategory)
#Left /p/ category
Ldistp = Ldistribution[np.arange(6,14,1)]
Ldistp = np.append(flatcategory[np.arange(0,6,1)],Ldistp)
# Right /b/ category
Rdistb = Rdistribution[np.arange(0,8,1)] 
Rdistb = np.append(Rdistb,flatcategory[np.arange(0,6,1)])
# Right /p/ category
Rdistp = Rdistribution[np.arange(8,14,1)]
Rdistp = np.append(flatcategory,Rdistp)

# %% 1a. fit gaussian to each mode

# get mean and std of each category 
LBmu = sum(VOTsteps * Ldistb) / sum(Ldistb)         
LBsigma = np.sqrt(sum(Ldistb*(VOTsteps-LBmu)**2)/sum(Ldistb)) 
LPmu = sum(VOTsteps * Ldistp) / sum(Ldistp)   
LPsigma = np.sqrt(sum(Ldistp*(VOTsteps-LPmu)**2)/sum(Ldistp)) 

# curve fit each category (L distribution)
LBparam, LBcov = optimize.curve_fit(gauss,VOTsteps, Ldistb, bounds = (0, np.inf), p0=[max(Ldistb), LBmu, LBsigma])
LPparam, LPcov = optimize.curve_fit(gauss,VOTsteps, Ldistp, bounds = (0, np.inf), p0=[max(Ldistp), LPmu, LPsigma])

#plot to make sure it looks right! 
plt.plot(VOTsteps,Ldistb, 'b-')
plt.plot(VOTsteps,gauss(VOTsteps,LBparam[0], LBparam[1], LBparam[2]), 'c--')
plt.plot(VOTsteps,Ldistp, 'r-')
plt.plot(VOTsteps,gauss(VOTsteps,LPparam[0], LPparam[1], LPparam[2]), 'r--')
# %% right side now 
RBmu = sum(VOTsteps * Rdistb) / sum(Rdistb)
RBsigma = np.sqrt(sum(Rdistb*(VOTsteps-RBmu)**2)/sum(Rdistb))
RPmu = sum(VOTsteps * Rdistp) / sum(Rdistp)
RPsigma = np.sqrt(sum(Rdistp*(VOTsteps-RPmu)**2)/sum(Rdistp))

# curve fit each category (R distribution)
RBparam, RBcov = optimize.curve_fit(gauss,VOTsteps, Rdistb, bounds = (0, np.inf), p0=[max(Ldistb), RBmu, RBsigma])
RPparam, RPcov = optimize.curve_fit(gauss,VOTsteps, Rdistp, bounds = (0, np.inf), p0=[max(Ldistp), RPmu, RPsigma])

plt.plot(VOTsteps,Rdistb, 'b-')
plt.plot(VOTsteps,gauss(VOTsteps,RBparam[0], RBparam[1], RBparam[2]), 'c--')
plt.plot(VOTsteps,Rdistp, 'r-')
plt.plot(VOTsteps,gauss(VOTsteps,RPparam[0], RPparam[1], RPparam[2]), 'r--')

# %% 1b. sum the two gaussians 

Lbimodal = bimodal(VOTsteps,LBparam[0],LBparam[1],LBparam[2],LPparam[0],LPparam[1],LPparam[2])
plt.plot(VOTsteps, Lbimodal)

Rbimodal = bimodal(VOTsteps,RBparam[0],RBparam[1],RBparam[2],RPparam[0],RPparam[1],RPparam[2])
plt.plot(VOTsteps, Rbimodal)

# %%  example: get P(VOT), P(VOT|/b/), P(VOT|/p/) for VOT at step 3

#x is VOT step
x = 3
#left shifted categories 
#P(VOT)
prbVOTL = Lbimodal[x]/numstim
# P(VOT|b)
prbVOTLB = gauss(x,LBparam[0],LBparam[1],LBparam[2])/numstim


#Right shifted categories
#P(VOT)
prbVOTR = Rbimodal[x]/numstim
# P(VOT|b)
prbVOTRB = gauss(x,RBparam[0],RBparam[1],RBparam[2])/numstim
# P(VOT|p)
prbVOTRP = gauss(x,RPparam[0],RPparam[1],RPparam[2])/numstim

# %% 2. Bayes law: P(B|A) = (P(A|B) * P(B) ) / P(A)

# P(b|VOT) = (P(VOT|b)*P(b))/P(VOT)
prbBVOT = prbVOTLB*.5/prbVOTL

# get ratio of P("ba"|VOT) / P("pa"|VOT) for all steps

#prop p response across VOT would look like...
### P(p|VOT) = P(VOT|p)*P(p)/P(VOT)
# prbPVOT = prbVOTLP*.5/prbVOTL

# P(VOT|p)
prbVOTLP = gauss(x,LPparam[0],LPparam[1],LPparam[2])/numstim
Lspeechcurve = np.zeros((len(VOTsteps)))
for i in range(len(VOTsteps)):
    prbVOTL = Lbimodal[i]/numstim
    prbVOTLP = gauss(i,LPparam[0],LPparam[1],LPparam[2])/numstim
    ### P(p|VOT) = P(VOT|p)*P(p)/P(VOT)
    prbPVOT = prbVOTLP*.5/prbVOTL
    #currcdf = NormalDist(mu=LPmu, sigma=LPsigma).cdf(i)
    Lspeechcurve[i] = prbPVOT #currcdf
plt.plot(VOTsteps, Lspeechcurve)

Lspeechcurve = np.zeros((len(VOTsteps)))
for i in range(len(VOTsteps)):
    currcdf = NormalDist(mu=LPmu, sigma=LPsigma).cdf(i)
    Lspeechcurve[i] = currcdf
plt.plot(VOTsteps, Lspeechcurve)

Rspeechcurve = np.zeros((len(VOTsteps)))
for i in range(len(VOTsteps)):
    currcdf = NormalDist(mu=RPmu, sigma=RPsigma).cdf(i)
    Rspeechcurve[i] = currcdf
plt.plot(VOTsteps, Rspeechcurve)

# %% figure out how the heck to use different priors 
Lspeechcurve2 = np.zeros([4,(len(VOTsteps))])
priors = np.array([.25,.5,.75, 1])
for pr in range(len(priors)):
    for i in range(len(VOTsteps)):
        prbVOTL = Lbimodal[i]/numstim
        prbVOTLP = gauss(i,LPparam[0],LPparam[1],LPparam[2])/numstim
        ### P(p|VOT) = P(VOT|p)*P(p)/P(VOT)
        prbPVOT = prbVOTLP*priors[pr]/prbVOTL
        Lspeechcurve2[pr,i] = prbPVOT
plt.plot(VOTsteps, Lspeechcurve2[0,])
plt.plot(VOTsteps, Lspeechcurve2[1,])
plt.plot(VOTsteps, Lspeechcurve2[2,])
plt.plot(VOTsteps, Lspeechcurve2[3,])


# %%
Rspeechcurve2 = np.zeros([4,(len(VOTsteps))])
priors = np.array([.25,.5,.75, 1])
for pr in range(len(priors)):
    for i in range(len(VOTsteps)):
        prbVOTR = Rbimodal[i]/numstim
        prbVOTRP = gauss(i,RPparam[0],RPparam[1],RPparam[2])/numstim
        ### P(p|VOT) = P(VOT|p)*P(p)/P(VOT)
        prbPVOT = prbVOTRP*priors[pr]/prbVOTR
        Rspeechcurve2[pr,i] = prbPVOT

plt.plot(VOTsteps, Rspeechcurve2[0,])
plt.plot(VOTsteps, Rspeechcurve2[1,])
plt.plot(VOTsteps, Rspeechcurve2[2,])
plt.plot(VOTsteps, Rspeechcurve2[3,])

# %% 
plt.plot(VOTsteps, Lspeechcurve2[3,])
plt.plot(VOTsteps, Rspeechcurve2[3,])

# %%
