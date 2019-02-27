# r.blip

R binding for Blip (Bayesian network Learning Improved Project)

### Install

You can simply use: 

'''
devtools::install_github(“mauro-idsia/r.blip”)
'''

Or you can download this git and then go with: 

'''
./publish.sh
'''

### Example 

Minimal example: learns a BN from 'child-5000.dat' dataset: 

''' 
library('foreign')
library('bnlearn')
library('r.blip')
dat <- read.table('https://raw.githubusercontent.com/mauro-idsia/blip/master/data/child-5000.dat', sep = ' ')
bn <- blip.learn(dat, time = 10)
'''

The resulting Bayesian network is in bnlearn format. 
