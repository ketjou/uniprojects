from chimera import runCommand
frame = mdInfo['frame']
runCommand("findhbond linewidth 2 color yellow")
from chimera.misc import getPseudoBondGroup
hbonds = len(getPseudoBondGroup("hydrogen bonds").pseudoBonds)
runCommand("2dlabels change mylabel text '%d H-bonds'" % hbonds)
runCommand("2dlabels change framelabel text '%d Frame'" % frame)
