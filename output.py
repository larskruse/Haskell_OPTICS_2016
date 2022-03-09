# output.py
# Autor: Lars Kruse
# 
# 2- oder 3-d Plot von Clustern

# output: Plot in Datei "cluster.jpg"


import numpy as np
import matplotlib.pyplot as plt
import ast
from mpl_toolkits.mplot3d import Axes3D


# einlesen der Daten aus output.txt. Daten muessen im Formal [[[x-Koordinaten],[y-Koordinaten],[z-Koordinaten]]] sein, wobei die z-Koordinaten optional sind. 
# es darf keine leere Menge in output.txt geben
data = open ("output.txt" , "r")
filedaten = data.read() 
data.close()
datenListe = ast.literal_eval(filedaten)



# im zwei-dimensionalen Fall:
if len (datenListe [0]) == 2: 
	fig = plt.figure()

	ax = fig.add_subplot(111)

	for cluster in datenListe[0:-1]:
		ax.plot (cluster[0],cluster[1], '+')
	ax.plot(datenListe[-1][0], datenListe[-1][1], '*')
	plt.savefig("cluster.jpg")
	


# im drei-dimensionalen Fall:
elif len (datenListe [0]) == 3:
	fig = plt.figure()

	ax = fig.gca(projection='3d')

	for cluster in datenListe[0:-1]:
		ax.plot(cluster[0], cluster[1], cluster[2], '+')
	ax.plot(datenListe[-1][0], datenListe[-1][1], datenListe[-1][2], '*')
	plt.savefig("cluster.jpg")
# falls Eingabe von Daten im falschen Format 
else:

	print "Fehler: Dimension der Daten falsch"




