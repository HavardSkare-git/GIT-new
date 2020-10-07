set N; 							# total cities
set OC;							# potential operation centres
set V; 							# vehicles
set T;  						# set of timeperiods

param time{N,N} ; 				# travel time between cities
param st{OC} ;					# setup times
param opcity{N} ;				# potential operation centres 
param l:= 4;					# number of cities

var x{N,N,V} binary; 			# 1 if vehicle v travels directly between city i and j
var a{N, V, T} binary ; 		# 1 if city is visited by vehicle v in time t
var z{N} binary; 				# 1 if potential operation centre is selected
var e{N,V} binary;				# 1 if actual operation centre is selected

minimize distribution_time: 	# minimize distribution and setup time
	sum{i in N, j in N, v in V: i <> j} x[i,j,v]*time[i,j]	
	+ sum{i in N, v in V, oc in OC: i = oc} e[i,v]*st[oc];
	
subject to
# every city must be exited
cityexit{j in N, v in V}:		
	sum{i in N: i <> j} x[i,j,v] = sum{t in T} a[j,v,t] ;
# every city must be entered
cityentry{i in N, v in V}:		
	sum{j in N: i <> j} x[i,j,v] = sum{t in T} a[i,v,t] ;
# removing subtours. A route can at most be returned once	
subtour {v in V, i in N, j in N}:
	x[i,j,v] + x[j,i,v] <= 1;
	
# every vehicle visits four city	
vehicle_limit  {v in V}: 		
	sum{i in N,t in T} a[i,v,t] = l;
# every city is visited once by one vehicle
city_limit {i in N}: 			
	sum{v in V, t in T} a[i,v,t] = 1;
# first city must be an operation centre
first_city{i in N, v in V}:		
	a[i,v,0] = e[i,v];
# chosing operation centre among potentials	
oc_chosen {i in N}: 			
	z[i]*opcity[i] = sum{v in V} e[i,v] ;
# only one operation centre per vehicle	
oc_in_route{v in V}:			
	sum{i in N} e[i,v] = 1;
	
