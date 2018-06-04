	
	
	
	DtoR = pi/180
	RtoD = 1/DtoR
	
	aspectAngle = c(15,30,45,75) + 270
	zoneSlope = 15 
	
	###------------------------------------------------------------------------
		aspectcos = mean( cos(aspectAngle*DtoR)*sin(zoneSlope*DtoR) )
		aspectsin = mean( sin(aspectAngle*DtoR)*sin(zoneSlope*DtoR) )
	zoneAspect = atan(aspectsin/aspectcos)* RtoD ## always between -90 and 90
		zoneAspect[aspectcos<0] = zoneAspect[aspectcos<0] + 180 #yes
		zoneAspect[aspectsin<0 & aspectcos>0] = zoneAspect[aspectsin<0 & aspectcos>0] + 360
		zoneAspect # 40.99643, 130.9964, 220.9964, 310.9964
		
	###------------------------------------------------------------------------
		aspectcos = mean( cos(aspectAngle*DtoR)*sin(zoneSlope*DtoR) )
		aspectsin = mean( sin(aspectAngle*DtoR)*sin(zoneSlope*DtoR) )
	zoneAspect = atan(aspectsin/aspectcos)* RtoD ## always between -90 and 90
		zoneAspect[aspectcos<0] = zoneAspect[aspectcos<0] + 180 #yes
		zoneAspect[aspectsin<0 & aspectcos>0] = zoneAspect[aspectsin<0 & aspectcos>0] + 360
		
		### ----- https://github.com/RHESSys/RHESSys/blob/master/g2w/grass2world/main.c#L535 
		zoneAspect = 360 - zoneAspect
		cond = zoneAspect<=270
		zoneAspect[cond] = zoneAspect[cond] + 90
		zoneAspect[!cond] = zoneAspect[!cond] - 270
		
		zoneAspect # 49.00357, 319.0036, 229.0036, 139.0036
		
		