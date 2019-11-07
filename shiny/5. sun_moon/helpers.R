sunpos <- function(date, lat, lon, TZ = 0){
  lat <- lat*(pi/180)	
  lon <- lon*(pi/180)	
  # The time scale in these formulae are counted in days. Hours, minutes, seconds are expressed as fractions of a day. Day 0.0 occurs at 2000 Jan 0.0 UT 
  #d2000 <- as.numeric(ISOdate(2000, 1, 1, 0, 0, 0)): 946684800
  #	stopifnot(is.Date(day), "supply day as a Date object")
  day <- (as.numeric(date) - 946684800)/(24*60*60)	# Time in days and fractions of days since 2000-1-1
  
  ecl <- (23.4393 - 3.563E-7 * day)*(pi/180)     # Obliquity of the ecliptic, i.e. the "tilt" of the Earth's axis of rotation. in radians
  M  <- (356.0470 + 0.9856002585 * day)*(pi/180) # Mean anomaly of sun
  e  <- (0.016709 - 1.151E-9 * day )*(pi/180)    # Eccentricity of sun
  E  <- M + e * sin(M) * (1.0 + e * cos(M))      # Eccentric anomaly E (in radians)
  w  <- (282.9404 + 4.70935E-5 * day) * (pi/180) # argument of perihelion
  xv <- cos(E) - e
  yv <- sqrt(1.0 - e^2) * sin(E)
  v  <- atan2(yv, xv)     # Sun's true anomaly
  r  <- sqrt(xv^2 + yv^2) # Sun's distance
  L  <- v + w             # sun's mean longitude
  
  xe <- r * cos(L)             #  equatorial, rectangular, geocentric coordinates
  ye <- r * sin(L) * cos(ecl)  #  equatorial, rectangular, geocentric coordinates
  ze <- r * sin(L) * sin(ecl)  #  equatorial, rectangular, geocentric coordinates
  
  RA   <- atan2(ye, xe)              # Sun's Right Ascension (RA)
  Decl <- atan2(ze, sqrt(xe^2+ye^2)) # Sun's Declination
  
  GMST0 <- ifelse(M+w>2*pi, M+w+pi, M+w-pi)/15            # Sidereal time at Grenwich. in hours
  LST   <- GMST0 + (day-floor(day) + TZ)*2*pi+(lon)/15    # Local sidereal time
  LHA   <- LST - RA                                       # Sun's local hour angle
  
  h = asin(sin(lat)*sin(Decl)+cos(lat)*cos(Decl)*cos(LHA)) # h = sun's elevation angle
  
  out <- data.frame(date = date, RA = RA*180/pi, Decl = Decl*180/pi, L = L, elevation = h*180/pi)
  return(out)
}


moonpos <- function(date, lat, lon, TZ = 0){
  lat <- lat*(pi/180)	
  lon <- lon*(pi/180)	
  # The time scale in these formulae are counted in days. Hours, minutes, seconds are expressed as fractions of a day. Day 0.0 occurs at 2000 Jan 0.0 UT 
  day <- (as.numeric(date) - 946684800)/(24*60*60)	# Time in days and fractions of days since 2000-1-1
  ecl <- (23.4393 - 3.563E-7 * day)*(pi/180)     # Obliquity of the ecliptic, i.e. the "tilt" of the Earth's axis of rotation. in radians
  
  # Orbital elements of MOON
  N <- 125.1228 - 0.0529538083 * day
  i <- 5.1454
  w <- (318.0634 + 0.1643573223 * day)*(pi/180) # argument of perihelion
  a <- 60.2666                                  # (Earth radii)
  e <- 0.054900*(pi/180)                        # Eccentricity of moon
  M <- 115.3654 + 13.0649929509 * day*(pi/180)  # Mean anomaly of moon
  
  E  <- M + e * sin(M) * (1.0 + e * cos(M))      # Eccentric anomaly E (in radians)
  xv <- a * (cos(E) - e)
  yv <- a * (sqrt(1.0 - e^2) * sin(E))
  v  <- atan2(yv, xv)     # Moon's true anomaly
  r  <- sqrt(xv^2 + yv^2) # Moon's distance
  L  <- v + w             # Moon's mean longitude
  
  xh <- r * (cos(N)*cos(L) - sin(N)*sin(L)*cos(i)) #  equatorial, rectangular, geocentric coordinates
  yh <- r * (sin(N)*cos(L) + cos(N)*sin(L)*cos(i)) #  equatorial, rectangular, geocentric coordinates
  zh <- r * sin(L) * sin(i)  #  equatorial, rectangular, geocentric coordinates
  
  RA   <- atan2(yh, xh)              # Moon's Right Ascension (RA)
  Decl <- atan2(zh, sqrt(xh^2+yh^2)) # Moon's Declination
  
  GMST0 <- ifelse(M+w>2*pi, M+w+pi, M+w-pi)/15            # Sidereal time at Grenwich. in hours
  LST   <- GMST0 + (day-floor(day) + TZ)*2*pi+(lon)/15    # Local sidereal time
  LHA <- LST - RA                                         # Sun's local hour angle
  
  h = asin(sin(lat)*sin(Decl)+cos(lat)*cos(Decl)*cos(LHA)) # Moon's elevation angle
  out <- data.frame(date = date, RA = RA*180/pi, Decl = Decl*180/pi, L = L, elevation = h*180/pi)
  return(out)
}

