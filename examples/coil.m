
R1 = 10e-3;
R2 = 15e-3;
L = 40e-3;
N = 1000;
mu = 4e-7 * pi;

steps = 10;
seq = 0:(steps - 1);

sum1 = 0;

for iRa = steps
	Ra = R1 + (0.5 + iRa) / steps * (R2 - R1);
	
	for iRb = steps
		Rb = R1 + (0.5 + iRb) / steps * (R2 - R1);
		
		for iLa = steps
			la = (0.5 + iLa) / steps * L;
			
			for iLb = steps
				lb = (0.5 + iLb) / steps * L;
				
				for iAlpha = steps
					alpha = (0.5 + iAlpha) / steps * L;
					
					sum1 += Ra * Rb / sqrt((lb - la)^2 + (Rb - Ra * cos(alpha))^2 + (Ra * sin(alpha))^2);
				endfor
			endfor
		endfor
	endfor
endfor

L1 = (N * mu) / (2 * (R2 - R1)) * sum1;

L1

