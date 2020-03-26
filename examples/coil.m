
R1 = 10e-3;
R2 = 15e-3;
S = 20e-3;
N = 1000;
mu = 4e-7 * pi;

steps = 10;
seq = 0:(steps - 1);

# Dlouha civka
Rs = (R1 + R2) / 2;
L1 = mu * pi * Rs^2 * N^2 / S;
L1

# Uplny vypocet integralu
diff2 = (S / steps)^2 * ((R2 - R1) / steps)^2 * (2 * pi / steps);
sum2 = 0;

for iRa = seq
	Ra = R1 + (0.5 + iRa) / steps * (R2 - R1);
	
	for iRb = seq
		Rb = R1 + (0.5 + iRb) / steps * (R2 - R1);
		
		for iLa = seq
			la = (0.5 + iLa) / steps * S;
			
			for iLb = seq
				lb = (0.5 + iLb) / steps * S;
				
				for iAlpha = seq
					alpha = (0.5 + iAlpha) / steps * 2 * pi;
					r = sqrt((lb - la)^2 + (Rb - Ra * cos(alpha))^2 + (Ra * sin(alpha))^2);
					sum2 += Ra * Rb / r;
				endfor
			endfor
		endfor
	endfor
endfor

L2 = (N^2 * mu * diff2) / (2 * (R2 - R1)^2 * S^2) * sum2;
L2

# Uplny vypocet integralu
diff3 = (S / steps) * ((R2 - R1) / steps)^2 * (2 * pi / steps);
sum3 = 0;

for iRa = seq
	Ra = R1 + (0.5 + iRa) / steps * (R2 - R1);
	
	for iRb = seq
		Rb = R1 + (0.5 + iRb) / steps * (R2 - R1);
		
		for it = seq
			t = (0.5 + it) / steps * S;
							
			for iAlpha = seq
				alpha = (0.5 + iAlpha) / steps * 2 * pi;
				r = sqrt(t^2 + (Rb - Ra * cos(alpha))^2 + (Ra * sin(alpha))^2);
				sum3 += Ra * Rb * (S - t) / r;
			endfor
		endfor
	endfor
endfor

L3 = (N^2 * mu * diff3) / ((R2 - R1)^2 * S^2) * sum3;
L3

