
pkg load statistics;

cylindrical.p = @(x, y, z) [ sqrt(x^2 + y^2), atan2(y, x), z ]; 
cylindrical.pInv = @(r, alpha, z) [ r * cos(alpha), r * sin(alpha), z ];

spherical.p = @(x, y, z) [ sqrt(x^2 + y^2 + z^2), atan2(y, x), atan(z / (sqrt(x^2 + y^2))) ]; 
spherical.pInv = @(r, alpha, beta) [ r * cos(alpha) * cos(beta), r * sin(alpha) * cos(beta), r * sin(beta) ];

systems = { cylindrical, spherical };
eps = 1e-9;

# Inverze
for s = systems
	sys = s{};

	for i = 1:100
		x = normrnd(0, 1);
		y = normrnd(0, 1);
		z = normrnd(0, 1);
		
		curved = sys.p(x, y, z);
		cartesian = sys.pInv(curved(1), curved(2), curved(3));
		
		assert(cartesian(1), x, eps);
		assert(cartesian(2), y, eps);
		assert(cartesian(3), z, eps);
	endfor
endfor
