
pkg load statistics;

rehash();

cylindrical.p = @(x, y, z) [ sqrt(x^2 + y^2), atan2(y, x), z ]; 
cylindrical.pInv = @(r, alpha, z) [ r * cos(alpha), r * sin(alpha), z ];

cylindrical.dp = @(r, alpha, z) [
	cos(alpha), sin(alpha), 0;
	-sin(alpha) / r, cos(alpha) / r, 0;
	0, 0, 1
];

cylindrical.dpInv = @(r, alpha, z) [
	cos(alpha), -r * sin(alpha), 0;
	sin(alpha), r * cos(alpha), 0;
	0, 0, 1
];

cylindrical.gCov = @(r, alpha, z) [
	1, 0, 0;
	0, r^2, 0;
	0, 0, 1
];

cylindrical.gContra = @(r, alpha, z) [
	1, 0, 0;
	0, 1 / r^2, 0;
	0, 0, 1
]; 

spherical.p = @(x, y, z) [ sqrt(x^2 + y^2 + z^2), atan2(y, x), atan(z / (sqrt(x^2 + y^2))) ]; 
spherical.pInv = @(r, alpha, beta) [ r * cos(alpha) * cos(beta), r * sin(alpha) * cos(beta), r * sin(beta) ];

spherical.dp = @(r, alpha, beta) [
	cos(alpha) * cos(beta), sin(alpha) * cos(beta), sin(beta);
	-sin(alpha) / (r * cos(beta)), cos(alpha) / (r * cos(beta)), 0;
	-cos(alpha) * sin(beta) / r, -sin(alpha) * sin(beta) / r, cos(beta) / r
];

spherical.dpInv = @(r, alpha, beta) [
	cos(alpha) * cos(beta), -r * sin(alpha) * cos(beta), -r * cos(alpha) * sin(beta);
	sin(alpha) * cos(beta), r * cos(alpha) * cos(beta), -r * sin(alpha) * sin(beta);
	sin(beta), 0, r * cos(beta)
];

spherical.gCov = @(r, alpha, beta) [
	1, 0, 0;
	0, (r * cos(beta))^2, 0;
	0, 0, r^2
];

spherical.gContra = @(r, alpha, beta) [
	1, 0, 0;
	0, 1 / (r * cos(beta))^2, 0;
	0, 0, 1 / r^2
]; 

systems = {cylindrical, spherical };

eps = 1e-10;

for i = 1:1
	x = normrnd(0, 1);
	y = normrnd(0, 1);
	z = normrnd(0, 1);
		
	for s = systems
		sys = s{};

		# Inverze souradneho systemu
		curvedCoord = sys.p(x, y, z);
		cartesianCoord = sys.pInv(curvedCoord(1), curvedCoord(2), curvedCoord(3));
		
		assert([x, y, z], cartesianCoord, eps);

		# Provazanost prvnich derivaci souradneho systemu
		dp = sys.dp(curvedCoord(1), curvedCoord(2), curvedCoord(3));
		dpInv = sys.dpInv(curvedCoord(1), curvedCoord(2), curvedCoord(3));
		
		firstDerivatesProduct = dp * dpInv;

		assert(eye(3), firstDerivatesProduct, eps);
		
		# Metricke tenzory g
		assert(sys.gCov(curvedCoord(1), curvedCoord(2), curvedCoord(3)), dpInv' * dpInv, eps);
		assert(sys.gContra(curvedCoord(1), curvedCoord(2), curvedCoord(3)), dp * dp', eps);
	endfor
endfor
