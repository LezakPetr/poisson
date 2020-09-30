
pkg load statistics;

rehash();

cylindrical.p = @(x, y, z) [ sqrt(x^2 + y^2), atan2(y, x), z ]; 
cylindrical.pInv = @(r, alpha, z) [ r * cos(alpha), r * sin(alpha), z ];

cylindrical.dp = @(x, y, z) [
	x / sqrt(x^2 + y^2), y / sqrt(x^2 + y^2), 0;
	-y / (x^2 + y^2), x / (x^2 + y^2), 0;
	0, 0, 1
];

cylindrical.dpInv = @(r, alpha, z) [
	cos(alpha), -r * sin(alpha), 0;
	sin(alpha), r * cos(alpha), 0;
	0, 0, 1
];

spherical.p = @(x, y, z) [ sqrt(x^2 + y^2 + z^2), atan2(y, x), atan(z / (sqrt(x^2 + y^2))) ]; 
spherical.pInv = @(r, alpha, beta) [ r * cos(alpha) * cos(beta), r * sin(alpha) * cos(beta), r * sin(beta) ];

spherical.dp = @(x, y, z) [
	x / sqrt(x^2 + y^2 + z^2), y / sqrt(x^2 + y^2 + z^2), z / sqrt(x^2 + y^2 + z^2);
	-y / (x^2 + y^2), x / (x^2 + y^2), 0;
	-x*z / ((x^2 + y^2 + z^2) * sqrt(x^2 + y^2)), -y*z / ((x^2 + y^2 + z^2) * sqrt(x^2 + y^2)), sqrt(x^2 + y^2) / (x^2 + y^2 + z^2)
];

spherical.dpInv = @(r, alpha, beta) [
	cos(alpha) * cos(beta), -r * sin(alpha) * cos(beta), -r * cos(alpha) * sin(beta);
	sin(alpha) * cos(beta), r * cos(alpha) * cos(beta), -r * sin(alpha) * sin(beta);
	sin(beta), 0, r * cos(beta)
];

systems = {cylindrical,	spherical };

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
		firstDerivatesProduct = sys.dp(x, y, z) * sys.dpInv(curvedCoord(1), curvedCoord(2), curvedCoord(3));

		assert(eye(3), firstDerivatesProduct, eps);
	endfor
endfor
