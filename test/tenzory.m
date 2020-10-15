
pkg load statistics;

rehash();

eps = 1e-6;

function d = der(f, x)
	dx = 1e-7;
	
	d = (f(x + dx / 2) - f(x - dx / 2)) / dx;
endfunction


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


cylindrical.div = @(vr, valpha, vz, r, alpha, z) (
	der(@(t) vr(t, alpha, z), r) + der(@(t) valpha(r, t, z), alpha) / r^2 + der(@(t) vz(r, alpha, t), z) + vr(r, alpha, z) / r
);

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

spherical.div = @(vr, valpha, vbeta, r, alpha, beta) (
	der(@(t) vr(t, alpha, beta), r) + der(@(t) valpha(r, t, beta), alpha) / (r * cos(beta))^2 + der(@(t) vbeta(r, alpha, t), beta) / r^2 + 2 * vr(r, alpha, beta) / r - tan(beta) * vbeta(r, alpha, beta) / r^2
);

systems = {cylindrical, spherical };

v_a = @(a, b, c) a^3 * sin(2*b) * cos(3*c);
v_b = @(a, b, c) a^2 * sin(4*b) * cos(2*c);
v_c = @(a, b, c) a * sin(6*b) * cos(1*c);


for i = 1:1
	x = normrnd(0, 1);
	y = normrnd(0, 1);
	z = normrnd(0, 1);
		
	for s = systems
		sys = s{};

		# Inverze souradneho systemu
		curvedCoord = sys.p(x, y, z);
		
		c1 = curvedCoord(1);
		c2 = curvedCoord(2);
		c3 = curvedCoord(3);
		
		cartesianCoord = sys.pInv(c1, c2, c3);
		
		assert([x, y, z], cartesianCoord, eps);

		# Provazanost prvnich derivaci souradneho systemu
		dp = sys.dp(c1, c2, c3);
		dpInv = sys.dpInv(c1, c2, c3);
		
		firstDerivatesProduct = dp * dpInv;

		assert(eye(3), firstDerivatesProduct, eps);
		
		# Metricke tenzory g
		assert(sys.gCov(c1, c2, c3), dpInv' * dpInv, eps);
		assert(sys.gContra(c1, c2, c3), dp * dp', eps);
		
		# Divergence
		l1 = sys.div(v_a, v_b, v_c, c1, c2, c3);
		
		v_cart = @(px, py, pz) [
			v_a(sys.p(px, py, pz)(1), sys.p(px, py, pz)(2), sys.p(px, py, pz)(3));
			v_b(sys.p(px, py, pz)(1), sys.p(px, py, pz)(2), sys.p(px, py, pz)(3));
			v_c(sys.p(px, py, pz)(1), sys.p(px, py, pz)(2), sys.p(px, py, pz)(3))
		]' * sys.dp(sys.p(px, py, pz)(1), sys.p(px, py, pz)(2), sys.p(px, py, pz)(3));
		
		l2 = der(@(t) v_cart(t, y, z)(1), x) + der(@(t) v_cart(x, t, z)(2), y) + der(@(t) v_cart(x, y, t)(3), z);
		
		assert(l2, l1, eps);
	endfor
endfor
