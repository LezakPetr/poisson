
%%%%% Zadani %%%%%

% Rozmery pasku [m]
a = 8e-2;
b = 1.5e-2;
c = 3e-3;

% Merna tepelna vodivost [W / (K * m)]
lambda = 237;

% Ztratovy vykon tranzistoru [W]
P = 10;

% Merny ztratovy vykon tranzistoru [W/m]
pt = P / 1e-2;

% Teploty koncu tranzistoru
T1 = 20;
T2 = 20;

% Merne ztratovy vykony podle x
ptx = [
  0e-2, 0;
  2e-2, 0;
  2e-2, pt;
  3e-2, pt;
  3e-2, 0;
  5e-2, 0;
  5e-2, pt;
  6e-2, pt;
  6e-2, 0;
  8e-2, 0
];

%%%%% Vypocet %%%%%

% Funkci ptx interpolujeme po 1mm. Sice dokazeme po castech konstantni funkce ptx
% dvakrat integrovat presne, takze pro vypocet to neni nutne, ale chceme mit podrobne
% vyykresleny graf.
ptx = populate_function(ptx, 1e-3);

% Obsah prurezu pasku [m^2]
S = b * c;

% Pocet bodu funkce ptx
n = rows(ptx);

% Funkce zdroju
f = zeros(n, 2);
f(:, 1) = ptx(:, 1);
f(:, 2) = -ptx(:, 2) / (lambda * S);

% Provedeme dvoji integraci
count = ceil(a / 1e-3) + 1;
phi = zeros(count, 2);

for i = 1:count
	x = a * (i - 1) / (count - 1);
	
	sum = 0;
	
	for j = 2:n
		x1 = f(j - 1, 1);
		x2 = f(j, 1);
		y1 = f(j - 1, 2);
		y2 = f(j, 2);
		
		if (x <= x1)
			sum -= integrate_segment(x1, x2, y1, y2, x);
		elseif (x >= x2)
			sum += integrate_segment(x1, x2, y1, y2, x);
		else
			yx = y1 + (y2 - y1) * (x - x1) / (x2 - x1);
			
			sum -= integrate_segment(x1, x, y1, yx, x);
			sum += integrate_segment(x, x2, yx, y2, x);
		endif
	end
	
	phi(i, 1) = x;
	phi(i, 2) = 0.5 * sum;
end

% Reseni Laplaceovy rovnice
temp = zeros(count, 2);

H1 = T1 - phi(1, 2);
H2 = T2 - phi(count, 2);

for i = 1:count
	x = phi(i, 1);
	t = x / phi(count, 1);
	
	temp(i, 1) = x;
	temp(i, 2) = phi(i, 2) + H1 * (1 - t) + H2 * t;
end

plot(phi(:, 1), phi(:, 2));
print("chladic_pasek_integrace_zdroju_1.png");

plot(temp(:, 1), temp(:, 2));
print("chladic_pasek_integrace_zdroju_2.png");

