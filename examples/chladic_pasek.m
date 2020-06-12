
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
int1 = running_integral(f);
int2 = running_integral(int1);

% Vysledny vypocet teploty
coeff = int2(n, 2) / int2(n, 1);

temp = zeros(n, 2);
temp(:, 1) = int2(:, 1);

for i = 1:n
	temp(i, 2) = T1 + coeff * (T2 - T1 - temp(i, 1)) + int2(i, 2);
endfor

% Vykresleni grafu a vypis vysledku
plot(ptx(:, 1), ptx(:, 2));
print("chladic_pasek_1.png");

plot(temp(:, 1), temp(:, 2));
print("chladic_pasek_2.png");

max(temp(:, 2))

