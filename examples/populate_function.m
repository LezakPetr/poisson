
% Funkce interpoluje zadanou funkci "func" tak, ze maximalni odstup dvou po sobe
% jdoucich bodu bude max_dx. Vsechny body funkce "func" budou i ve vysledku.
% Funkce i vysledek jsou ve forme matice [x1, y1; x2, y2; ...; xn, yn], pricemz xi <= x(i+1). Lze mit xi = x(i+1) a ve funkci tak udelat skok.
function res = populate_function(func, max_dx)
	res = func(1, :);
	
	for i = 2:rows(func)
		x1 = func(i-1, 1);
		x2 = func(i, 1);

		y1 = func(i-1, 2);
		y2 = func(i, 2);
		
		count = ceil ((x2 - x1) / max_dx);
		
		for j = 1:(count-1)
			x = x1 + (x2 - x1) * j / count;
			y = y1 + (y2 - y1) * j / count;
			
			res = [res; [x, y]];
		endfor
		
		res = [res; func(i, :)];
	endfor
end

