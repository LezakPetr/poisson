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

