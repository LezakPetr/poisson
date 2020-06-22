
function res = integrate_segment(x1, x2, y1, y2, x)
	res = (x2 - x1) * (0.5 * (x - x1) * (y1 + y2) - (x2 - x1) * (y1 / 6 + y2 / 3));
end

