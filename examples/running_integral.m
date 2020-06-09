
function res = running_integral(f)
  n = rows(f);
  res = zeros(n, 2);
  res(:, 1) = f(:, 1);

  for i = 2:n
    res(i, 2) = res(i-1, 2) + 0.5 * (f(i, 1) - f(i-1, 1)) * (f(i-1, 2) + f(i, 2));
  endfor
endfunction

