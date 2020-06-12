
% Funkce vrati prubezny urcity integral funkce "f".
% Funkce "f" je zadana ve forme matice [x1, y1; x2, y2; ...; xn, yn], pricemz xi <= x(i+1). Lze mit xi = x(i+1) a ve funkci tak udelat skok.
% Vraceny urcity integral bude vypocitan ve stejnych bodech xi jako ma funkce "f" a bude vracen ve stejnem formatu.
% Hodnota res(i, 2) tedy bude hodnota integralu v bode xi a bude nabyvat urciteho integralu funkce "f" z bodu x1 do bodu xi.
function res = running_integral(f)
  n = rows(f);
  res = zeros(n, 2);
  res(:, 1) = f(:, 1);

  for i = 2:n
    res(i, 2) = res(i-1, 2) + 0.5 * (f(i, 1) - f(i-1, 1)) * (f(i-1, 2) + f(i, 2));
  endfor
endfunction

