
ALL: chladic_pasek_1.png chladic_pasek_2.png chladic_pasek_integrace_zdroju.png

chladic_pasek_1.png chladic_pasek_2.png &: chladic_pasek.m populate_function.m running_integral.m
	octave chladic_pasek.m

chladic_pasek_integrace_zdroju.png : chladic_pasek_integrace_zdroju.m populate_function.m running_integral.m
	octave chladic_pasek_integrace_zdroju.m
	
