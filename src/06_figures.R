figures<- function(n)
	{
	if(n==99){	
matplot(N_N_SUM,type='l')
matplot(N_H_SUM,type='l')
	
}
	if(n==1)
		{
		plot(total~year,total_pop,type='n')
		for(j in 1:max(total_pop$scenario))
			{
			for(i in 1:max(total_pop$r))
				{
				points(total~year, total_pop,
					subset=r==i & scenario==j,
					type='l',col=i)
				}
			}
		}
	if(n==2)
		{
		plot(n~year,total_pop,type='n')
		for(j in 1:max(total_pop$scenario))
			{
			for(i in 1:max(total_pop$r))
				{
				points(n~year, total_pop,
					subset=r==i & scenario==j,
					type='l',col=i)
				}
			}
		}
	if(n==3)
		{
		plot(h~year,total_pop,type='n')
		for(j in 1:max(total_pop$scenario))
			{
			for(i in 1:max(total_pop$r))
				{
				points(h~year, total_pop,
					subset=r==i & scenario==j,
					type='l',col=i)
				}
			}
		}
	if(n==10){}
	if(n==11){}
	if(n==12){}
	if(n==13){}
	if(n==14){}
	}